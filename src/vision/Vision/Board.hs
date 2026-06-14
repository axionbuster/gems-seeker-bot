-- | Recover a 'Board' from one or more captured frames.
--
-- The parser keeps only the classification pipeline the bot actually needs:
--
--   1. 'estimateGrid' — locate the two yellow HUD controls in the top half, and from
--      their separation derive the grid pitch and origin (a fixed affine of that
--      separation; see the calibrated constants below).
--   2. 'findPlayfield' — combine the board's occupied wall and object fragments
--      to bound its extent. Cell coordinates may be negative; the same pitch
--      supports four possible half-cell lattice phases.
--   3. 'measureFrame' — per cell, ZNCC the gem\/bat luma and mask templates and
--      measure foreground\/yellow\/cyan pixel fractions.
--   4. 'classifyFrame' — threshold those into gem\/bat\/air, flood walls in from
--      the boundary, and take the heaviest leftover component as the player.
--   5. 'consensusMap' — majority-vote across frames.
--   6. 'validateParsedBoard' — reject maps that cannot represent a playable scene.
--
-- Calibration is frozen ('calibratedThresholds', measured from the bundled
-- calibration run); thresholds are not re-derived at runtime. The reporting,
-- rendering, and runtime-calibration code from the exploratory prototype is
-- omitted, as are the unused per-cell RGB matches.
--
-- Geometry and classification are pure; the only IO (loading templates and PNG
-- frames) lives in the caller.
module Vision.Board
  ( Thresholds (..)
  , calibratedThresholds
  , Templates (..)
  , prepareTemplates
  , parseBoard
  , validateParsedBoard
  , Grid (..)
  , GridRect (..)
  , CellBounds (..)
  , estimateGrid
  , findPlayfield
  , cellBounds
  ) where

import           Board               (Board (..), Cell (..))
import           Codec.Picture       (Image, Pixel8, PixelRGB8 (..),
                                      imageHeight, imageWidth, pixelAt)
import           Codec.Picture.Types (pixelMap)
import           Control.Monad       (forM)
import           Control.Monad.ST    (ST, runST)
import           Data.Array.ST       (STUArray, newArray, readArray, writeArray)
import           Data.Complex        (Complex (..), imagPart, realPart)
import           Data.List           (delete, group, sort)
import           Image.Frame         (resizeNearest)
import           Image.Zncc          (zncc)

-- | Per-cell classification thresholds, measured and frozen from the calibration
-- run. Constants now.
data Thresholds = Thresholds
  { gemLumaThreshold           :: {-# UNPACK #-} !Double
  , gemMaskThreshold           :: {-# UNPACK #-} !Double
  , gemYellowFraction          :: {-# UNPACK #-} !Double
  , batLumaThreshold           :: {-# UNPACK #-} !Double
  , batMaskThreshold           :: {-# UNPACK #-} !Double
  , batCyanFraction            :: {-# UNPACK #-} !Double
  , airForegroundFraction      :: {-# UNPACK #-} !Double
  , occupiedForegroundFraction :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

-- | The frozen calibration from the recorded calibration run.
calibratedThresholds :: Thresholds
calibratedThresholds =
  Thresholds
    { gemLumaThreshold = 0.818193412917773
    , gemMaskThreshold = 0.4897744778323243
    , gemYellowFraction = 5.0e-2
    , batLumaThreshold = 0.42380862933019015
    , batMaskThreshold = 0.3946005657078237
    , batCyanFraction = 0.1
    , airForegroundFraction = 7.719884503474203e-2
    , occupiedForegroundFraction = 0.19444444444444445
    }

-- | Gem and bat templates, pre-derived into the luma and foreground-mask forms
-- the matcher compares against (the raw RGB is unused by classification).
data Templates = Templates
  { gemLumaTemplate :: !(Image PixelRGB8)
  , gemMaskTemplate :: !(Image PixelRGB8)
  , batLumaTemplate :: !(Image PixelRGB8)
  , batMaskTemplate :: !(Image PixelRGB8)
  }

-- | Build the luma/mask template variants from the raw gem and bat images.
prepareTemplates :: Image PixelRGB8 -> Image PixelRGB8 -> Templates
prepareTemplates gemTemplate batTemplate =
  Templates
    { gemLumaTemplate = luminanceImage gemTemplate
    , gemMaskTemplate = foregroundMask 20 gemTemplate
    , batLumaTemplate = luminanceImage batTemplate
    , batMaskTemplate = foregroundMask 20 batTemplate
    }

-- | Parse a board from one or more RGB frames of the same scene. Uses each
-- frame's own grid estimate but a single playfield extent (the most common
-- across frames), then majority-votes the per-frame classifications.
parseBoard :: Templates -> [Image PixelRGB8] -> Either String Board
parseBoard _ [] = Left "Vision.Board.parseBoard: no frames"
parseBoard templates frames = do
  baseFrames <-
    traverse
      ( \image -> do
          grid <- estimateGrid image
          pure (image, grid)
      )
      frames
  let attempts = map (parseWithOffset baseFrames) phaseOffsets
  case [board | Right board <- attempts] of
    board : _ -> Right board
    [] ->
      case attempts of
        Left primaryError : _ -> Left primaryError
        _ -> Left "Vision.Board.parseBoard: no valid grid phase"
  where
    phaseOffsets =
      [ (0, 0)
      , (-0.5, 0)
      , (0, -0.5)
      , (-0.5, -0.5)
      ]
    parseWithOffset baseFrames offset = do
      perFrame <-
        traverse
          ( \(image, baseGrid) -> do
              let grid = offsetGrid offset baseGrid
              playfield <- findPlayfield grid image
              pure (image, grid, playfield)
          )
          baseFrames
      let playfields = [pf | (_, _, pf) <- perFrame]
      playfield <-
        maybe (Left "Vision.Board.parseBoard: no playfield detected") Right $
          mostCommon playfields
      let w = gridRectWidth playfield
          h = gridRectHeight playfield
          maps =
            [ classifyFrame
                calibratedThresholds
                w
                h
                (measureFrame (templatesForPitch (gridPitch grid) templates) grid playfield image)
            | (image, grid, _) <- perFrame
            ]
          cells = consensusMap maps
      validateParsedBoard (Board w h (concat cells)) >>= validateBoardEnvelope

offsetGrid :: (Double, Double) -> Grid -> Grid
offsetGrid (offsetX, offsetY) grid@Grid {gridPitch, gridOrigin = (originX, originY)} =
  grid
    { gridOrigin =
        ( originX + round (offsetX * gridPitch)
        , originY + round (offsetY * gridPitch)
        )
    }

templatesForPitch :: Double -> Templates -> Templates
templatesForPitch pitch templates =
  Templates
    { gemLumaTemplate = scaled (gemLumaTemplate templates)
    , gemMaskTemplate = scaled (gemMaskTemplate templates)
    , batLumaTemplate = scaled (batLumaTemplate templates)
    , batMaskTemplate = scaled (batMaskTemplate templates)
    }
  where
    calibratedPitch = 49.6
    scale = pitch / calibratedPitch
    scaled image =
      resizeNearest
        (round (fromIntegral (imageWidth image) * scale))
        (round (fromIntegral (imageHeight image) * scale))
        image

-- | Require the object counts needed to solve and replay a freshly captured
-- board. A malformed parse must stop before the solver can treat it as a win.
validateParsedBoard :: Board -> Either String Board
validateParsedBoard board@Board {boardCells}
  | playerCount /= 1 =
      Left
        ( "Vision.Board.parseBoard: expected exactly one player, found "
            ++ show playerCount
        )
  | gemCount == 0 =
      Left "Vision.Board.parseBoard: expected at least one gem"
  | otherwise = Right board
  where
    playerCount = length (filter (== Player) boardCells)
    gemCount = length (filter (== Gem) boardCells)

validateBoardEnvelope :: Board -> Either String Board
validateBoardEnvelope board@Board {boardW, boardH, boardCells}
  | boardW < 4 || boardH < 4 =
      Left "Vision.Board.parseBoard: playfield envelope is too small"
  | any isMovable boundaryCells =
      Left "Vision.Board.parseBoard: movable object reached the playfield boundary"
  | wallCount * 5 < length boundaryCells * 2 =
      Left "Vision.Board.parseBoard: playfield boundary has too little wall evidence"
  | otherwise = Right board
  where
    cellAt x y = boardCells !! (y * boardW + x)
    boundaryCells =
      [ cellAt x y
      | y <- [0 .. boardH - 1]
      , x <- [0 .. boardW - 1]
      , x == 0 || y == 0 || x == boardW - 1 || y == boardH - 1
      ]
    wallCount = length (filter (== Wall) boundaryCells)
    isMovable cell = cell == Gem || cell == Bat || cell == Player

-- 1. Grid and playfield geometry --------------------------------------------

data Grid = Grid
  { gridPitch  :: !Double
  , gridOrigin :: !(Int, Int)
  }
  deriving (Eq, Show)

data GridRect = GridRect
  { minCellX :: !Int
  , minCellY :: !Int
  , maxCellX :: !Int
  , maxCellY :: !Int
  }
  deriving (Eq, Ord, Show)

gridRectWidth :: GridRect -> Int
gridRectWidth GridRect {minCellX, maxCellX} = maxCellX - minCellX + 1

gridRectHeight :: GridRect -> Int
gridRectHeight GridRect {minCellY, maxCellY} = maxCellY - minCellY + 1

-- | Find the two yellow HUD-control clusters in the top half of the frame and turn
-- their horizontal separation into a grid pitch and origin.
estimateGrid :: Image PixelRGB8 -> Either String Grid
estimateGrid image =
  case widestPair of
    Nothing -> Left "estimateGrid: grid clusters are missing"
    Just (firstCenter, secondCenter)
      | distance <= 0 -> Left "estimateGrid: grid clusters are in invalid order"
      | otherwise -> Right (Grid {gridPitch = pitch, gridOrigin = origin})
      where
        distance = realPart secondCenter - realPart firstCenter
        pitch = 0.08778 * distance
        originFloat = firstCenter + (0.14743 :+ 0.60539) * (distance :+ 0)
        origin = (round (realPart originFloat), round (imagPart originFloat))
  where
    width = imageWidth image
    height = imageHeight image
    topHeight = height `div` 2
    yellowish = isBrightYellow

    clusters :: [[(Int, Int)]]
    clusters = runST $ do
      visited <-
        newArray ((0, 0), (max 0 (width - 1), max 0 (topHeight - 1))) False
          :: ST s (STUArray s (Int, Int) Bool)
      let neighbors (x, y) =
            [ (x', y')
            | x' <- [x - 1 .. x + 1]
            , y' <- [y - 1 .. y + 1]
            , x' >= 0
            , x' < width
            , y' >= 0
            , y' < topHeight
            , (x', y') /= (x, y)
            ]
          dfs start = go [] [start]
            where
              go acc [] = pure acc
              go acc (p@(x, y) : pending) = do
                seen <- readArray visited (x, y)
                if seen || not (yellowish (pixelAt image x y))
                  then go acc pending
                  else do
                    writeArray visited (x, y) True
                    go (p : acc) (neighbors p ++ pending)
      raw <-
        forM [0 .. topHeight - 1] $ \y ->
          forM [0 .. width - 1] $ \x -> do
            seen <- readArray visited (x, y)
            if not seen && yellowish (pixelAt image x y)
              then dfs (x, y)
              else pure []
      pure (filter ((> 25) . length) (concat raw))

    boundingBox cluster =
      let (xs, ys) = unzip cluster
       in (minimum xs, minimum ys, maximum xs, maximum ys)
    centerOf (x0, y0, x1, y1) =
      (fromIntegral x0 + fromIntegral x1) / 2 :+ (fromIntegral y0 + fromIntegral y1) / 2
    compact (x0, y0, x1, y1) = x1 - x0 <= 100 && y1 - y0 <= 100
    boxes = filter compact (map boundingBox clusters)
    candidatePairs =
      [ if realPart a <= realPart b then (a, b) else (b, a)
      | firstBox <- boxes
      , secondBox <- boxes
      , firstBox /= secondBox
      , let a = centerOf firstBox
      , let b = centerOf secondBox
      , abs (imagPart a - imagPart b) <= 50
      ]
    widestPair = maximumBy separation candidatePairs
    separation (left, right) = realPart right - realPart left

-- | Bound the playfield from occupied cell fragments. Some themes draw gaps in
-- the perimeter, so wall runs and isolated sprites contribute to one envelope.
-- Tall one-cell components at the right edge belong to the mirrored window.
findPlayfield :: Grid -> Image PixelRGB8 -> Either String GridRect
findPlayfield grid image =
  case boardCells of
    [] -> Left "findPlayfield: no occupied playfield cells found"
    _ ->
      let xs = map fst boardCells
          ys = map snd boardCells
          rect =
            GridRect
              { minCellX = minimum xs
              , minCellY = minimum ys
              , maxCellX = maximum xs
              , maxCellY = maximum ys
              }
       in if gridRectWidth rect >= 4 && gridRectHeight rect >= 4
            then Right rect
            else Left "findPlayfield: occupied envelope is too small"
  where
    bounds = cellBounds (gridPitch grid) (gridOrigin grid)
    candidateCells =
      [ (cellX, cellY)
      | cellY <- [-1 .. 12]
      , cellX <- [-1 .. 10]
      , boundsInside image (bounds (cellX, cellY))
      ]
    occupiedCells =
      [ point
      | point <- candidateCells
      , cellForegroundFraction image (bounds point) > 0.08
      ]
    boardCells = concat (filter boardFragment (connectedComponents occupiedCells))
    boardFragment component =
      let xs = map fst component
          ys = map snd component
          width = maximum xs - minimum xs + 1
          height = maximum ys - minimum ys + 1
       in length component >= 2
            && not (width == 1 && height >= 8)

boundsInside :: Image pixel -> CellBounds -> Bool
boundsInside image CellBounds {cellLeft, cellTop, cellWidth, cellHeight} =
  cellLeft >= 0
    && cellTop >= 0
    && cellLeft + cellWidth <= imageWidth image
    && cellTop + cellHeight <= imageHeight image

-- 2. Cell geometry and measurements -----------------------------------------

data CellBounds = CellBounds
  { cellLeft   :: !Int
  , cellTop    :: !Int
  , cellWidth  :: !Int
  , cellHeight :: !Int
  }
  deriving (Eq, Show)

cellBounds :: Double -> (Int, Int) -> (Int, Int) -> CellBounds
cellBounds pitch (originX, originY) (cellX, cellY) =
  CellBounds left top (right - left) (bottom - top)
  where
    line origin cell = round (fromIntegral origin + fromIntegral cell * pitch)
    left = line originX cellX
    top = line originY cellY
    right = line originX (cellX + 1)
    bottom = line originY (cellY + 1)

-- | The per-cell evidence classification needs: four ZNCC scores and three
-- pixel-fraction summaries.
data CellMeasurement = CellMeasurement
  { gemLumaScore       :: !Double
  , gemMaskScore       :: !Double
  , batLumaScore       :: !Double
  , batMaskScore       :: !Double
  , foregroundFraction :: !Double
  , yellowFraction     :: !Double
  , cyanFraction       :: !Double
  }
  deriving (Eq, Show)

measureFrame :: Templates -> Grid -> GridRect -> Image PixelRGB8 -> [[CellMeasurement]]
measureFrame templates grid playfield image =
  [ [ measureCell templates image lumaImage maskImage (bounds (cellX, cellY))
    | cellX <- [minCellX playfield .. maxCellX playfield]
    ]
  | cellY <- [minCellY playfield .. maxCellY playfield]
  ]
  where
    bounds = cellBounds (gridPitch grid) (gridOrigin grid)
    lumaImage = luminanceImage image
    maskImage = foregroundMask 20 image

measureCell
  :: Templates
  -> Image PixelRGB8
  -> Image PixelRGB8
  -> Image PixelRGB8
  -> CellBounds
  -> CellMeasurement
measureCell templates image lumaImage maskImage bounds =
  CellMeasurement
    { gemLumaScore = cellZnccScore (gemLumaTemplate templates) lumaImage bounds radius
    , gemMaskScore = cellZnccScore (gemMaskTemplate templates) maskImage bounds radius
    , batLumaScore = cellZnccScore (batLumaTemplate templates) lumaImage bounds radius
    , batMaskScore = cellZnccScore (batMaskTemplate templates) maskImage bounds radius
    , foregroundFraction = cellForegroundFraction image bounds
    , yellowFraction = cellColorFraction isBrightYellow image bounds
    , cyanFraction = cellColorFraction isBrightCyan image bounds
    }
  where
    radius = 2

-- | Best ZNCC of @template@ against @source@ over offsets within @radius@ of the
-- cell-centred position, considering only windows that fit inside the cell.
cellZnccScore :: Image PixelRGB8 -> Image PixelRGB8 -> CellBounds -> Int -> Double
cellZnccScore template source bounds radius =
  maximum
    [ if windowFits x y then zncc template source (x, y) else -1
    | offsetY <- [-radius .. radius]
    , offsetX <- [-radius .. radius]
    , let x = centeredX + offsetX
    , let y = centeredY + offsetY
    ]
  where
    centeredX = cellLeft bounds + (cellWidth bounds - imageWidth template) `div` 2
    centeredY = cellTop bounds + (cellHeight bounds - imageHeight template) `div` 2
    windowFits x y =
      x >= cellLeft bounds
        && y >= cellTop bounds
        && x + imageWidth template <= cellLeft bounds + cellWidth bounds
        && y + imageHeight template <= cellTop bounds + cellHeight bounds

cellForegroundFraction :: Image PixelRGB8 -> CellBounds -> Double
cellForegroundFraction image bounds =
  fractionOfCell image bounds (\(PixelRGB8 r g b) -> maximum [r, g, b] > 20)

cellColorFraction :: (PixelRGB8 -> Bool) -> Image PixelRGB8 -> CellBounds -> Double
cellColorFraction predicate image bounds = fractionOfCell image bounds predicate

-- | Fraction of the (4px-inset) interior pixels of a cell that satisfy a
-- predicate. Returns 0 for a degenerate cell.
fractionOfCell :: Image PixelRGB8 -> CellBounds -> (PixelRGB8 -> Bool) -> Double
fractionOfCell image CellBounds {cellLeft, cellTop, cellWidth, cellHeight} predicate
  | sampleCount <= 0 = 0
  | otherwise = fromIntegral matchingCount / fromIntegral sampleCount
  where
    margin = 4
    pixels =
      [ pixelAt image x y
      | y <- [cellTop + margin .. cellTop + cellHeight - margin - 1]
      , x <- [cellLeft + margin .. cellLeft + cellWidth - margin - 1]
      ]
    sampleCount = length pixels
    matchingCount = length (filter predicate pixels)

isBrightYellow :: PixelRGB8 -> Bool
isBrightYellow (PixelRGB8 red green blue) = red >= 160 && green >= 160 && blue <= 120

isBrightCyan :: PixelRGB8 -> Bool
isBrightCyan (PixelRGB8 red green blue) =
  green >= 180
    && blue >= 150
    && fromIntegral green >= fromIntegral red + (70 :: Int)
    && fromIntegral blue >= fromIntegral red + (50 :: Int)

luminanceImage :: Image PixelRGB8 -> Image PixelRGB8
luminanceImage = pixelMap toLuminance
  where
    toLuminance (PixelRGB8 red green blue) =
      let value :: Pixel8
          value =
            round
              ( 0.2126 * fromIntegral @Pixel8 @Double red
                  + 0.7152 * fromIntegral green
                  + 0.0722 * fromIntegral blue
              )
       in PixelRGB8 value value value

foregroundMask :: Pixel8 -> Image PixelRGB8 -> Image PixelRGB8
foregroundMask threshold = pixelMap toMask
  where
    toMask (PixelRGB8 red green blue)
      | maximum [red, green, blue] > threshold = PixelRGB8 255 255 255
      | otherwise = PixelRGB8 0 0 0

-- 3. Frame-local classification ---------------------------------------------

data PreliminaryCell = Classified !Cell | Occupied
  deriving (Eq, Show)

classifyFrame :: Thresholds -> Int -> Int -> [[CellMeasurement]] -> [[Cell]]
classifyFrame thresholds width height measurements =
  [ [finalCell (x, y) | x <- [0 .. width - 1]]
  | y <- [0 .. height - 1]
  ]
  where
    preliminary =
      [ [classifyMeasurement thresholds (isBoundary x y) measurement | (x, measurement) <- zip [0 ..] row]
      | (y, row) <- zip [0 ..] measurements
      ]
    isBoundary x y = x == 0 || y == 0 || x == width - 1 || y == height - 1
    preliminaryAt (x, y) = (preliminary !! y) !! x
    measurementAt (x, y) = (measurements !! y) !! x
    boundaryOccupied =
      [ (x, y)
      | y <- [0 .. height - 1]
      , x <- [0 .. width - 1]
      , isBoundary x y
      , preliminaryAt (x, y) == Occupied
      ]
    wallCells = floodOccupied width height preliminaryAt boundaryOccupied
    explicitPlayerCells =
      [ (x, y)
      | y <- [0 .. height - 1]
      , x <- [0 .. width - 1]
      , preliminaryAt (x, y) == Classified Player
      ]
    disconnectedOccupied =
      [ (x, y)
      | y <- [0 .. height - 1]
      , x <- [0 .. width - 1]
      , preliminaryAt (x, y) == Occupied
      , (x, y) `notElem` wallCells
      ]
    playerCells =
      if null explicitPlayerCells
        then
          case maximumBy componentWeight (connectedComponents disconnectedOccupied) of
            Nothing        -> []
            Just component -> component
        else []
    componentWeight = sum . map (foregroundFraction . measurementAt)
    finalCell point =
      case preliminaryAt point of
        Classified cell -> cell
        Occupied
          | point `elem` wallCells -> Wall
          | not (null explicitPlayerCells) -> Wall
          | point `elem` playerCells -> Player
          | otherwise -> Air

classifyMeasurement :: Thresholds -> Bool -> CellMeasurement -> PreliminaryCell
classifyMeasurement thresholds boundaryCell measurement
  | isGem = Classified Gem
  | isPlayer = Classified Player
  | isBat = Classified Bat
  | foregroundFraction measurement <= airForegroundFraction thresholds = Classified Air
  | boundaryCell = Occupied
  | foregroundFraction measurement < occupiedForegroundFraction thresholds = Classified Air
  | otherwise = Occupied
  where
    isGem =
      ( yellowFraction measurement >= gemYellowFraction thresholds
          && gemLumaScore measurement >= gemLumaThreshold thresholds
      )
        || ( gemMaskScore measurement >= gemMaskThreshold thresholds
              && gemLumaScore measurement >= gemLumaThreshold thresholds * 0.5
           )
    isPlayer =
      yellowFraction measurement >= 0.15
        && gemLumaScore measurement < gemLumaThreshold thresholds * 0.5
    isBat =
      cyanFraction measurement >= batCyanFraction thresholds
        && ( batLumaScore measurement >= batLumaThreshold thresholds
              || batMaskScore measurement >= batMaskThreshold thresholds
           )

floodOccupied :: Int -> Int -> ((Int, Int) -> PreliminaryCell) -> [(Int, Int)] -> [(Int, Int)]
floodOccupied width height cellAt = go []
  where
    go visited [] = visited
    go visited (point : pending)
      | point `elem` visited = go visited pending
      | cellAt point /= Occupied = go visited pending
      | otherwise = go (point : visited) (neighbors point ++ pending)
    neighbors (x, y) =
      [ (x', y')
      | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      , x' >= 0
      , x' < width
      , y' >= 0
      , y' < height
      ]

-- 4. Multi-frame consensus ---------------------------------------------------

consensusMap :: [[[Cell]]] -> [[Cell]]
consensusMap [] = []
consensusMap maps@(firstMap : _) =
  [ [winningCell (votesAt x y) | x <- [0 .. width - 1]]
  | y <- [0 .. height - 1]
  ]
  where
    height = length firstMap
    width = case firstMap of
      []           -> 0
      firstRow : _ -> length firstRow
    votesAt x y = [(frameMap !! y) !! x | frameMap <- maps]

winningCell :: [Cell] -> Cell
winningCell [] = Air
winningCell votes = snd (foldl1 max (voteCounts votes))
  where
    voteCounts = map count . group . sort
    count g = (length g, headOr Air g)
    headOr d []      = d
    headOr _ (c : _) = c

-- generic helpers -----------------------------------------------------------

-- | The element maximising a key; 'Nothing' for an empty list. Ties keep the
-- earlier element from the left fold.
maximumBy :: Ord weight => (a -> weight) -> [a] -> Maybe a
maximumBy _ [] = Nothing
maximumBy weight (first : rest) = Just (foldl' choose first rest)
  where
    choose best candidate
      | weight candidate > weight best = candidate
      | otherwise = best

-- | The most frequent value; 'Nothing' for an empty list.
mostCommon :: Ord a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon values =
  Just (snd (maximum [(length g, v) | g@(v : _) <- group (sort values)]))

-- | 4-connected components of a set of grid points.
connectedComponents :: [(Int, Int)] -> [[(Int, Int)]]
connectedComponents [] = []
connectedComponents (start : remaining) = component : connectedComponents rest
  where
    (component, rest) = grow [] [start] remaining
    grow found [] available = (found, available)
    grow found (point : pending) available =
      let adjacent = [n | n <- fourNeighbors point, n `elem` available]
          available' = foldl' (flip delete) available adjacent
       in grow (point : found) (adjacent ++ pending) available'
    fourNeighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
