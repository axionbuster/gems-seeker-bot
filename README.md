# Gems Seeker Bot

A macOS bot for the Gem Seeker minigame from Tomb of the Mask+.

It captures the game window, reads the board from a few frames, solves the level, and replays the winning move sequence as swipe gestures.

![Gameplay capture](assets/readme/gameplay.png)

## What it does

- Locates and activates the mirrored phone window with native macOS APIs.
- Captures the window through a ScreenCaptureKit window filter directly as RGB
  pixels, with stable black pixels outside its rounded corners.
- Parses the board from 1 to 3 good frames.
- Solves the full board before it starts replaying moves.
- Replays gravity moves with native Core Graphics events.
- Yields and stops replay when it detects competing pointer input.
- Records `run`, `almost`, and `swipe` gameplay with application audio to
  timestamped H.264 movies.

## Requirements

- macOS 15.2 or later
- Cabal and GHC 9.14.x
- Accessibility permission for the terminal or runner
- Screen Recording permission for the terminal or runner

## Optional Formatting Hook

This repository ships an opt-in pre-commit hook for Haskell formatting.

- Run `./scripts/install-hooks.sh` in a clone to enable it locally.
- The hook uses `stylish-haskell` when it is available.
- If you do not have `stylish-haskell`, the hook prints a warning and lets the commit continue.
- Before opening a pull request, format any changed Haskell files with `stylish-haskell` or another equivalent formatter setup.

## Quick Start

```bash
cabal build all
cabal test
cabal run gems-seeker-bot
```

A few useful subcommands:

```bash
cabal run gems-seeker-bot -- solve test/fixtures/cases/case0.txt
cabal run gems-seeker-bot -- parse test/fixtures/frames/live-window.png
cabal run gems-seeker-bot -- capture
cabal run gems-seeker-bot -- swipe left
cabal run gems-seeker-bot -- almost
cabal run gems-seeker-bot -- --no-record run
cabal run gems-seeker-bot -- --help
```

## Gameplay Recording

The live `run`, `almost`, and `swipe` modes record by default. Movies are saved
under the Git-ignored `recordings/` directory with local timestamps and mode
names in their filenames. Pass `--no-record` to suppress recording.

Recording requests 120 FPS and clamps that request to the maximum refresh rate
of the display containing the game window. ScreenCaptureKit writes the frames
the display can supply, so the movie does not add duplicate frames to claim a
higher rate.

Movies use H.264 video and 48 kHz stereo application audio in a QuickTime
`.mov` container. The single-window ScreenCaptureKit filter includes audio from
the application that owns the captured window. Movie dimensions are fixed at 2
pixels per macOS point for the window size at recording start. A given window
size therefore produces the same editing canvas on Retina and non-Retina
displays; non-Retina input is upscaled to preserve that stable canvas.

## How It Works

1. The app locates, activates, and captures the iPhone Mirroring window.
2. Vision code classifies the board from a handful of frames.
3. Search code computes the full solution.
4. ScreenCaptureKit records live modes while the macOS gesture layer replays
   the moves.

## Repository Layout

- `app/` - command-line entry point and mode dispatch
- `src/search/` - board model, physics, and solver
- `src/vision/` - image helpers and board parsing
- `src/mac/` - window capture and gestures
- `assets/` - bundled templates and README imagery
- `test/fixtures/` - board and frame fixtures used by the test suite
- `test/` - hspec tests

## License

BSD-3-Clause. See [LICENSE](LICENSE).
