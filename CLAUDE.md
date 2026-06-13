# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

`gems-seeker-bot` is an automated solver for the **Gem Seeker** minigame (from *Tomb of the Mask+*) that runs on **macOS**. It combines two halves:

- the **algorithm and state representation** of `references/pure-solver` (the search "brain"), and
- the **macOS "eyes and hands"** of `references/experiments` (computer vision that reads the board, gestures that play it).

This is a **fresh project**, not a fork of either. It builds with **Cabal** on **GHC 9.14.\***, targets macOS only, and **does not depend on or link against either submodule**. The submodules under `references/` exist purely to read from — port and reimplement their ideas as first-party modules here, improving the style as you go. They do not need to be re-cloned after the initial clone unless you genuinely need to consult them.

After a fresh clone, `git submodule update --init --recursive` populates `references/`; skip it if you don't need the references.

## The pipeline (intended design)

1. **Grab frames programmatically** via `Mac.Mirror` (`screencapture`) — a frame or two is enough.
2. **Parse the board with CV** from those frames — realistically **1–3 good frames** recover the full board layout.
3. **Solve completely** with the search algorithm. The solver produces the entire optimal move sequence up front.
4. **Replay** the gravity moves as macOS swipe gestures.

The bot drives its own capture for the board parse. The user *separately* runs a screen recorder, but that is **only for footage/debugging** — the bot does not consume it. CV is needed only for the initial board parse; per-move CV sanity-checking is **optional, out of scope for the first iteration**.

## Game model & glyphs

Gravity puzzle: each move snaps gravity to Up/Down/Left/Right; all movable pieces slide until they hit a wall, boundary, or each other. A gem sliding onto the **player** slot is collected; a bat reaching it loses. Win when all gems are collected.

Glyph vocabulary (unified for this repo):

- `.` air · `@` gem · `%` bat · `#` wall/obstacle · `*` **player**

Note: `pure-solver` calls the player slot **target** — an archaic name (the slot gems "target" and slide into). Treat `target` in the reference as today's `player`.

## Project layout & build

Cabal, GHC 9.14.\*, default language **GHC2024**. The package is split into sub-libraries the way an equivalent Python project would split imports ("virtual dependencies"):

| Component | Path | Role | Notes |
|---|---|---|---|
| `gsb-search` (lib) | `src/search/` | board state, physics, search | perf-critical (`-O2`); `Board`, `Search.Dijkstra`, `Solve` |
| `gsb-vision` (lib) | `src/vision/` | image ops + board parsing | perf-critical (`-O2`); `Image` re-export surface, `Image.Zncc`, `Image.Frame`, `Vision.Board` |
| `gsb-mac` (lib) | `src/mac/` | window geometry, capture, gestures | **Darwin only**; `Mac.Mirror`, `Mac.Gesture` |
| `gems-seeker-bot` (exe) | `app/` | wires the pipeline | **Darwin only** |
| `gsb-test` (test) | `test/` | hspec suite | drives TDD |

Internal libraries are referenced as `gems-seeker-bot:gsb-search` in `build-depends`.

```bash
cabal build all            # all libs + exe (mac libs/exe skip off-Darwin)
cabal test                 # run the hspec suite
cabal run gems-seeker-bot  # the bot (macOS)
cabal repl gsb-search      # poke at the algorithm
```

Modules are currently typechecking stubs with `TODO`s pointing at the reference module to port from. The `_pipeline` in `app/Main.hs` shows the intended wiring (underscored so it's deliberately unused until live).

## Conventions

- **Indentation:** 2 spaces. Keep code concise; `pure-solver`'s logic is the model, but re-style it to 2-space indent.
- **Strictness:** use `StrictData`, **not** `Strict`. `Strict` can silently implode performance. Neither inserts unpacking — add explicit `{-# UNPACK #-}` on simple data fields (ints, etc.) so plain numbers aren't boxed behind pointers.
- **Where performance matters:** only the **computer-vision** and **board-search** modules. Everything else only needs to be correct. (The reference solver's board search is already excellent; the reference CV is only *acceptable*.)
- **Module organization — "virtual Python dependencies":** prefer large source files, but factor the project the way you'd structure imports in an equivalent Python project: each conceptual dependency gets its own Cabal sub-library or a re-exporting module. This is how `experiments` is organized (`iexp-image`, `iexp-video`, etc.); follow that, not `pure-solver`'s flatter older layout.
- **Testing:** `pure-solver` was written before good agentic tooling, without TDD. New code here should be test-driven.

## Porting notes

### From `references/pure-solver` (the algorithm)
- `src/TotM2.hs` — bit-packed board representation + gravity/collection/win-lose physics. The performance-critical core.
- `src/Dijk.hs` — generic Dijkstra / uniform-cost search.
- `src/SolveTotM2.hs` — wires the board into the search for the minimum-move solution.
- See `CHANGELOG.md` and `docs/INTERNING_PLAN.md` for the optimization history and the planned state-interning direction. Carry the performance lessons over; add the test coverage it never had.

### From `references/experiments` (the macOS eyes & hands)
The **macOS-specific parts matter most**; the universal parts were exploration. The source needs real cleanup as you port:
- `src/mac/Mac.Mirror` (Darwin-only) — window geometry (`osascript`), region capture (`screencapture`), gestures (`cliclick`). **The hands.**
- `src/image/Image.Zncc` (ZNCC template matching) and `Image.Frame` — the CV building blocks for board parsing.
- **Drop the recording feature** (`Video.Recorder`) — unneeded here (it was the one well-performing piece, but irrelevant).
- **Drop the calibration logic** — calibration is already done; the measured thresholds live in `calibration.txt` (gem/bat luma & mask thresholds, yellow/cyan fraction anchors). Bake them in as constants; don't re-derive them.
- MJPEG streaming (`Web.Mjpeg`) is optional / not core.
- Design docs in `docs/superpowers/{plans,specs}/` describe the grid-overlay and cell-map-detection work (pure-first: pure geometry/measurement/classification, IO only in `main`).

**Grid-origin caveat:** the saved grid origin is only valid for a particular grid size. Reusing it verbatim can push cells negative on larger boards. Handle origin/extent so any board fits — by design the entire Gem Seeker board always fits within the screen.

## Reference submodules — toolchains (for consulting only)

These build independently if you need to run them; this repo does not use either.

- `references/pure-solver` (`gemssearch002`) — **Stack** (lts-24.9): `stack build`, `stack exec gemssearch002-exe < case0.txt`, `stack test`.
- `references/experiments` (`iexp1`) — **Cabal** (GHC2024): `cabal build all`, `cabal run iexp1 -- 2`, `cabal run iexp3 -- --help` (macOS only).
