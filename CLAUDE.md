# CLAUDE.md

Working notes for Codex in this repository.

## What this repo is

`gems-seeker-bot` is a macOS solver for the Gem Seeker minigame from Tomb of the Mask+. It captures the game window, parses the board from a few frames, solves the level up front, and replays the result as swipe gestures.

## Required system dependencies

- macOS 15.2 or later
- Cabal
- GHC 9.14.x
- Accessibility permission for the terminal or runner that drives the bot
- Screen Recording permission for the terminal or runner that captures frames

## Build and run

- `cabal build all`
- `cabal test`
- `cabal run gems-seeker-bot`
- `cabal run gems-seeker-bot -- solve test/fixtures/cases/case0.txt`
- `cabal run gems-seeker-bot -- parse test/fixtures/frames/live-window.png`
- `cabal run gems-seeker-bot -- almost`
- `cabal run gems-seeker-bot -- --no-record run`
- `cabal run gems-seeker-bot -- --help`

## Project layout

| Component | Path | Role | Notes |
|---|---|---|---|
| `gsb-search` (lib) | `src/search/` | board state, physics, search | perf-critical (`-O2`) |
| `gsb-vision` (lib) | `src/vision/` | image ops and board parsing | perf-critical (`-O2`) |
| `gsb-mac` (lib) | `src/mac/` | window geometry, capture, recording, gestures | Darwin only |
| `gems-seeker-bot` (exe) | `app/` | command-line entry point | Darwin only |
| `gsb-test` (test) | `test/` | hspec suite | drives the fixtures |

## Working conventions

- Use 2-space indentation.
- Keep `StrictData` and add `{-# UNPACK #-}` on simple numeric fields.
- Prefer large source files when that improves navigation and shared context.
- Add comments where the intent is not obvious.
- Add a doc comment for every newly exported item.
- Prefer TDD for new behavior.
- Keep CV and search code straightforward and performance-aware.
- Keep first-party fixtures under `test/fixtures/` and images under `assets/`.

## Notes

- `app/Main.hs` exposes `solve`, `parse`, `capture`, `swipe`, `almost`, and
  `run` subcommands.
- Window discovery, application activation, raw RGB capture, movie recording,
  and pointer gestures use the Objective-C bridge in `src/mac/Mac/Native.m`.
- The repository's pre-commit hook is opt-in through `scripts/install-hooks.sh`.
  If `stylish-haskell` is unavailable, the hook warns and skips formatting
  rather than blocking the commit.
- `run`, `almost`, and `swipe` record timestamped H.264 movies under the
  Git-ignored `recordings/` directory unless `--no-record` is set.
- Recordings request up to 120 FPS and use a stable 2-pixels-per-point canvas
  across Retina and non-Retina displays.
- If you add new documentation, make it self-contained and keep the prose aligned with the current code.
