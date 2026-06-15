# Changelog

All notable changes to this project will be documented in this file.

## 0.3.0.0 - 2026-06-15

- Added an `almost` mode that replays every move before the final winning move.
- Recorded `run`, `almost`, and `swipe` gameplay to timestamped H.264 QuickTime
  movies by default, with `--no-record` available to suppress recording.
- Requested up to 120 FPS based on the game window's display refresh rate and
  standardized movie dimensions at 2 pixels per macOS point.
- Added explicit `--help` and `-h` command-line options.
- Simplified vision support code and tightened project formatting settings.

## 0.2.0.0 - 2026-06-14

- Added a native macOS Objective-C bridge that performs ScreenCaptureKit capture
  and Core Graphics gestures inside the executable.
- Reduced each swipe to a short linear event path with enough time for iPhone
  Mirroring and the game animation to process every move.
- Made gesture replay yield immediately when other pointer input is detected.
- Passed ScreenCaptureKit window frames directly to vision as RGB pixels, with
  a stable background around rounded window corners.
- Added native macOS window discovery and application activation.

## 0.1.0.0 - 2026-06-13

- First working release of the macOS bot.
- Added startup dependency probing for capture and replay modes.
- Added first-party fixtures, README imagery, and BSD-3-Clause licensing.
