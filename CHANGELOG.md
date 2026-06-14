# Changelog

All notable changes to this project will be documented in this file.

## Unreleased

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
