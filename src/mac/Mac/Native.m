#import "Native.h"

#import <AppKit/AppKit.h>
#import <ApplicationServices/ApplicationServices.h>
#import <AVFoundation/AVFoundation.h>
#import <Foundation/Foundation.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Table of contents
 * 1. Shared errors and permissions
 * 2. Raw ScreenCaptureKit frames
 * 3. ScreenCaptureKit movie recording
 * 4. Native application windows
 * 5. Core Graphics pointer events
 */

static const useconds_t dragLeadInUs = 500000;
static const useconds_t eventDelayUs = 20000;
static const useconds_t monitorPollUs = 2000;
static const int nativeInterrupted = 2;
static const int64_t syntheticEventTag = INT64_C(0x4753424E41544956);
static BOOL hasAutomationPosition = NO;
static CGPoint automationPosition;

typedef struct {
  BOOL interrupted;
  CFMachPortRef eventTap;
} PointerMonitor;

@interface GSBRecordingSession
    : NSObject <SCRecordingOutputDelegate, SCStreamDelegate>

@property(nonatomic, strong) SCStream *stream;
@property(nonatomic, strong) SCRecordingOutput *output;
@property(nonatomic, strong) dispatch_semaphore_t startedSemaphore;
@property(nonatomic, strong) dispatch_semaphore_t finishedSemaphore;
@property(nonatomic, copy) NSString *recordingError;
@property(nonatomic) BOOL started;

@end

static GSBRecordingSession *activeRecording = nil;

@implementation GSBRecordingSession

- (void)recordingOutputDidStartRecording:
    (SCRecordingOutput *)recordingOutput {
  (void)recordingOutput;
  @synchronized(self) {
    self.started = YES;
  }
  dispatch_semaphore_signal(self.startedSemaphore);
}

- (void)recordingOutput:
    (SCRecordingOutput *)recordingOutput
    didFailWithError:(NSError *)error {
  (void)recordingOutput;
  @synchronized(self) {
    if (self.recordingError == nil) {
      self.recordingError = error.localizedDescription;
    }
  }
  dispatch_semaphore_signal(self.startedSemaphore);
  dispatch_semaphore_signal(self.finishedSemaphore);
}

- (void)recordingOutputDidFinishRecording:
    (SCRecordingOutput *)recordingOutput {
  (void)recordingOutput;
  dispatch_semaphore_signal(self.finishedSemaphore);
}

- (void)stream:(SCStream *)stream didStopWithError:(NSError *)error {
  (void)stream;
  @synchronized(self) {
    if (self.recordingError == nil) {
      self.recordingError = error.localizedDescription;
    }
  }
  dispatch_semaphore_signal(self.startedSemaphore);
  dispatch_semaphore_signal(self.finishedSemaphore);
}

@end

// 1. Shared errors and permissions -------------------------------------------

static void setError(char **outError, NSString *message) {
  if (outError == NULL) {
    return;
  }

  const char *utf8 = message.UTF8String;
  *outError = strdup(utf8 == NULL ? "unknown macOS error" : utf8);
}

static BOOL ensureAccessibility(char **outError) {
  NSDictionary *options = @{
    (__bridge NSString *)kAXTrustedCheckOptionPrompt : @YES
  };
  if (AXIsProcessTrustedWithOptions(
          (__bridge CFDictionaryRef)options)) {
    return YES;
  }

  setError(
      outError,
      @"Accessibility permission is required in System Settings > "
       "Privacy & Security > Accessibility.");
  return NO;
}

static BOOL postMouseEvent(
    CGEventType type,
    int32_t x,
    int32_t y,
    char **outError) {
  CGEventRef event = CGEventCreateMouseEvent(
      NULL,
      type,
      CGPointMake(x, y),
      kCGMouseButtonLeft);
  if (event == NULL) {
    setError(outError, @"Core Graphics could not create a mouse event.");
    return NO;
  }

  CGEventSetIntegerValueField(
      event,
      kCGEventSourceUserData,
      syntheticEventTag);
  CGEventPost(kCGHIDEventTap, event);
  CFRelease(event);
  return YES;
}

static CGPoint currentPointerLocation(void) {
  CGEventRef event = CGEventCreate(NULL);
  if (event == NULL) {
    return CGPointZero;
  }

  CGPoint location = CGEventGetLocation(event);
  CFRelease(event);
  return location;
}

static BOOL pointerMovedFromAutomationPosition(void) {
  if (!hasAutomationPosition) {
    return NO;
  }

  CGPoint current = currentPointerLocation();
  return fabs(current.x - automationPosition.x) > 1.0
      || fabs(current.y - automationPosition.y) > 1.0;
}

static CGEventRef monitorPointerInput(
    CGEventTapProxy proxy,
    CGEventType type,
    CGEventRef event,
    void *userInfo) {
  (void)proxy;
  PointerMonitor *monitor = userInfo;
  if (type == kCGEventTapDisabledByTimeout
      || type == kCGEventTapDisabledByUserInput) {
    monitor->interrupted = YES;
    return event;
  }

  int64_t tag = CGEventGetIntegerValueField(
      event,
      kCGEventSourceUserData);
  if (tag != syntheticEventTag) {
    monitor->interrupted = YES;
  }
  return event;
}

static CFMachPortRef createPointerMonitor(
    PointerMonitor *monitor,
    char **outError) {
  CGEventMask mask =
      CGEventMaskBit(kCGEventLeftMouseDown)
      | CGEventMaskBit(kCGEventLeftMouseUp)
      | CGEventMaskBit(kCGEventRightMouseDown)
      | CGEventMaskBit(kCGEventRightMouseUp)
      | CGEventMaskBit(kCGEventMouseMoved)
      | CGEventMaskBit(kCGEventLeftMouseDragged)
      | CGEventMaskBit(kCGEventRightMouseDragged)
      | CGEventMaskBit(kCGEventScrollWheel)
      | CGEventMaskBit(kCGEventOtherMouseDown)
      | CGEventMaskBit(kCGEventOtherMouseUp)
      | CGEventMaskBit(kCGEventOtherMouseDragged);
  CFMachPortRef eventTap = CGEventTapCreate(
      kCGHIDEventTap,
      kCGHeadInsertEventTap,
      kCGEventTapOptionListenOnly,
      mask,
      monitorPointerInput,
      monitor);
  if (eventTap == NULL) {
    setError(
        outError,
        @"Could not monitor pointer input for a weak gesture.");
  }
  return eventTap;
}

static BOOL waitWhileMonitoring(
    useconds_t durationUs,
    PointerMonitor *monitor) {
  useconds_t remaining = durationUs;
  while (remaining > 0 && !monitor->interrupted) {
    useconds_t slice = MIN(remaining, monitorPollUs);
    CFRunLoopRunInMode(
        kCFRunLoopDefaultMode,
        (CFTimeInterval)slice / 1000000.0,
        true);
    remaining -= slice;
  }
  return !monitor->interrupted;
}

static void releasePrimaryButtonAtCurrentLocation(void) {
  CGPoint location = currentPointerLocation();
  postMouseEvent(
      kCGEventLeftMouseUp,
      (int32_t)llround(location.x),
      (int32_t)llround(location.y),
      NULL);
}

// 2. Raw ScreenCaptureKit frames ---------------------------------------------

static uint8_t *copyRgbPixels(
    CGImageRef image,
    int32_t *outWidth,
    int32_t *outHeight,
    char **outError) {
  size_t width = CGImageGetWidth(image);
  size_t height = CGImageGetHeight(image);
  if (width == 0 || height == 0 || width > INT32_MAX || height > INT32_MAX
      || width > SIZE_MAX / 4
      || height > SIZE_MAX / (width * 4)
      || width > SIZE_MAX / 3
      || height > SIZE_MAX / (width * 3)) {
    setError(outError, @"ScreenCaptureKit returned invalid image dimensions.");
    return NULL;
  }

  size_t rgbaLength = width * height * 4;
  uint8_t *rgba = calloc(rgbaLength, 1);
  if (rgba == NULL) {
    setError(outError, @"Could not allocate the captured RGBA buffer.");
    return NULL;
  }

  CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
  if (colorSpace == NULL) {
    free(rgba);
    setError(outError, @"Core Graphics could not create an RGB color space.");
    return NULL;
  }
  CGContextRef context = CGBitmapContextCreate(
      rgba,
      width,
      height,
      8,
      width * 4,
      colorSpace,
      kCGImageAlphaPremultipliedLast | kCGBitmapByteOrder32Big);
  CGColorSpaceRelease(colorSpace);
  if (context == NULL) {
    free(rgba);
    setError(outError, @"Core Graphics could not create an RGB bitmap.");
    return NULL;
  }

  CGContextDrawImage(context, CGRectMake(0, 0, width, height), image);
  CGContextRelease(context);

  size_t rgbLength = width * height * 3;
  uint8_t *rgb = malloc(rgbLength);
  if (rgb == NULL) {
    free(rgba);
    setError(outError, @"Could not allocate the captured RGB buffer.");
    return NULL;
  }
  for (size_t source = 0, target = 0; target < rgbLength;
       source += 4, target += 3) {
    rgb[target] = rgba[source];
    rgb[target + 1] = rgba[source + 1];
    rgb[target + 2] = rgba[source + 2];
  }
  free(rgba);

  *outWidth = (int32_t)width;
  *outHeight = (int32_t)height;
  return rgb;
}

int gsb_capture_rgb(
    uint32_t windowID,
    uint8_t **outPixels,
    int32_t *outWidth,
    int32_t *outHeight,
    char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (windowID == 0 || outPixels == NULL || outWidth == NULL
        || outHeight == NULL) {
      setError(outError, @"Screen capture received an invalid window.");
      return 1;
    }
    *outPixels = NULL;
    *outWidth = 0;
    *outHeight = 0;

    if (!CGPreflightScreenCaptureAccess()
        && !CGRequestScreenCaptureAccess()) {
      setError(
          outError,
          @"Screen Recording permission is required in System Settings > "
           "Privacy & Security > Screen & System Audio Recording.");
      return 1;
    }

    if (@available(macOS 15.2, *)) {
      // Resolve the CGWindowID to ScreenCaptureKit's window object, then use
      // an independent-window filter so transparent corners do not sample the
      // desktop behind the rounded iPhone Mirroring window.
      dispatch_semaphore_t finished = dispatch_semaphore_create(0);
      __block CGImageRef capturedImage = NULL;
      __block NSString *captureError = nil;
      [SCShareableContent
          getShareableContentExcludingDesktopWindows:YES
          onScreenWindowsOnly:YES
          completionHandler:^(
              SCShareableContent *content,
              NSError *contentError) {
            SCWindow *target = nil;
            for (SCWindow *window in content.windows) {
              if (window.windowID == windowID) {
                target = window;
                break;
              }
            }
            if (target == nil) {
              captureError =
                  contentError == nil
                      ? @"ScreenCaptureKit could not find the selected window."
                      : [contentError.localizedDescription copy];
              dispatch_semaphore_signal(finished);
              return;
            }

            SCContentFilter *filter = [[SCContentFilter alloc]
                initWithDesktopIndependentWindow:target];
            SCShareableContentInfo *info =
                [SCShareableContent infoForFilter:filter];
            SCStreamConfiguration *configuration =
                [[SCStreamConfiguration alloc] init];
            configuration.width = (size_t)llround(
                target.frame.size.width * info.pointPixelScale);
            configuration.height = (size_t)llround(
                target.frame.size.height * info.pointPixelScale);
            configuration.showsCursor = NO;
            configuration.ignoreShadowsSingleWindow = YES;
            configuration.backgroundColor =
                CGColorGetConstantColor(kCGColorBlack);

            [SCScreenshotManager
                captureImageWithFilter:filter
                configuration:configuration
                completionHandler:^(CGImageRef image, NSError *error) {
                  if (image != NULL) {
                    capturedImage = CGImageRetain(image);
                  }
                  if (error != nil) {
                    captureError = [error.localizedDescription copy];
                  }
                  dispatch_semaphore_signal(finished);
                }];
          }];
      dispatch_semaphore_wait(finished, DISPATCH_TIME_FOREVER);

      if (capturedImage == NULL) {
        setError(
            outError,
            captureError == nil ? @"ScreenCaptureKit returned no image."
                                : captureError);
        return 1;
      }

      uint8_t *pixels = copyRgbPixels(
          capturedImage,
          outWidth,
          outHeight,
          outError);
      CGImageRelease(capturedImage);
      if (pixels == NULL) {
        return 1;
      }

      *outPixels = pixels;
      return 0;
    }

    setError(outError, @"Screen capture requires macOS 15.2 or later.");
    return 1;
  }
}

// 3. ScreenCaptureKit movie recording ---------------------------------------

static NSInteger maximumFpsForWindow(CGRect windowFrame) {
  NSScreen *targetScreen = nil;
  CGFloat largestIntersection = 0;
  for (NSScreen *screen in NSScreen.screens) {
    NSNumber *screenNumber = screen.deviceDescription[@"NSScreenNumber"];
    if (screenNumber == nil) {
      continue;
    }
    CGRect displayBounds =
        CGDisplayBounds((CGDirectDisplayID)screenNumber.unsignedIntValue);
    CGRect intersection = CGRectIntersection(windowFrame, displayBounds);
    if (CGRectIsNull(intersection)) {
      continue;
    }
    CGFloat area = intersection.size.width * intersection.size.height;
    if (area > largestIntersection) {
      targetScreen = screen;
      largestIntersection = area;
    }
  }

  NSInteger maximumFps =
      targetScreen == nil ? 60 : targetScreen.maximumFramesPerSecond;
  return maximumFps > 0 ? maximumFps : 60;
}

static size_t evenPixelDimension(CGFloat points, int32_t pixelsPerPoint) {
  size_t pixels = (size_t)llround(points * pixelsPerPoint);
  return pixels + (pixels % 2);
}

int gsb_start_recording(
    uint32_t windowID,
    const char *outputPath,
    int32_t preferredFps,
    int32_t pixelsPerPoint,
    int32_t *outWidth,
    int32_t *outHeight,
    int32_t *outFps,
    char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (windowID == 0 || outputPath == NULL || preferredFps <= 0
        || pixelsPerPoint <= 0 || outWidth == NULL || outHeight == NULL
        || outFps == NULL) {
      setError(outError, @"Screen recording received invalid arguments.");
      return 1;
    }
    if (activeRecording != nil) {
      setError(outError, @"A screen recording is already active.");
      return 1;
    }
    *outWidth = 0;
    *outHeight = 0;
    *outFps = 0;

    if (!CGPreflightScreenCaptureAccess()
        && !CGRequestScreenCaptureAccess()) {
      setError(
          outError,
          @"Screen Recording permission is required in System Settings > "
           "Privacy & Security > Screen & System Audio Recording.");
      return 1;
    }

    NSString *path = [[NSString alloc] initWithUTF8String:outputPath];
    if (path == nil) {
      setError(outError, @"Recording path is not valid UTF-8.");
      return 1;
    }
    if ([NSFileManager.defaultManager fileExistsAtPath:path]) {
      setError(outError, @"The recording output path already exists.");
      return 1;
    }

    if (@available(macOS 15.2, *)) {
      dispatch_semaphore_t contentFinished = dispatch_semaphore_create(0);
      __block GSBRecordingSession *session = nil;
      __block NSString *setupError = nil;
      __block size_t outputWidth = 0;
      __block size_t outputHeight = 0;
      __block NSInteger outputFps = 0;

      [SCShareableContent
          getShareableContentExcludingDesktopWindows:YES
          onScreenWindowsOnly:YES
          completionHandler:^(
              SCShareableContent *content,
              NSError *contentError) {
            SCWindow *target = nil;
            for (SCWindow *window in content.windows) {
              if (window.windowID == windowID) {
                target = window;
                break;
              }
            }
            if (target == nil) {
              setupError =
                  contentError == nil
                      ? @"ScreenCaptureKit could not find the selected window."
                      : [contentError.localizedDescription copy];
              dispatch_semaphore_signal(contentFinished);
              return;
            }

            outputWidth =
                evenPixelDimension(target.frame.size.width, pixelsPerPoint);
            outputHeight =
                evenPixelDimension(target.frame.size.height, pixelsPerPoint);
            if (outputWidth == 0 || outputHeight == 0
                || outputWidth > INT32_MAX || outputHeight > INT32_MAX) {
              setupError = @"Screen recording resolved invalid dimensions.";
              dispatch_semaphore_signal(contentFinished);
              return;
            }

            NSInteger displayFps = maximumFpsForWindow(target.frame);
            outputFps = MIN((NSInteger)preferredFps, displayFps);

            SCContentFilter *filter = [[SCContentFilter alloc]
                initWithDesktopIndependentWindow:target];
            SCStreamConfiguration *streamConfiguration =
                [[SCStreamConfiguration alloc] init];
            streamConfiguration.width = outputWidth;
            streamConfiguration.height = outputHeight;
            streamConfiguration.minimumFrameInterval =
                CMTimeMake(1, (int32_t)outputFps);
            streamConfiguration.pixelFormat = kCVPixelFormatType_32BGRA;
            streamConfiguration.scalesToFit = YES;
            streamConfiguration.preservesAspectRatio = YES;
            streamConfiguration.showsCursor = NO;
            streamConfiguration.ignoreShadowsSingleWindow = YES;
            streamConfiguration.backgroundColor =
                CGColorGetConstantColor(kCGColorBlack);
            streamConfiguration.queueDepth = 8;

            GSBRecordingSession *newSession =
                [[GSBRecordingSession alloc] init];
            newSession.startedSemaphore = dispatch_semaphore_create(0);
            newSession.finishedSemaphore = dispatch_semaphore_create(0);

            SCRecordingOutputConfiguration *recordingConfiguration =
                [[SCRecordingOutputConfiguration alloc] init];
            recordingConfiguration.outputURL =
                [NSURL fileURLWithPath:path];
            if (![recordingConfiguration.availableOutputFileTypes
                    containsObject:AVFileTypeQuickTimeMovie]) {
              setupError =
                  @"ScreenCaptureKit cannot write QuickTime movies on this Mac.";
              dispatch_semaphore_signal(contentFinished);
              return;
            }
            recordingConfiguration.outputFileType =
                AVFileTypeQuickTimeMovie;
            recordingConfiguration.videoCodecType =
                AVVideoCodecTypeH264;

            newSession.output = [[SCRecordingOutput alloc]
                initWithConfiguration:recordingConfiguration
                delegate:newSession];
            newSession.stream = [[SCStream alloc]
                initWithFilter:filter
                configuration:streamConfiguration
                delegate:newSession];
            NSError *outputError = nil;
            if (![newSession.stream
                    addRecordingOutput:newSession.output
                    error:&outputError]) {
              setupError = outputError.localizedDescription;
              dispatch_semaphore_signal(contentFinished);
              return;
            }

            session = newSession;
            dispatch_semaphore_signal(contentFinished);
          }];
      dispatch_semaphore_wait(contentFinished, DISPATCH_TIME_FOREVER);

      if (session == nil) {
        setError(
            outError,
            setupError == nil ? @"Screen recording setup failed." : setupError);
        return 1;
      }

      activeRecording = session;
      [session.stream
          startCaptureWithCompletionHandler:^(NSError *error) {
            if (error != nil) {
              @synchronized(session) {
                if (session.recordingError == nil) {
                  session.recordingError = error.localizedDescription;
                }
              }
              dispatch_semaphore_signal(session.startedSemaphore);
            }
          }];
      dispatch_semaphore_wait(
          session.startedSemaphore,
          DISPATCH_TIME_FOREVER);

      NSString *recordingError = nil;
      BOOL started = NO;
      @synchronized(session) {
        recordingError = session.recordingError;
        started = session.started;
      }
      if (!started) {
        activeRecording = nil;
        [session.stream stopCaptureWithCompletionHandler:nil];
        [NSFileManager.defaultManager removeItemAtPath:path error:nil];
        setError(
            outError,
            recordingError == nil
                ? @"ScreenCaptureKit did not start recording."
                : recordingError);
        return 1;
      }

      *outWidth = (int32_t)outputWidth;
      *outHeight = (int32_t)outputHeight;
      *outFps = (int32_t)outputFps;
      return 0;
    }

    setError(outError, @"Screen recording requires macOS 15.2 or later.");
    return 1;
  }
}

int gsb_stop_recording(char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    GSBRecordingSession *session = activeRecording;
    if (session == nil) {
      setError(outError, @"No screen recording is active.");
      return 1;
    }

    [session.stream
        stopCaptureWithCompletionHandler:^(NSError *error) {
          if (error != nil) {
            @synchronized(session) {
              if (session.recordingError == nil) {
                session.recordingError = error.localizedDescription;
              }
            }
            dispatch_semaphore_signal(session.finishedSemaphore);
          }
        }];
    dispatch_semaphore_wait(
        session.finishedSemaphore,
        DISPATCH_TIME_FOREVER);

    NSString *recordingError = nil;
    @synchronized(session) {
      recordingError = session.recordingError;
    }
    activeRecording = nil;
    if (recordingError != nil) {
      setError(outError, recordingError);
      return 1;
    }
    return 0;
  }
}

// 4. Native application windows ---------------------------------------------

int gsb_list_windows(
    const char *appName,
    int32_t **outCoordinates,
    size_t *outWindowCount,
    char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (appName == NULL || outCoordinates == NULL || outWindowCount == NULL) {
      setError(outError, @"Window lookup received invalid arguments.");
      return 1;
    }
    *outCoordinates = NULL;
    *outWindowCount = 0;

    NSString *owner = [[NSString alloc] initWithUTF8String:appName];
    if (owner == nil) {
      setError(outError, @"Application name is not valid UTF-8.");
      return 1;
    }

    CFArrayRef windowInfo = CGWindowListCopyWindowInfo(
        kCGWindowListOptionOnScreenOnly
            | kCGWindowListExcludeDesktopElements,
        kCGNullWindowID);
    if (windowInfo == NULL) {
      setError(outError, @"Core Graphics could not list on-screen windows.");
      return 1;
    }
    NSArray<NSDictionary *> *windows = CFBridgingRelease(windowInfo);
    if (windows.count > SIZE_MAX / (5 * sizeof(int32_t))) {
      setError(outError, @"Core Graphics returned too many windows.");
      return 1;
    }

    int32_t *coordinates = calloc(
        windows.count * 5,
        sizeof(int32_t));
    if (coordinates == NULL && windows.count > 0) {
      setError(outError, @"Could not allocate the window geometry buffer.");
      return 1;
    }

    size_t count = 0;
    for (NSDictionary *window in windows) {
      NSString *windowOwner = window[(__bridge NSString *)kCGWindowOwnerName];
      NSNumber *layer = window[(__bridge NSString *)kCGWindowLayer];
      NSNumber *windowID = window[(__bridge NSString *)kCGWindowNumber];
      NSDictionary *bounds =
          window[(__bridge NSString *)kCGWindowBounds];
      CGRect rect = CGRectZero;
      if (![windowOwner isEqualToString:owner]
          || layer.integerValue != 0
          || windowID == nil
          || bounds == nil
          || !CGRectMakeWithDictionaryRepresentation(
              (__bridge CFDictionaryRef)bounds,
              &rect)) {
        continue;
      }

      size_t offset = count * 5;
      coordinates[offset] = windowID.intValue;
      coordinates[offset + 1] = (int32_t)llround(rect.origin.x);
      coordinates[offset + 2] = (int32_t)llround(rect.origin.y);
      coordinates[offset + 3] = (int32_t)llround(rect.size.width);
      coordinates[offset + 4] = (int32_t)llround(rect.size.height);
      count += 1;
    }

    if (count == 0) {
      free(coordinates);
      return 0;
    }
    *outCoordinates = coordinates;
    *outWindowCount = count;
    return 0;
  }
}

int gsb_focus_app(const char *appName, char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (appName == NULL) {
      setError(outError, @"Application activation received no name.");
      return 1;
    }

    NSString *name = [[NSString alloc] initWithUTF8String:appName];
    if (name == nil) {
      setError(outError, @"Application name is not valid UTF-8.");
      return 1;
    }
    for (NSRunningApplication *application
         in NSWorkspace.sharedWorkspace.runningApplications) {
      if ([application.localizedName isEqualToString:name]) {
        if ([application activateWithOptions:NSApplicationActivateAllWindows]) {
          return 0;
        }
        setError(outError, @"macOS could not activate the application.");
        return 1;
      }
    }

    setError(
        outError,
        [NSString stringWithFormat:@"The '%@' application is not running.", name]);
    return 1;
  }
}

// 5. Core Graphics pointer events --------------------------------------------

int gsb_click(int32_t x, int32_t y, char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (!ensureAccessibility(outError)) {
      return 1;
    }

    if (!postMouseEvent(kCGEventMouseMoved, x, y, outError)
        || !postMouseEvent(kCGEventLeftMouseDown, x, y, outError)) {
      return 1;
    }
    usleep(15000);
    if (!postMouseEvent(kCGEventLeftMouseUp, x, y, outError)) {
      return 1;
    }
    automationPosition = CGPointMake(x, y);
    hasAutomationPosition = YES;
    return 0;
  }
}

int gsb_drag(
    const int32_t *coordinates,
    size_t pointCount,
    char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (coordinates == NULL || pointCount < 2) {
      setError(outError, @"A drag requires at least two points.");
      return 1;
    }
    if (!ensureAccessibility(outError)) {
      return 1;
    }
    if (pointerMovedFromAutomationPosition()) {
      hasAutomationPosition = NO;
      return nativeInterrupted;
    }

    PointerMonitor monitor = {
      .interrupted = NO,
      .eventTap = NULL,
    };
    monitor.eventTap = createPointerMonitor(&monitor, outError);
    if (monitor.eventTap == NULL) {
      return 1;
    }
    CFRunLoopSourceRef monitorSource = CFMachPortCreateRunLoopSource(
        kCFAllocatorDefault,
        monitor.eventTap,
        0);
    if (monitorSource == NULL) {
      CFRelease(monitor.eventTap);
      setError(outError, @"Could not attach the pointer input monitor.");
      return 1;
    }
    CFRunLoopAddSource(
        CFRunLoopGetCurrent(),
        monitorSource,
        kCFRunLoopCommonModes);

    if (!waitWhileMonitoring(dragLeadInUs, &monitor)) {
      CFRunLoopRemoveSource(
          CFRunLoopGetCurrent(),
          monitorSource,
          kCFRunLoopCommonModes);
      CFRelease(monitorSource);
      CFRelease(monitor.eventTap);
      return nativeInterrupted;
    }

    int32_t startX = coordinates[0];
    int32_t startY = coordinates[1];
    if (!postMouseEvent(kCGEventMouseMoved, startX, startY, outError)) {
      goto dragError;
    }
    if (!waitWhileMonitoring(eventDelayUs, &monitor)) {
      goto dragInterruptedBeforeDown;
    }
    if (!postMouseEvent(kCGEventLeftMouseDown, startX, startY, outError)) {
      goto dragError;
    }
    if (!waitWhileMonitoring(eventDelayUs, &monitor)) {
      goto dragInterrupted;
    }

    // A few evenly spaced drag events produce a compact swipe without the
    // hundreds of eased events that overloaded iPhone Mirroring.
    for (size_t index = 1; index < pointCount; ++index) {
      int32_t x = coordinates[index * 2];
      int32_t y = coordinates[index * 2 + 1];
      if (!postMouseEvent(kCGEventLeftMouseDragged, x, y, outError)) {
        goto dragErrorWithButtonDown;
      }
      if (!waitWhileMonitoring(eventDelayUs, &monitor)) {
        goto dragInterrupted;
      }
    }

    size_t last = pointCount - 1;
    int32_t endX = coordinates[last * 2];
    int32_t endY = coordinates[last * 2 + 1];
    if (!postMouseEvent(kCGEventLeftMouseUp, endX, endY, outError)) {
      goto dragError;
    }
    automationPosition = CGPointMake(endX, endY);
    hasAutomationPosition = YES;
    CFRunLoopRemoveSource(
        CFRunLoopGetCurrent(),
        monitorSource,
        kCFRunLoopCommonModes);
    CFRelease(monitorSource);
    CFRelease(monitor.eventTap);
    return 0;

dragInterrupted:
    releasePrimaryButtonAtCurrentLocation();
dragInterruptedBeforeDown:
    CFRunLoopRemoveSource(
        CFRunLoopGetCurrent(),
        monitorSource,
        kCFRunLoopCommonModes);
    CFRelease(monitorSource);
    CFRelease(monitor.eventTap);
    hasAutomationPosition = NO;
    return nativeInterrupted;

dragErrorWithButtonDown:
    releasePrimaryButtonAtCurrentLocation();
dragError:
    CFRunLoopRemoveSource(
        CFRunLoopGetCurrent(),
        monitorSource,
        kCFRunLoopCommonModes);
    CFRelease(monitorSource);
    CFRelease(monitor.eventTap);
    hasAutomationPosition = NO;
    return 1;
  }
}

void gsb_free(void *pointer) {
  free(pointer);
}
