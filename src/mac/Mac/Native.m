#import "Native.h"

#import <ApplicationServices/ApplicationServices.h>
#import <Foundation/Foundation.h>
#import <ImageIO/ImageIO.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static const useconds_t eventDelayUs = 20000;

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

  CGEventPost(kCGHIDEventTap, event);
  CFRelease(event);
  return YES;
}

int gsb_capture_png(
    int32_t x,
    int32_t y,
    int32_t width,
    int32_t height,
    uint8_t **outBytes,
    size_t *outLength,
    char **outError) {
  @autoreleasepool {
    if (outError != NULL) {
      *outError = NULL;
    }
    if (outBytes == NULL || outLength == NULL || width <= 0 || height <= 0) {
      setError(outError, @"Screen capture received an invalid rectangle.");
      return 1;
    }
    *outBytes = NULL;
    *outLength = 0;

    if (!CGPreflightScreenCaptureAccess()
        && !CGRequestScreenCaptureAccess()) {
      setError(
          outError,
          @"Screen Recording permission is required in System Settings > "
           "Privacy & Security > Screen & System Audio Recording.");
      return 1;
    }

    if (@available(macOS 15.2, *)) {
      // ScreenCaptureKit is asynchronous. The C ABI stays synchronous so
      // Haskell receives one owned byte buffer or one owned error string.
      dispatch_semaphore_t finished = dispatch_semaphore_create(0);
      __block CGImageRef capturedImage = NULL;
      __block NSString *captureError = nil;
      CGRect rect = CGRectMake(x, y, width, height);

      [SCScreenshotManager
          captureImageInRect:rect
          completionHandler:^(CGImageRef image, NSError *error) {
            if (image != NULL) {
              capturedImage = CGImageRetain(image);
            }
            if (error != nil) {
              captureError = [error.localizedDescription copy];
            }
            dispatch_semaphore_signal(finished);
          }];
      dispatch_semaphore_wait(finished, DISPATCH_TIME_FOREVER);

      if (capturedImage == NULL) {
        setError(
            outError,
            captureError == nil ? @"ScreenCaptureKit returned no image."
                                : captureError);
        return 1;
      }

      NSMutableData *png = [NSMutableData data];
      CGImageDestinationRef destination = CGImageDestinationCreateWithData(
          (__bridge CFMutableDataRef)png,
          CFSTR("public.png"),
          1,
          NULL);
      if (destination == NULL) {
        CGImageRelease(capturedImage);
        setError(outError, @"ImageIO could not create a PNG encoder.");
        return 1;
      }

      CGImageDestinationAddImage(destination, capturedImage, NULL);
      BOOL encoded = CGImageDestinationFinalize(destination);
      CFRelease(destination);
      CGImageRelease(capturedImage);
      if (!encoded) {
        setError(outError, @"ImageIO could not encode the captured frame.");
        return 1;
      }
      if (png.length == 0) {
        setError(outError, @"ImageIO encoded an empty captured frame.");
        return 1;
      }

      uint8_t *bytes = malloc(png.length);
      if (bytes == NULL) {
        setError(outError, @"Could not allocate memory for the captured PNG.");
        return 1;
      }
      memcpy(bytes, png.bytes, png.length);
      *outBytes = bytes;
      *outLength = png.length;
      return 0;
    }

    setError(outError, @"Screen capture requires macOS 15.2 or later.");
    return 1;
  }
}

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
    return postMouseEvent(kCGEventLeftMouseUp, x, y, outError) ? 0 : 1;
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

    int32_t startX = coordinates[0];
    int32_t startY = coordinates[1];
    if (!postMouseEvent(kCGEventMouseMoved, startX, startY, outError)) {
      return 1;
    }
    usleep(eventDelayUs);
    if (!postMouseEvent(kCGEventLeftMouseDown, startX, startY, outError)) {
      return 1;
    }
    usleep(eventDelayUs);

    // A few evenly spaced drag events produce a compact swipe without the
    // hundreds of eased events that overloaded iPhone Mirroring.
    for (size_t index = 1; index < pointCount; ++index) {
      int32_t x = coordinates[index * 2];
      int32_t y = coordinates[index * 2 + 1];
      if (!postMouseEvent(kCGEventLeftMouseDragged, x, y, outError)) {
        postMouseEvent(kCGEventLeftMouseUp, x, y, NULL);
        return 1;
      }
      usleep(eventDelayUs);
    }

    size_t last = pointCount - 1;
    int32_t endX = coordinates[last * 2];
    int32_t endY = coordinates[last * 2 + 1];
    return postMouseEvent(kCGEventLeftMouseUp, endX, endY, outError) ? 0 : 1;
  }
}

void gsb_free(void *pointer) {
  free(pointer);
}
