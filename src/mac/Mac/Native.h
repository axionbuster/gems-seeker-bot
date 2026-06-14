#ifndef GSB_MAC_NATIVE_H
#define GSB_MAC_NATIVE_H

#include <stddef.h>
#include <stdint.h>

// Capture a global screen rectangle and allocate PNG bytes for the caller.
int gsb_capture_png(
    int32_t x,
    int32_t y,
    int32_t width,
    int32_t height,
    uint8_t **out_bytes,
    size_t *out_length,
    char **out_error);

// Post one primary-button click at an absolute screen point.
int gsb_click(int32_t x, int32_t y, char **out_error);

// Drag through absolute screen points, yielding when other pointer input occurs.
// Returns 0 when complete, 2 when interrupted by pointer input, and 1 on error.
int gsb_drag(
    const int32_t *coordinates,
    size_t point_count,
    char **out_error);

// Release memory returned through an output pointer from this bridge.
void gsb_free(void *pointer);

#endif
