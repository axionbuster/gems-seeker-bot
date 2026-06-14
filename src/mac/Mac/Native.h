#ifndef GSB_MAC_NATIVE_H
#define GSB_MAC_NATIVE_H

#include <stddef.h>
#include <stdint.h>

// Capture one native window and allocate packed RGB pixels.
int gsb_capture_rgb(
    uint32_t window_id,
    uint8_t **out_pixels,
    int32_t *out_width,
    int32_t *out_height,
    char **out_error);

// List layer-zero on-screen windows as ID, x, y, width, height records.
int gsb_list_windows(
    const char *app_name,
    int32_t **out_coordinates,
    size_t *out_window_count,
    char **out_error);

// Bring the named running application and all of its windows forward.
int gsb_focus_app(const char *app_name, char **out_error);

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
