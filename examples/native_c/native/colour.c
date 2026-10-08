#include "colour.h"

#include <math.h>

static double linear(int32_t channel) {
    double c = (double)channel / COLOUR_CHANNEL_MAX;
    return c <= 0.04045 ? c / 12.92 : pow((c + 0.055) / 1.055, 2.4);
}

int32_t native_c_lightness(int32_t red, int32_t green, int32_t blue) {
    double y = 0.2126 * linear(red) + 0.7152 * linear(green) + 0.0722 * linear(blue);
    double l = y > 216.0 / 24389.0 ? 116.0 * cbrt(y) - 16.0 : y * 24389.0 / 27.0;
    return (int32_t)lround(l * 10.0);
}

int64_t native_c_distance_sq(int32_t r1, int32_t g1, int32_t b1, int32_t r2, int32_t g2, int32_t b2) {
    int64_t dr = r1 - r2;
    int64_t dg = g1 - g2;
    int64_t db = b1 - b2;
    return dr * dr + dg * dg + db * db;
}
