#ifndef NATIVE_C_COLOUR_H
#define NATIVE_C_COLOUR_H

#include <stdint.h>

/* CIE L* lightness of an sRGB colour, in tenths (0..1000). */
int32_t native_c_lightness(int32_t red, int32_t green, int32_t blue);

/* Squared distance between two colours in channel units. */
int64_t native_c_distance_sq(int32_t r1, int32_t g1, int32_t b1, int32_t r2,
                             int32_t g2, int32_t b2);

#endif
