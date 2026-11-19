#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
  int32_t station;
  double value;
} Reading;
typedef struct {
  Reading reading;
  uint64_t count;
} Sample;

typedef struct {
  float low;
  float high;
} Range;
extern double packed_range(double, double, double, double, double, double,
                           double, double, Range, Range);
extern Range shift_range(Range, float);
extern const uint8_t *station_pointer(const uint8_t *);

extern Sample weather_sample(Sample);
extern Reading select_reading(int64_t, int64_t, int64_t, int64_t, int64_t,
                              int64_t, Reading);
extern int8_t sign(int8_t, bool);
extern int64_t checked_ratio(int64_t);

Reading calibrate(Reading r) {
  r.station += 2;
  r.value *= 0.5;
  return r;
}

Sample close_sample(Sample sample) {
  sample.count += 10;
  return sample;
}

void record_cleanup(void) {
  fputs("ratio cleanup\n", stderr);
  fflush(stderr);
}

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1) {
    (void)checked_ratio(0);
    return 0;
  }
  Sample initial = {{41, 12.5}, 8};
  Sample sample = weather_sample(initial);
  assert(sample.reading.station == 43 && sample.reading.value == 6.25 &&
         sample.count == 19);
  assert(initial.count == 8 && initial.reading.station == 41);
  Reading other = select_reading(2, 3, 5, 7, 11, 13, initial.reading);
  assert(other.station == 41 && other.value == 53.5);
  assert(sign(-97, true) == -97 && sign(-97, false) == 0);
  Range moved = shift_range((Range){-2.5f, 7.0f}, 4.0f);
  assert(moved.low == 1.5f && moved.high == 11.0f);
  assert(packed_range(1, 2, 3, 4, 5, 6, 7, 8, (Range){3.5f, 4.0f},
                      (Range){-1.0f, 8.0f}) == 47.5);
  const uint8_t station = 17;
  assert(station_pointer(&station) == &station);
  assert(checked_ratio(2) == 21);
  return 0;
}
