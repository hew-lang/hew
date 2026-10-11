#include "hew.h"
#include <stdio.h>
#include <string.h>

int main(void) {
  const uint8_t source[4] = {7, 0, 9, 42};
  HewBytes value = hew_bytes_copy(source, 4);
  hew_bytes_clone_ref(value.ptr);
  HewBytes view = value;
  view.offset = 1;
  view.len = 3;
  if (memcmp(hew_bytes_data(&view), source + 1, 3) != 0)
    return 1;
  hew_bytes_drop(value.ptr);
  if (memcmp(hew_bytes_data(&view), source + 1, 3) != 0)
    return 2;
  hew_bytes_drop(value.ptr);
  puts("sdk-native-consumer-ok");
  return 0;
}
