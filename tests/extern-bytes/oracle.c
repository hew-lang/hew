/* Independent C return ABI and exactly-once ownership oracle. */
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  unsigned char *ptr;
  uint32_t offset;
  uint32_t len;
} BytesTriple;

extern unsigned char *hew_bytes_new(uint32_t capacity);
extern void hew_bytes_clone_ref(unsigned char *data);
extern void hew_bytes_drop(unsigned char *data);

static unsigned char *observers[64];
static int32_t owners;
static int32_t expected;

static void require(int condition) {
  if (!condition) {
    fputs("extern byte ABI or ownership mismatch\n", stderr);
    abort();
  }
}

static void finish(void) {
  require(owners == expected);
  for (int32_t i = 0; i < owners; ++i) {
    /* The runtime bytes allocation has an eight-byte header beginning
     * with its atomic reference count. Our observer owns one reference;
     * every reference returned to Hew must have been released by now.
     * ASan also observes a duplicate release before this read. */
    const _Atomic uint32_t *count =
        (const _Atomic uint32_t *)(observers[i] - 8);
    require(atomic_load_explicit(count, memory_order_acquire) == 1);
    hew_bytes_drop(observers[i]);
    observers[i] = NULL;
  }
  printf("owned extern bytes released: %d\n", owners);
}

void oracle_expect(int32_t count) {
  expected = count;
  require(atexit(finish) == 0);
}

BytesTriple oracle_make(int32_t seed) {
  require(owners < 64);
  unsigned char *data = hew_bytes_new(8);
  require(data != NULL);
  /* Nonzero offset, distinct length, embedded NUL and a high-bit byte make
   * returning the wrong register packing observable. */
  const unsigned char contents[8] = {9,   8,  7, 'H', 0, (unsigned char)seed,
                                     255, 'w'};
  for (int i = 0; i < 8; ++i) {
    data[i] = contents[i];
  }
  hew_bytes_clone_ref(data);
  observers[owners++] = data;
  return (BytesTriple){data, 3, 5};
}

BytesTriple oracle_relay(int32_t before, const BytesTriple *value,
                         int32_t after, int64_t cookie) {
  require(before == 17 && after == 23 && cookie == INT64_C(0x123456789abcdef0));
  require(value->ptr != NULL && value->offset == 3 && value->len == 5);
  require(value->ptr[value->offset] == 'H' &&
          value->ptr[value->offset + 1] == 0);
  /* Consume and return the same owner without retaining or releasing it. */
  return *value;
}

BytesTriple oracle_empty(void) { return (BytesTriple){NULL, 0, 0}; }
