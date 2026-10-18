#define _POSIX_C_SOURCE 200809L
#include <inttypes.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

/* Keep each batch observable to the optimizer without volatile element access. */
static void observe(const void *value) {
#if defined(__GNUC__) || defined(__clang__)
    __asm__ volatile("" : : "g"(value) : "memory");
#else
    (void)value;
    atomic_signal_fence(memory_order_seq_cst);
#endif
}

static int64_t now_ns(void) {
#ifdef _WIN32
    LARGE_INTEGER count, frequency;
    QueryPerformanceCounter(&count);
    QueryPerformanceFrequency(&frequency);
    return (count.QuadPart / frequency.QuadPart) * 1000000000LL +
           (count.QuadPart % frequency.QuadPart) * 1000000000LL / frequency.QuadPart;
#else
    struct timespec time;
    if (clock_gettime(CLOCK_MONOTONIC, &time) != 0) {
        abort();
    }
    return (int64_t)time.tv_sec * 1000000000LL + time.tv_nsec;
#endif
}

static size_t positive(const char *text) {
    char *end;
    uintmax_t value = strtoumax(text, &end, 10);
    if (*text == '-' || text == end || *end != '\0' || value == 0 || value > SIZE_MAX) {
        fprintf(stderr, "iterations and size must be positive integers\n");
        exit(1);
    }
    return (size_t)value;
}

static void *allocate(size_t size) {
    void *value = malloc(size);
    if (value == NULL) {
        abort();
    }
    return value;
}

int main(int argc, char **argv) {
    if (argc != 4) {
        fprintf(stderr, "usage: primitives WORKLOAD ITERATIONS SIZE\n");
        return 1;
    }
    const char *workload = argv[1];
    size_t iterations = positive(argv[2]), size = positive(argv[3]);
    if (size > SIZE_MAX / sizeof(int64_t)) {
        return 1;
    }
    int64_t *values = NULL;
    uint8_t *original = NULL;
    if (strcmp(workload, "vector_scan") == 0) {
        values = allocate(size * sizeof(*values));
        for (size_t i = 0; i < size; ++i) {
            values[i] = (int64_t)(i % 251);
        }
    }
    if (strncmp(workload, "bytes_", 6) == 0) {
        original = allocate(size);
        for (size_t i = 0; i < size; ++i) {
            original[i] = (uint8_t)(i % 251);
        }
    }
    atomic_size_t references = 1;
    int64_t start = now_ns(), checksum = 0;
    if (strcmp(workload, "scalar") == 0) {
        checksum = 1;
        for (size_t r = 0; r < iterations; ++r) {
            checksum = (checksum * 48271) % 2147483647;
        }
    } else if (strcmp(workload, "vector_scan") == 0) {
        for (size_t r = 0; r < iterations; ++r) {
            observe(values);
            for (size_t i = 0; i < size; ++i) {
                checksum += values[i];
            }
        }
    } else if (strcmp(workload, "vector_build") == 0) {
        for (size_t r = 0; r < iterations; ++r) {
            size_t capacity = 0;
            int64_t *built = NULL;
            for (size_t i = 0; i < size; ++i) {
                if (i == capacity) {
                    capacity = capacity ? capacity * 2 : 4;
                    int64_t *grown = realloc(built, capacity * sizeof(*built));
                    if (grown == NULL) {
                        abort();
                    }
                    built = grown;
                }
                built[i] = (int64_t)(i % 251);
            }
            observe(built);
            for (size_t i = 0; i < size; ++i) {
                checksum += built[i];
            }
            free(built);
        }
    } else if (strcmp(workload, "bytes_shared") == 0) {
        for (size_t r = 0; r < iterations; ++r) {
            atomic_fetch_add_explicit(&references, 1, memory_order_relaxed);
            const uint8_t *snapshot = original;
            observe(snapshot);
            checksum += snapshot[r % size];
            atomic_fetch_sub_explicit(&references, 1, memory_order_release);
        }
    } else if (strcmp(workload, "bytes_cow") == 0) {
        for (size_t r = 0; r < iterations; ++r) {
            uint8_t *snapshot = allocate(size);
            memcpy(snapshot, original, size);
            size_t index = r % size;
            snapshot[index] = (uint8_t)((original[index] + 1) % 251);
            observe(snapshot);
            checksum += snapshot[index] + original[index];
            free(snapshot);
        }
    } else {
        fprintf(stderr, "unknown workload\n");
        return 1;
    }
    int64_t elapsed = now_ns() - start;
    printf("{\"workload\":\"%s\",\"iterations\":%zu,\"size\":%zu,\"elapsed_ns\":%" PRId64 ",\"checksum\":%" PRId64 "}\n",
           workload, iterations, size, elapsed, checksum);
    free(original);
    free(values);
    return 0;
}
