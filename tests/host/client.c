#include "config_policy.h"
#include "hew_host.h"
#include "take_policy.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void text_is(const HewText *text, const unsigned char *expected,
                    size_t size) {
  size_t len = 99;
  const unsigned char *bytes = hew_host_text_data(text, &len);
  assert(text && bytes && len == size);
  assert(memcmp(bytes, expected, size) == 0);
  if (!size)
    assert(*bytes == 0);
}
static void error_is(HewError *error, int32_t code) {
  size_t len = 0;
  assert(error && hew_host_error_code(error) == code);
  assert(hew_host_error_message(error, &len) && len);
}
static HewJson *parse(const char *input) {
  HewJson *value = NULL;
  HewError *error = NULL;
  assert(hew_host_json_parse((const unsigned char *)input, strlen(input),
                             &value, &error) == HEW_OK);
  assert(value && !error);
  return value;
}
static void json_is(HewJson *value, const char *expected) {
  HewText *text = NULL;
  HewError *error = NULL;
  assert(hew_host_json_encode(value, &text, &error) == HEW_OK && !error);
  text_is(text, (const unsigned char *)expected, strlen(expected));
  hew_host_text_release(text);
}
static void compiled_policy(void) {
  static const unsigned char key[] = "label";
  static const unsigned char initial[] = "  caf\xc3\xa9\0\xe9\x9b\xaa  ";
  static const unsigned char expected[] = "caf\xc3\xa9\0\xe9\x9b\xaa";
  HewJson *original = parse("{\"label\":\"  caf\\u00e9\\u0000\\u96ea  \"}");
  HewJson *copy = NULL;
  HewText *label = NULL, *normalized = NULL, *unchanged = NULL, *taken = NULL;
  HewError *error = NULL;
  assert(hew_host_json_copy(original, &copy, &error) == HEW_OK && !error);
  assert(hew_host_json_get_text(copy, key, sizeof key - 1, &label, &error) ==
             HEW_OK &&
         !error);
  assert(config_normalize_label(label, &normalized, &error) == HEW_OK &&
         !error);
  text_is(label, initial, sizeof initial - 1);
  assert(config_take_label(label, &taken, &error) == HEW_OK && !error);
  hew_host_text_release(label);
  text_is(taken, initial, sizeof initial - 1);
  hew_host_text_release(taken);
  assert(hew_host_json_set_text(copy, key, sizeof key - 1, normalized,
                                &error) == HEW_OK &&
         !error);
  hew_host_text_release(normalized);
  normalized = NULL;
  assert(hew_host_json_get_text(original, key, sizeof key - 1, &unchanged,
                                &error) == HEW_OK &&
         !error);
  assert(hew_host_json_get_text(copy, key, sizeof key - 1, &normalized,
                                &error) == HEW_OK &&
         !error);
  hew_host_json_release(original);
  hew_host_json_release(copy);
  text_is(unchanged, initial, sizeof initial - 1);
  text_is(normalized, expected, sizeof expected - 1);
  hew_host_text_release(unchanged);
  hew_host_text_release(normalized);

  for (int attempt = 0; attempt < 2; ++attempt) {
    static const unsigned char rejected[] = "reject: caf\xc3\xa9\0tail";
    static const unsigned char diagnostic[] =
        "hew: failure: UserPanic (212): REJECT: CAF\xc3\x89\0TAIL\n";
    static const unsigned char consumed_diagnostic[] =
        "hew: failure: UserPanic (212): reject: caf\xc3\xa9\0tail\n";
    const unsigned char *expected_error =
        attempt ? consumed_diagnostic : diagnostic;
    const size_t expected_len =
        attempt ? sizeof consumed_diagnostic - 1 : sizeof diagnostic - 1;
    size_t len = 0;
    label = NULL;
    normalized = NULL;
    assert(hew_host_text_from_utf8(rejected, sizeof rejected - 1, &label,
                                   &error) == HEW_OK &&
           !error);
    assert((attempt ? config_take_label(label, &normalized, &error)
                    : config_normalize_label(label, &normalized, &error)) ==
           HEW_LOGICAL_FAULT);
    assert(!normalized);
    error_is(error, HEW_LOGICAL_FAULT);
    text_is(label, rejected, sizeof rejected - 1);
    hew_host_text_release(label);
    label = NULL;
    for (int inspect = 0; inspect < 2; ++inspect) {
      const unsigned char *message = hew_host_error_message(error, &len);
      assert(len == expected_len && memcmp(message, expected_error, len) == 0);
    }
    HewError *next_error = NULL;
    assert(hew_host_text_from_utf8(NULL, 0, &label, &next_error) == HEW_OK &&
           !next_error);
    assert(config_normalize_label(label, &normalized, &next_error) == HEW_OK &&
           !next_error);
    hew_host_text_release(label);
    text_is(normalized, (const unsigned char *)"", 0);
    hew_host_text_release(normalized);
    const unsigned char *message = hew_host_error_message(error, &len);
    assert(len == expected_len && memcmp(message, expected_error, len) == 0);
    hew_host_error_release(error);
    error = NULL;
  }
}

int main(void) {
  compiled_policy();
  HewError *error = NULL, *next_error = NULL;
  HewText *text = NULL, *extracted = NULL;
  HewJson *bad = NULL;
  const unsigned char invalid[] = {0xff};
  unsigned char payload[] = {'A', 0, 0xc3, 0xa9, 0xe9, 0x9b, 0xaa};
  const unsigned char expected[] = {'A', 0, 0xc3, 0xa9, 0xe9, 0x9b, 0xaa};
  const unsigned char key[] = {'k', 0, 'x'};
  size_t len = 0;
  const unsigned char *message = NULL;
  unsigned char *saved = NULL;

  assert(hew_host_text_from_utf8(NULL, 0, &text, &error) == HEW_OK && !error);
  text_is(text, (const unsigned char *)"", 0);
  hew_host_text_release(text);
  text = NULL;
  assert(hew_host_text_from_utf8(invalid, sizeof invalid, &text, &error) ==
         HEW_INVALID_UTF8);
  assert(!text);
  error_is(error, HEW_INVALID_UTF8);
  message = hew_host_error_message(error, &len);
  saved = (unsigned char *)malloc(len);
  assert(saved);
  memcpy(saved, message, len);
  assert(hew_host_text_from_utf8(payload, sizeof payload, &text, &next_error) ==
             HEW_OK &&
         !next_error);
  memset(payload, 'x', sizeof payload);
  text_is(text, expected, sizeof expected);
  assert(memcmp(hew_host_error_message(error, &len), saved, len) == 0);
  free(saved);
  hew_host_error_release(error);
  error = NULL;

  for (int release_copy_first = 0; release_copy_first < 2;
       ++release_copy_first) {
    HewJson *original =
        parse("{\"label\":\"original\",\"empty\":\"\",\"null\":null}");
    HewJson *copy = NULL;
    assert(hew_host_json_copy(original, &copy, &error) == HEW_OK && !error);
    assert(hew_host_json_set_text(copy, key, sizeof key, text, &error) ==
               HEW_OK &&
           !error);
    assert(hew_host_json_get_text(copy, key, sizeof key, &extracted, &error) ==
               HEW_OK &&
           !error);
    {
      HewText *encoded = NULL, *round_text = NULL;
      HewJson *round_trip = NULL;
      size_t encoded_len = 0;
      const unsigned char *encoded_bytes = NULL;
      assert(hew_host_json_encode(copy, &encoded, &error) == HEW_OK && !error);
      encoded_bytes = hew_host_text_data(encoded, &encoded_len);
      assert(hew_host_json_parse(encoded_bytes, encoded_len, &round_trip,
                                 &error) == HEW_OK &&
             !error);
      hew_host_text_release(encoded);
      assert(hew_host_json_get_text(round_trip, key, sizeof key, &round_text,
                                    &error) == HEW_OK &&
             !error);
      hew_host_json_release(round_trip);
      text_is(round_text, expected, sizeof expected);
      hew_host_text_release(round_text);
    }
    assert(hew_host_json_set_text(original, invalid, sizeof invalid, text,
                                  &error) == HEW_INVALID_UTF8);
    error_is(error, HEW_INVALID_UTF8);
    hew_host_error_release(error);
    error = NULL;
    if (release_copy_first) {
      hew_host_json_release(copy);
      copy = NULL;
    } else {
      hew_host_json_release(original);
      original = NULL;
    }
    text_is(extracted, expected, sizeof expected);
    hew_host_text_release(extracted);
    extracted = NULL;
    if (original) {
      assert(hew_host_json_get_text(original, key, sizeof key, &extracted,
                                    &error) == HEW_MISSING_FIELD);
      assert(!extracted);
      error_is(error, HEW_MISSING_FIELD);
      hew_host_error_release(error);
      error = NULL;
      assert(hew_host_json_get_text(original, (const unsigned char *)"empty", 5,
                                    &extracted, &error) == HEW_OK);
      assert(!error);
      text_is(extracted, (const unsigned char *)"", 0);
      hew_host_text_release(extracted);
      extracted = NULL;
      assert(hew_host_json_get_text(original, (const unsigned char *)"null", 4,
                                    &extracted, &error) == HEW_WRONG_KIND);
      assert(!extracted);
      error_is(error, HEW_WRONG_KIND);
      hew_host_error_release(error);
      error = NULL;
    }
    hew_host_json_release(original);
    hew_host_json_release(copy);
  }
  {
    HewJson *array = parse("[1]");
    assert(hew_host_json_set_text(array, key, sizeof key, text, &error) ==
           HEW_WRONG_KIND);
    error_is(error, HEW_WRONG_KIND);
    json_is(array, "[1]");
    hew_host_error_release(error);
    error = NULL;
    hew_host_json_release(array);
  }
  hew_host_text_release(text);
  text = NULL;
  assert(hew_host_json_parse(invalid, sizeof invalid, &bad, &error) ==
         HEW_INVALID_UTF8);
  assert(!bad);
  error_is(error, HEW_INVALID_UTF8);
  hew_host_error_release(error);
  error = NULL;
  assert(hew_host_json_parse((const unsigned char *)"{", 1, &bad, &error) ==
         HEW_INVALID_JSON);
  assert(!bad);
  error_is(error, HEW_INVALID_JSON);
  message = hew_host_error_message(error, &len);
  saved = (unsigned char *)malloc(len);
  assert(saved);
  memcpy(saved, message, len);
  {
    HewJson *later = parse("{\"ok\":true}");
    json_is(later, "{\"ok\":true}");
    hew_host_json_release(later);
  }
  assert(memcmp(hew_host_error_message(error, &len), saved, len) == 0);
#ifdef __linux__
  {
    FILE *sink = fopen("/dev/full", "wb");
    assert(sink && setvbuf(sink, NULL, _IONBF, 0) == 0);
    assert(fwrite(message, 1, len, sink) < len);
    (void)fclose(sink);
    assert(memcmp(hew_host_error_message(error, &len), saved, len) == 0);
  }
#endif
  free(saved);
  hew_host_error_release(error);
  hew_host_text_release(NULL);
  hew_host_json_release(NULL);
  hew_host_error_release(NULL);
  puts("host client: compiled Hew, independent values and owned errors OK");
  return 0;
}
