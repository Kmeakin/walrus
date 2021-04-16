#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t Int;
typedef uint8_t Byte;
typedef uint32_t Char;
typedef float Float;
typedef bool Bool;
typedef void Never;

typedef struct {
  Int len;
  const Byte *const bytes;
} String;

typedef struct {
} Unit;
static_assert(sizeof(Unit) == 0, "Unit should occupy zero bytes");

const Byte TRUE_BYTES[] = "true";
const Byte FALSE_BYTES[] = "false";

const String STRING_TRUE = (String){.len = 4, .bytes = TRUE_BYTES};
const String STRING_FALSE = (String){.len = 5, .bytes = FALSE_BYTES};

const Unit UNIT = (Unit){};

Never builtin_exit(Int status) { exit(status); }

Unit builtin_print(String s) {
  fwrite(s.bytes, sizeof(Byte), s.len, stdout);
  return UNIT;
}

Unit builtin_print_error(String s) {
  fwrite(s.bytes, sizeof(Byte), s.len, stderr);
  return UNIT;
}

Int builtin_string_length(String s) { return s.len; }

String builtin_bool_to_string(Bool b) {
  if (b) {
    return STRING_TRUE;
  } else {
    return STRING_FALSE;
  }
}
String builtin_int_to_string(Int x) {
  Int len = snprintf(NULL, 0, "%d", x);
  Byte *bytes = malloc(len);
  snprintf(bytes, len, "%d", x);
  return (String){.len = len, .bytes = bytes};
}
String builtin_float_to_string(Float x) {
  Int len = snprintf(NULL, 0, "%f", x);
  Byte *bytes = malloc(len * sizeof(Byte));
  snprintf(bytes, len, "%f", x);
  return (String){.len = len, .bytes = bytes};
}
// taken from
// https://stackoverflow.com/questions/42012563/convert-unicode-code-points-to-utf-8-and-utf-32
String builtin_char_to_string(Char c) {
  if (c <= 0x7F) {
    char *bytes = malloc(1 * sizeof(Byte));
    bytes[0] = (Byte)c;
    return (String){.len = 1, .bytes = bytes};
  } else if (c <= 0x7FF) {
    char *bytes = malloc(2 * sizeof(Byte));
    bytes[0] = 0xC0 | (c >> 6);   // 110xxxxx
    bytes[1] = 0x80 | (c & 0x3F); // 10xxxxxx
    return (String){.len = 2, .bytes = bytes};
  } else if (c <= 0xFFFF) {
    char *bytes = malloc(3 * sizeof(Byte));
    bytes[0] = 0xE0 | (c >> 12);         // 1110xxxx
    bytes[1] = 0x80 | ((c >> 6) & 0x3F); // 10xxxxxx
    bytes[2] = 0x80 | (c & 0x3F);        // 10xxxxxx
    return (String){.len = 3, .bytes = bytes};
  } else {
    char *bytes = malloc(4 * sizeof(Byte));
    bytes[0] = 0xF0 | (c >> 18);          // 11110xxx
    bytes[1] = 0x80 | ((c >> 12) & 0x3F); // 10xxxxxx
    bytes[2] = 0x80 | ((c >> 6) & 0x3F);  // 10xxxxxx
    bytes[3] = 0x80 | (c & 0x3F);         // 10xxxxxx
    return (String){.len = 4, .bytes = bytes};
  }
}

String builtin_string_append(String s1, String s2) {
  Int len = s1.len + s2.len;
  Byte *bytes = malloc(len * sizeof(Byte));
  memcpy(bytes, s1.bytes, s1.len * sizeof(Byte));
  memcpy(bytes + s1.len, s2.bytes, s2.len * sizeof(Byte));
  return (String){.len = s1.len + s2.len, .bytes = bytes};
}
