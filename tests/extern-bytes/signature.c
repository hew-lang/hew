/* No target headers: Clang can classify this C ABI without a cross sysroot. */
typedef struct {
  unsigned char *ptr;
  unsigned int offset;
  unsigned int len;
} BytesTriple;
extern BytesTriple oracle_make(int seed);
BytesTriple forward(int seed) { return oracle_make(seed); }

#include "aggregates.h"
OracleSmall forward_small(void) { return oracle_small(); }
OraclePacked forward_packed(void) { return oracle_packed(); }
OracleBig forward_big(int low, long long high) { return oracle_big(low, high); }
OracleMixed forward_mixed(void) { return oracle_mixed(); }
OracleReversed forward_reversed(void) { return oracle_reversed(); }
OracleFloatPair forward_float_pair(void) { return oracle_float_pair(); }
OracleFloatThree forward_float_three(void) { return oracle_float_three(); }
OracleNested forward_nested(void) { return oracle_nested(); }
OracleHfa forward_hfa(void) { return oracle_hfa(); }

OracleIntThree forward_int_three(void) { return oracle_int_three(); }
OracleTiny forward_tiny(void) { return oracle_tiny(); }
