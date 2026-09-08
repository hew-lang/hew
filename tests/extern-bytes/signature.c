/* No target headers: Clang can classify this C ABI without a cross sysroot. */
typedef struct {
  unsigned char *ptr;
  unsigned int offset;
  unsigned int len;
} BytesTriple;
extern BytesTriple oracle_make(int seed);
BytesTriple forward(int seed) { return oracle_make(seed); }
