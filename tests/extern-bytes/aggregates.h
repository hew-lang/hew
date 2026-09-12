/* No target headers: these natural C layouts can be classified without a
 * sysroot. */
typedef struct {
  int a;
  int b;
} OracleSmall;
typedef struct {
  int a;
  int b;
  int c;
} OracleIntThree;
typedef struct {
  unsigned char a;
  unsigned char b;
  unsigned char c;
} OracleTiny;
typedef struct {
  long long a;
  int b;
  int c;
} OraclePacked;
typedef struct {
  long long a;
  long long b;
  long long c;
} OracleBig;
typedef struct {
  double value;
  int tag;
} OracleMixed;
typedef struct {
  int tag;
  double value;
} OracleReversed;
typedef struct {
  float a;
  float b;
} OracleFloatPair;
typedef struct {
  float a;
  float b;
  float c;
} OracleFloatThree;
typedef struct {
  OracleFloatPair first;
  OracleFloatPair second;
} OracleNested;
typedef struct {
  double a;
  double b;
  double c;
  double d;
} OracleHfa;

extern OracleSmall oracle_small(void);
extern OracleIntThree oracle_int_three(void);
extern OracleTiny oracle_tiny(void);
extern OraclePacked oracle_packed(void);
extern OracleBig oracle_big(int low, long long high);
extern OracleMixed oracle_mixed(void);
extern OracleReversed oracle_reversed(void);
extern OracleFloatPair oracle_float_pair(void);
extern OracleFloatThree oracle_float_three(void);
extern OracleNested oracle_nested(void);
extern OracleHfa oracle_hfa(void);
