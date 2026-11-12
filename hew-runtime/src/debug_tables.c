/* Default definitions of the tables the compiler emits into every native Hew
 * image (`hew-debug-abi`). Stable Rust cannot declare an extern_weak symbol,
 * and Mach-O refuses a weak reference nothing defines, so this shim gives
 * each table a zeroed stand-in the program's own table replaces at link time.
 * An image without tables (a Rust test binary that links the runtime, an
 * object built before the tables existed) keeps the stand-in, whose zero
 * magic reads as absent. */

#if defined(_MSC_VER)
/* MSVC has no weak definitions; `/alternatename` binds each table to the
 * stand-in when the program object does not define it. */
const unsigned char hew_absent_debug_table[16] = {0};
#pragma comment(linker, "/alternatename:__hew_file_table=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_site_table=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_symtab=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_has_debug_info=hew_absent_debug_table")
extern const unsigned char __hew_file_table[];
extern const unsigned char __hew_site_table[];
extern const unsigned char __hew_symtab[];
extern const unsigned char __hew_has_debug_info[];
#else
__attribute__((weak)) const unsigned char __hew_file_table[16] = {0};
__attribute__((weak)) const unsigned char __hew_site_table[16] = {0};
__attribute__((weak)) const unsigned char __hew_symtab[16] = {0};
__attribute__((weak)) const unsigned char __hew_has_debug_info[1] = {0};
#endif

const unsigned char *hew_debug_file_table(void) { return __hew_file_table; }
const unsigned char *hew_debug_site_table(void) { return __hew_site_table; }
const unsigned char *hew_debug_symtab(void) { return __hew_symtab; }
const unsigned char *hew_debug_has_debug_info(void) { return __hew_has_debug_info; }
