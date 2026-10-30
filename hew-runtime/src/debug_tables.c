/* Weak references to the tables the compiler emits into every native Hew
 * image (`hew-debug-abi`). Stable Rust cannot declare an extern_weak symbol,
 * so this shim resolves each table to its address, or to null in an image
 * without tables: a Rust test binary that links the runtime, or an object
 * built before the tables existed. */

#if defined(_MSC_VER)
/* MSVC has no weak undefined symbols. `/alternatename` binds each table to a
 * zeroed stand-in when the program object does not define it; its zero magic
 * reads as absent. */
const unsigned char hew_absent_debug_table[16] = {0};
#pragma comment(linker, "/alternatename:__hew_file_table=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_site_table=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_symtab=hew_absent_debug_table")
#pragma comment(linker, "/alternatename:__hew_has_debug_info=hew_absent_debug_table")
#define HEW_WEAK
#else
#define HEW_WEAK __attribute__((weak))
#endif

extern const unsigned char __hew_file_table[] HEW_WEAK;
extern const unsigned char __hew_site_table[] HEW_WEAK;
extern const unsigned char __hew_symtab[] HEW_WEAK;
extern const unsigned char __hew_has_debug_info[] HEW_WEAK;

const unsigned char *hew_debug_file_table(void) { return __hew_file_table; }
const unsigned char *hew_debug_site_table(void) { return __hew_site_table; }
const unsigned char *hew_debug_symtab(void) { return __hew_symtab; }
const unsigned char *hew_debug_has_debug_info(void) { return __hew_has_debug_info; }
