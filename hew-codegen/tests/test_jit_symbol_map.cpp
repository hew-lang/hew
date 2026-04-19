#include "hew/generated_jit_stable_symbols.h"
#include "hew/jit_symbol_map.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <string_view>

int main() {
  hew::HewJitSymbolMap map;
  const auto &stableSymbols = hew::jit_detail::kStableJitHostSymbols;

  assert(map.stableSymbolCount() > 0);
  assert(map.stableSymbolCount() == stableSymbols.size());
  assert(std::is_sorted(stableSymbols.begin(), stableSymbols.end()));
  assert(map.hasStableSymbol("hew_actor_spawn"));
  assert(!map.hasStableSymbol("hew_sched_init"));
  assert(!map.hasStableSymbol("hew_runtime_cleanup"));
  assert(!map.hasStableSymbol("hew_shutdown_initiate"));
  assert(map.hasStableSymbol(stableSymbols.front()));
  assert(map.hasStableSymbol(stableSymbols[stableSymbols.size() / 2]));
  assert(map.hasStableSymbol(stableSymbols.back()));
  assert(!map.hasStableSymbol("hew_zzz_missing_symbol"));

  std::size_t visited = 0;
  map.forEachStableSymbol([&](std::string_view symbol) {
    ++visited;
    assert(symbol != "hew_sched_init");
    assert(symbol != "hew_runtime_cleanup");
    assert(symbol != "hew_shutdown_initiate");
  });

  assert(visited == map.stableSymbolCount());
  std::printf("loaded %zu stable JIT symbols\n", visited);
  return 0;
}
