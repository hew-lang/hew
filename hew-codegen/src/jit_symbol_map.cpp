#include "hew/jit_symbol_map.h"
#include "hew/generated_jit_stable_symbols.h"

#include <algorithm>

namespace hew {

HewJitSymbolMap::HewJitSymbolMap()
    : stableSymbols(jit_detail::kStableJitHostSymbols.begin(),
                    jit_detail::kStableJitHostSymbols.end()) {}

std::size_t HewJitSymbolMap::stableSymbolCount() const {
  return stableSymbols.size();
}

bool HewJitSymbolMap::hasStableSymbol(std::string_view name) const {
  return std::find(stableSymbols.begin(), stableSymbols.end(), name) != stableSymbols.end();
}

void HewJitSymbolMap::forEachStableSymbol(const StableSymbolVisitor &visitor) const {
  for (std::string_view symbol : stableSymbols) {
    visitor(symbol);
  }
}

} // namespace hew
