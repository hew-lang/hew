#pragma once

#include <cstddef>
#include <functional>
#include <string_view>
#include <vector>

namespace hew {

class HewJitSymbolMap {
public:
  using StableSymbolVisitor = std::function<void(std::string_view)>;

  HewJitSymbolMap();

  [[nodiscard]] std::size_t stableSymbolCount() const;
  [[nodiscard]] bool hasStableSymbol(std::string_view name) const;
  void forEachStableSymbol(const StableSymbolVisitor &visitor) const;

private:
  std::vector<std::string_view> stableSymbols;
};

} // namespace hew
