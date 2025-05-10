#pragma once

#include <variant>
#include <utility>

template <class... Ts>
struct overloaded: Ts... {
    using Ts::operator()...;
};

template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template <typename Variant, typename... Visitors>
auto ovisit(Variant &&variant, Visitors ...visitors) {
  return std::visit(overloaded{std::forward<Visitors>(visitors)...},
                    std::forward<Variant>(variant));
}
template <typename Variant, typename... Visitors>
auto ovisit(const Variant &&variant, Visitors ...visitors) {
  return std::visit(overloaded{std::forward<Visitors>(visitors)...},
                    std::forward<Variant>(variant));
}
