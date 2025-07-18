#pragma once

#include "../nicknames.hpp"
#include "./parser.hpp"

namespace grammar {
struct cursor_helper_t {
  using node_t = grammar::node_t::external_node;
  using span_t = grammar::node_t::median_proxy_t<false>::span_t;
  using cursor_t = span_t::iterator;
  using median_t = grammar::node_t::median_proxy_t<false>;
  using final_t = grammar::node_t::final_t;
  using err_t = grammar::node_t::err_t;

  cursor_helper_t(span_t s) : span_(s), cursor_(s.begin()) {}

  template <auto in = medianc::any, typename in_t = decltype(in),
            typename t = std::conditional_t<std::is_same_v<in_t, medianc::e>,
                                            median_t, final_t>>
  opt<t> extract() {
    static_assert(std::is_same_v<in_t, medianc::e> ||
                      std::is_same_v<in_t, tokc::e>,
                  "in must be a medianc::e or a tokc::e");

    if (!within() || !std::holds_alternative<t>(cursor_->node()))
      return std::nullopt;

    if constexpr (std::is_same_v<t, median_t>) {
      const auto med = cursor_->as_median();
      if constexpr (in == medianc::any) {
        cursor_.advance();
        return med;
      } else {
        if (med.type() == in) {
          cursor_.advance();
          return med;
        }
      }
    } else if constexpr (std::is_same_v<t, final_t>) {
      const auto fin = cursor_->as_final();
      if constexpr (in == tokc::any) {
        cursor_.advance();
        return fin;
      } else {
        if (fin->isa(in)) {
          cursor_.advance();
          return fin;
        }
      }
    } else {
      static_assert(false, "input is neither a medianc::e or a tokc::e");
    }
    return std::nullopt;
  }

  template <size_t index, size_t max, auto in, auto... ins>
  auto tuple_extract_impl(auto &tup) -> void {
    if constexpr (index < max) {
      std::get<index>(tup) = extract<in>();
      if constexpr (sizeof...(ins) > 0) {
        return tuple_extract_impl<index + 1, max, ins...>(tup);
      }
    }
  }
  template <
      auto... ins,
      typename ret = std::tuple<std::optional<std::conditional_t<
          std::is_same_v<decltype(ins), medianc::e>, median_t, final_t>>...>>
  auto tuple_extract() -> ret {
    ret tup;
    tuple_extract_impl<0, sizeof...(ins), ins...>(tup);
    return tup;
  }
  template <auto in = medianc::any, typename in_t = decltype(in),
            typename t = std::conditional_t<std::is_same_v<in_t, medianc::e>,
                                            median_t, final_t>>
  auto must_extract() -> t {
    auto res = extract<in>();
    if (!res)
      throw std::runtime_error("Expected element not found");
    return *res;
  }

  bool within() const { return span_.contains(cursor_); }

private:
  span_t span_;
  cursor_t cursor_;
};
} // namespace grammar
