#pragma once
#include "../overloaded.hpp"
#include "../median_enum.hpp"
#include "../str_lit.hpp"
#include "../token.hpp"
#include "../token_str.hpp"
#include <boost/container/flat_map.hpp>

#include <cstdlib>
#include <cstring>

#include <array>
#include <iostream>
#include <map>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>

namespace parser {
using cursor_t = podlist_t<token_t>::c_it;

struct node_t {
  using final_t = cursor_t;

  struct median_t {
    using code_t = medianc::e;

    code_t type_;
    std::int32_t len_;
  };

  struct err_t {
    std::int32_t index_;
  };

  template <bool is_const = false> struct median_proxy_t {
    struct span_t {
      node_t *const parent_;
      node_t *const begin_;
      node_t *const end_;

      struct iterator {
        using difference_type = std::ptrdiff_t;
        using value_type = node_t;
        using pointer = node_t *;
        using reference = node_t &;
        using iterator_category = std::forward_iterator_tag;

        pointer ptr;

        iterator(pointer p) : ptr(p) {}

        auto operator*() const -> reference { return *ptr; }
        auto operator->() const -> pointer { return ptr; }

        auto next() const -> iterator {
          return ptr +
                 std::visit(
                     overloaded{[&](const final_t &val) -> size_t { return 1; },
                                [&](const median_t &val) -> size_t {
                                  // one because the iterator is one behind
                                  // one because that would take us to the last
                                  // element and we want the next element
                                  return ptr->as_median().len() + 2;
                                },
                                [&](const err_t &val) -> size_t { return 1; }},
                     ptr->node);
        }

        auto advance() -> iterator {
          *this = next();
          return *this;
        }

        auto operator++() -> iterator & {
          advance();
          return *this;
        }

        auto operator++(int) -> iterator {
          iterator temp = *this;
          advance();
          return temp;
        }

        auto operator==(const iterator &other) const -> bool {
          return ptr == other.ptr;
        }
        auto operator!=(const iterator &other) const -> bool {
          return ptr != other.ptr;
        }
        auto operator<(const iterator &other) const -> bool {
          return ptr < other.ptr;
        }
        auto operator>(const iterator &other) const -> bool {
          return ptr > other.ptr;
        }
        auto operator<=(const iterator &other) const -> bool {
          return ptr <= other.ptr;
        }
        auto operator>=(const iterator &other) const -> bool {
          return ptr >= other.ptr;
        }
      };
      auto subspan(size_t offset, size_t count) const -> span_t {
        if (offset + count > size())
          throw std::out_of_range("Subspan out of range");
        return span_t{begin_ + offset, begin_ + offset + count};
      }
      auto at(size_t index) const -> node_t & {
        if (index >= size())
          throw std::out_of_range("Index out of range");
        return *(begin_ + index);
      }
      auto operator[](size_t index) const -> node_t & {
        return *(begin_ + index);
      }
      auto empty() const -> bool { return begin_ == end_; }
      auto size() const -> size_t { return end_ - begin_; }
      auto begin() const -> iterator { return iterator(begin_); }
      auto end() const -> iterator { return iterator(end_+1); }
      auto parent() const -> median_proxy_t<is_const> { return parent_->as_median(); }
    };

    using node_type = std::conditional_t<is_const, const node_t, node_t>;
    node_type *const node;
    auto type() const -> median_t::code_t {return node->as_raw_median().type_;}
    auto len() const -> size_t { return children().size(); }
    auto children() const -> span_t {

      return span_t(node, node + 1, node + node->as_raw_median().len_);
    }
    // auto children() const -> std::span<node_t> {
    //   return std::span<node_t>(reinterpret_cast<node_t *const>(node + 1),
    //                            node->as_raw_median().len_);
    // }
  };

  using var_t = std::variant<final_t, median_t, err_t>;
  parser::node_t::var_t node;

  [[nodiscard]] auto as_median() -> median_proxy_t<false> { return {this}; }
  [[nodiscard]] auto as_median() const -> const median_proxy_t<true> {
    return {this};
  }

  [[nodiscard]] auto as_raw_median() -> median_t & {
    return std::get<median_t>(node);
  }
  [[nodiscard]] auto as_raw_median() const -> const median_t & {
    return std::get<median_t>(node);
  }

  [[nodiscard]] auto as_final() -> final_t & { return std::get<final_t>(node); }
  [[nodiscard]] auto as_final() const -> const final_t & {
    return std::get<final_t>(node);
  }

  [[nodiscard]] auto as_err() -> err_t & { return std::get<err_t>(node); }
  [[nodiscard]] auto as_err() const -> const err_t & {
    return std::get<err_t>(node);
  }

  template <typename T> [[nodiscard]] auto as() const -> auto {
    if constexpr (std::is_same_v<T, median_t>) {
      return as_median();
    } else if constexpr (std::is_same_v<T, err_t>) {
      return as_err();
    } else {
      static_assert(std::is_same_v<T, final_t>, "Unsupported type for as()");
      return as_final();
    }
  }

  [[nodiscard]] auto is_median() const noexcept -> bool {
    return std::holds_alternative<median_t>(node);
  }
  [[nodiscard]] auto is_final() const noexcept -> bool {
    return std::holds_alternative<final_t>(node);
  }
  [[nodiscard]] auto is_err() const noexcept -> bool {
    return std::holds_alternative<err_t>(node);
  }

  template <typename T> [[nodiscard]] auto as_checked() -> auto {
    if (!std::holds_alternative<T>(node))
      throw std::runtime_error("Node does not hold the expected type");
    return this->as<T>();
  }

  template <typename T> [[nodiscard]] auto as_checked() const -> const auto {
    return as_checked<T>();
  }

  [[nodiscard]] static auto make(cursor_t cursor) -> node_t {
    return node_t{final_t(cursor)};
  }
  [[nodiscard]] static auto make(median_t::code_t type,
                                 std::int32_t len) -> node_t {
    return node_t{median_t{type, len}};
  }

};

using node_buffer_t = podlist_t<node_t>;
using parser_out = podlist_t<node_t>;


auto entry(const token_buffer_t &toks, cursor_t cursor,
           const cursor_t end) -> parser_out;

auto traverse(const token_buffer_t &buf, podlist_t<node_t> &buffer) -> void;
} // namespace parser
