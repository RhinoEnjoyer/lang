#pragma once
#include "../overloaded.hpp"
#include "../median_enum.hpp"
#include "../str_lit.hpp"
#include "../token.hpp"
#include "../token_str.hpp"

#include <cstdlib>
#include <cstring>

#include <array>
#include <iostream>
#include <map>
#include <span>
#include <type_traits>
#include <utility>
#include <array>
#include <variant>

namespace parser {
using cursor_t = podlist_t<token_t>::c_it;

struct node_t {
  using final_t = cursor_t;

  struct median_t {
    using code_t = medianc::e;
    code_t type_;
    std::int32_t len_;

    // somehow need a pointer or a reference to the next node or at least be
    // able to generate it

    auto type() const { return type_; }
    auto len() const { return len_; }
  };

  struct err_t {
    std::int32_t index_;
  };

  template <bool is_const = false> struct median_proxy_t {
    struct span_t {
      node_t *parent_;
      node_t *begin_;
      node_t *end_;

      size_t size_ = -1;

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
                     ptr->node_);
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
      auto at(size_t index) const -> auto {
        if (index >= size())
          throw std::out_of_range("Index out of range");
        return iterator(begin_ + index);
      }
      auto at2(size_t index) const -> auto {
        size_t len = 0;
        auto cursor = begin();
        while (cursor < end() && len < index) {
          ++len;
          cursor.advance();
        }
        return cursor;
        // if (index >= size())
        //   throw std::out_of_range("Index out of range");
        // return *(begin_ + index);
      }
      auto operator[](size_t index) const -> node_t & {
        return *(begin_ + index);
      }
      auto empty() const -> bool { return begin_ == end_; }
      auto size() const -> size_t { return end_ - begin_; }
      auto within(iterator val) const { return (val >= begin() && val < end()); }
      auto size2() -> size_t {
        if(size_ != -1)
          return size_;

        size_ = 0;
        auto cursor = begin();
        while (cursor < end()) {
          ++size_;
          cursor.advance();
        }
        return size_;
      }
      auto begin() const -> iterator { return iterator(begin_); }
      auto end() const -> iterator { return iterator(end_+1); }
      auto parent() const -> median_proxy_t<is_const> { return parent_->as_median(); }

      template<bool must_run = true, typename ...FNs>
      auto run(FNs... fns) const -> void{
        auto fn_arr = std::array{fns...};
        auto cursor = begin();
        for(auto i = 0; i < fn_arr.size(); i++){
          fn_arr[i](cursor);
          cursor.advance();
          if (cursor >= end()) {
            if constexpr (must_run)
              if (i != fn_arr.size() - 1)
                std::abort();
            break;
          }
        }
      }
    };

    template<typename T, typename = void>
    struct has_node : std::false_type {};

    template<typename T>
    struct has_node<T, std::void_t<decltype(std::declval<T>().node)>> : std::true_type {};

    // Modify the median_proxy_t constructor
    template <typename T>
    median_proxy_t(T &&val) : node{nullptr} {
        if constexpr (has_node<std::decay_t<T>>::value) {
            node = val.node;
        }
        // Additional initialization if needed
    }
    // median_proxy_t (): node(nullptr){
      
    // }
    median_proxy_t(node_t *val) : node(val) {}
    median_proxy_t(median_proxy_t<> val) : node(val.node) {}
    median_proxy_t(const node_t *val) : node(val) {}

    using node_type = std::conditional_t<is_const, const node_t, node_t>;
    node_type *node;
    auto type() const -> median_t::code_t {return node->as_raw_median().type_;}
    auto len() const -> size_t { return children().size(); }
    auto children() const -> span_t {
      node_type* p = node;
      node_type* b = node + 1;

      auto plus = node->as_raw_median().len_;
      node_type* e = node + plus;
      return span_t{p, b, e};
    }

    median_proxy_t(const auto &val) {
      // Logic to initialize from `val`
      node = val.node; // Assuming `val` has a `node` member
    }
  };

  using var_t = std::variant<final_t, median_t, err_t>;
  parser::node_t::var_t node_;

  [[nodiscard]] auto as_median() -> median_proxy_t<false> { return {this}; }
  [[nodiscard]] auto as_median() const -> const median_proxy_t<true> {
    return {this};
  }

  [[nodiscard]] auto as_raw_median() -> median_t & {
    return std::get<median_t>(node_);
  }
  [[nodiscard]] auto as_raw_median() const -> const median_t & {
    return std::get<median_t>(node_);
  }

  [[nodiscard]] auto as_final() -> final_t & { return std::get<final_t>(node_); }
  [[nodiscard]] auto as_final() const -> const final_t & {
    return std::get<final_t>(node_);
  }

  [[nodiscard]] auto as_err() -> err_t & { return std::get<err_t>(node_); }
  [[nodiscard]] auto as_err() const -> const err_t & {
    return std::get<err_t>(node_);
  }

  using ext_node_var = std::variant<std::monostate,final_t, median_proxy_t<>, err_t>;
  struct external_node : public ext_node_var {
    using ext_node_var::variant;
    external_node() : ext_node_var(std::monostate{}) {}
  };

  auto node() -> external_node {
    return std::visit(
        overloaded{[&](const median_t &val) -> external_node {
                     return this->as_median();
                   },
                   [&](const auto &val) -> external_node { return val; }},
        this->node_);
  }

  template <typename T> [[nodiscard]] auto as() -> auto {
    if constexpr (std::is_same_v<T, median_t> ||
                  std::is_same_v<T, median_proxy_t<>>) {
      return as_median();
    } else if constexpr (std::is_same_v<T, err_t>) {
      return as_err();
    } else {
      static_assert(std::is_same_v<T, final_t>, "Unsupported type for as()");
      return as_final();
    }
  }

  [[nodiscard]] auto is_median() const noexcept -> bool {
    return std::holds_alternative<median_t>(node_);
  }
  [[nodiscard]] auto is_final() const noexcept -> bool {
    return std::holds_alternative<final_t>(node_);
  }
  [[nodiscard]] auto is_err() const noexcept -> bool {
    return std::holds_alternative<err_t>(node_);
  }

  // template <typename T> [[nodiscard]] auto as_checked() -> auto {
  //   if (!std::holds_alternative<T>(node_))
  //     throw std::runtime_error("Node does not hold the expected type");
  //   return this->as<T>();
  // }

  // template <typename T> [[nodiscard]] auto as_checked() const -> const auto {
  //   return as_checked<T>();
  // }

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
