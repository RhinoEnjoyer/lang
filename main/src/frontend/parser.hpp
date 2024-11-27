#pragma once
#include "../token.hpp"
#include "../token_str.hpp"
#include "../str_lit.hpp"
#include "../median_enum.hpp"

#include <cstdlib>
#include <cstring>

#include <map>
#include <array>
#include <iostream>
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

  using node_var_t = std::variant<final_t, median_t, err_t>;
  node_var_t node;

  [[nodiscard]] auto as_median() -> median_t& { return std::get<median_t>(node); }
  [[nodiscard]] auto as_final() -> final_t& { return std::get<final_t>(node); }
  [[nodiscard]] auto as_err() -> err_t& { return std::get<err_t>(node); }

  [[nodiscard]] auto as_median() const -> const median_t& { return std::get<median_t>(node); }
  [[nodiscard]] auto as_final() const -> const final_t& { return std::get<final_t>(node); }
  [[nodiscard]] auto as_err() const -> const err_t& { return std::get<err_t>(node); }

  [[nodiscard]] auto is_median() const -> bool { return std::holds_alternative<median_t>(node); }
  [[nodiscard]] auto is_final() const -> bool { return std::holds_alternative<final_t>(node); }
  [[nodiscard]] auto is_err() const -> bool { return std::holds_alternative<err_t>(node); }

  [[nodiscard]] static auto make(cursor_t cursor) -> node_t { return node_t{final_t(cursor)}; }
  [[nodiscard]] static auto make(median_t::code_t type, std::int32_t len) -> node_t { return node_t{median_t{type, len}}; }
};


////////
// struct vardecl_t{};
// struct typedecl_t{};
// using val = std::variant<vardecl_t, typedecl_t>;
// struct rec_map; 
// using map_val = std::variant<val, std::map<std::string, rec_map>>;
// struct rec_map {map_val value;};
////////

auto entry(const token_buffer_t &toks, cursor_t cursor, const cursor_t end) -> podlist_t<node_t>;
auto traverse(const token_buffer_t &buf, podlist_t<node_t> &buffer) -> void;

} // namespace parser
