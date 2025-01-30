#pragma once

#include "../nicknames.hpp"
#include "../overloaded.hpp"
#include "../table.hpp"
#include "./parser.hpp"

#include <cmath>
#include <llvm/Support/FileSystem.h>
#include <memory>
#include <print>
#include <string_view>
#include <typeindex>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>





namespace semantics {


struct decl_t{
  
};

struct depth_inh {
  size_t depth = 0;
  template <typename Fn, typename... T> auto dive(Fn fn, T... args) {
    depth += 1;
    std::invoke(std::forward<Fn>(fn), std::forward<T>(args)...);
    depth -= 1;
  }
};

struct decl_index_inh {
  size_t decl_index = 0;
  auto increment() -> size_t { return decl_index++; }
};

struct locale_t;
struct context_t;

struct type_t;
struct expr_t;
struct decl_t;

using node_t = parser::node_t::external_node;
using span_t = parser::node_t::median_proxy_t<false>::span_t;
using cursor_t = span_t::iterator;

using median_t = parser::node_t::median_proxy_t<false>;
using final_t = parser::node_t::final_t;
using err_t = parser::node_t::err_t;

// thanks chat gpt
struct unresolved_symbol_t {
  sptr<locale_t> request_scope; // Scope where the symbol was needed
  std::string_view symbol_name; // Name of the unresolved symbol
  sptr<decl_t>
      requestee; // The declaration or expression that depends on this symbol
  std::size_t requested_type; // Type information or purpose of the request

  unresolved_symbol_t(sptr<locale_t> scope, std::string_view name,
                      sptr<decl_t> req, std::size_t type)
      : request_scope(scope), symbol_name(name), requestee(req),
        requested_type(type) {}
};

using final_fn = std::function<void(context_t &, final_t &)>;
using median_fn = std::function<void(context_t &, median_t &)>;
using err_fn = std::function<void(context_t &, err_t &)>;
using node_fn = std::function<void(context_t &, node_t &)>;

template <typename F> node_fn wrap_void(F func) {
  return [func](context_t &ctx, node_t &node) { func(ctx, node); };
}

auto for_each(context_t &ctx, span_t span, node_fn fn) -> void {
  for (auto &elm : span) {
    auto node = elm.node();
    fn(ctx, node);
  }
}

#define path_fnsig (context_t & ctx, median_t med)->void
#define path_lam [] path_fnsig
#define cpath_lam [&] path_fnsig

using pathing = Pathing<void(context_t &, median_t), medianc::e>;
using pair_t = pathing::pair_t;
template <auto table> auto path(context_t &ctx, median_t med) {
  return pathing::path<table, medianc::length()>(med.type(), ctx, med);
}

namespace runtime {
using pathing = Runtime_Pathing<std::function<void(context_t &, median_t)>, medianc::e>;
using pair_t = pathing::pair_t;
auto path(auto table, context_t &ctx, median_t med) {
  return pathing::path<medianc::length()>(med.type(), table, nullptr, ctx, med);
}
} // namespace runtime

} // namespace semantics

namespace semantics {

namespace decl {
struct var_decl_t {
  sptr<type_t> type;
  sptr<expr_t> init_val;

  var_decl_t(sptr<type_t> t, sptr<expr_t> val)
      : type(std::move(t)), init_val(std::move(val)) {}

  explicit var_decl_t(sptr<type_t> t) : type(std::move(t)), init_val(nullptr) {}

  explicit var_decl_t(sptr<expr_t> val)
      : type(nullptr), init_val(std::move(val)) {}

  bool has_type() const { return static_cast<bool>(type); }
  bool has_init_val() const { return static_cast<bool>(init_val); }
};

struct type_decl_t {
  sptr<type_t> type;
};
struct fn_decl_t {
  sptr<type_t> replace_with_ssptr_fnsig_t;
  // will figure the rest out someday
};

struct scope_decl_t {
  sptr<locale_t> symbols;
};
} // namespace decl

using decl_var = var<std::monostate,
                    decl::var_decl_t,
                    decl::type_decl_t,
                    decl::scope_decl_t,
                    decl::fn_decl_t>;

struct decl_t : public decl_var{
  std::string_view name;
  size_t decl_index;
  size_t target_type_index;
  decl_t(decl_var val, std::string_view n, size_t i, size_t tti)
      : decl_var(val), name(n), decl_index(i), target_type_index(tti) {}
};

namespace type {
struct bitsize_inh {
  size_t size;
  bitsize_inh(const size_t len) : size(len) {}
};
struct float_t : public bitsize_inh {
  float_t(const size_t len) : bitsize_inh(len) {
    if (len != 16 && len != 32 && len != 64 && len != 80 && len != 128) {
      throw std::invalid_argument(
          "Invalid bit size for float_t. Allowed: 16, 32, 64, 80, 128.");
    }
  }
}; // f<number>
struct uint_t : public bitsize_inh {
  using bitsize_inh::bitsize_inh;
}; // u<number>
struct sint_t : public bitsize_inh {
  using bitsize_inh::bitsize_inh;
}; // s<number>
using num_var = var<float_t, uint_t, sint_t>;
struct num_t : public num_var {};

struct boolean_t {}; // bool

struct ref_t {
  sptr<type_t> type;
}; // ~
struct ptr_t {
  sptr<type_t> type;
}; // ^

struct opaque_ptr_t {}; //@ptr

using primitive_var = var<num_t, boolean_t, ptr_t, ref_t, opaque_ptr_t>;
struct primitive_t : public primitive_var {};

struct union_t {
  std::unordered_set<sptr<type_t>> types;
};

struct rec_t {
  std::unordered_map<std::string_view, sptr<decl_t>> decls;
};

struct enum_t {
  sptr<type_t> type;
  std::unordered_map<std::string_view, sptr<expr_t>> enums;
};

struct fnsig_t {
  std::vector<ssptr<decl::var_decl_t, decl_t>> args;
  sptr<type_t> ret;
};
} // namespace type

using type_var =
    var<type::primitive_var, type::union_t, type::rec_t, type::enum_t>;
struct type_t : public type_var {};
struct expr_t {};

} // namespace semantics

namespace semantics {

struct locale_t {
  using var = sptr<decl_t>;
  using lookup_t = opt<std::pair<locale_t *, var>>;
  using entry_t = std::pair<std::string_view, var>;
  using insert_t = std::pair<std::string_view, std::pair<bool, var>>;

  // using var = sptr<decl_t>;
  // using lookup_t = opt<std::pair<locale_t *, var>>;

  sptr<locale_t> parent_;

  std::unordered_map<std::string_view, var> table;

  std::unordered_map<std::string_view, std::vector<unresolved_symbol_t>>
      unresolved_symbols;

  locale_t(sptr<locale_t> p) : parent_(p) {}
  locale_t() : parent_(nullptr) {}

  template <typename T>
  insert_t try_insert(std::string_view name, locale_t::var val,
             const std::size_t tti = var_index<T, decl_var>::value) {
    auto res = this->table.try_emplace(name, val);
    var res_ptr = res.first->second;

    if (res.second) {
      // Resolve unresolved symbols
      auto it = unresolved_symbols.find(name);
      if (it != unresolved_symbols.end()) {
        for (auto &unresolved : it->second) {
          // Check compatibility
          if (unresolved.requested_type == val->target_type_index) {
            // Update the requestee with the resolved declaration
            unresolved.requestee = val;
            std::println("Resolved symbol '{}' for requestee.", name);
          } else {
            std::println("Error: Symbol '{}' resolved but type mismatch.",name);
          }
        }
        unresolved_symbols.erase(it);
      }
    }
    // return {name,std::pair{res_name, res_ptr}};
    return {name, {res.second, res_ptr}};
  }

  bool operator==(const locale_t &rhs) const {
    auto &lhs = *this;

    // Check if parent pointers are equal or if parents themselves are equal
    if (lhs.parent_ != rhs.parent_) {
      if (!lhs.parent_ || !rhs.parent_ || !(*lhs.parent_ == *rhs.parent_))
        return false;
    }

    // Compare tables for equality
    if (lhs.table != rhs.table)
      return false;

    return true;
  }

  template <bool is_local_search = false>
  lookup_t lookup(const std::string_view name) {
    auto it = table.find(name);
    if (it != table.end())
      return std::pair{this, it->second};

    if constexpr (!is_local_search)
      if (parent_)
        return parent_->ancestor_lookup(name);
    return std::nullopt;
  }
  lookup_t ancestor_lookup(const std::string_view name) {
    return lookup<false>(name);
  }
  lookup_t local_lookup(const std::string_view name) {
    return lookup<true>(name);
  }
};

struct context_t : public depth_inh, public decl_index_inh {
  parser::node_buffer_t &nodes;
  token_buffer_t &toks;
  sptr<locale_t> locale;

  template <typename T>
  [[nodiscard]] auto try_insert(std::string_view name, locale_t::var val) -> auto {
    return locale->try_insert<T>(name, val);
  }

  context_t(parser::node_buffer_t &n, token_buffer_t &t, sptr<locale_t> l)
      : nodes(n), toks(t), locale(l) {}
};

static auto print(context_t &ctx, node_t &elm) -> void {
  const auto depth_str = std::string(2 * ctx.depth, ' ');
  ovisit(
      elm,
      [&](const final_t &val) {
        std::println("{}final: {}, {}", depth_str, ctx.toks.str(val),
                     token_code_str(val->type_));
      },
      [&](const median_t &val) {
        std::println("{}median: {}", depth_str, medianc::to_str(val.type()));
        ctx.dive([&] { for_each(ctx, val.children(), print); });
      },
      [&](const auto &val) { std::println("Error"); });
}

template <typename DT> auto create_decl(context_t &ctx, median_t elm) -> auto {
  auto cursor = elm.children().begin();
  const auto name = ctx.toks.str(cursor->as_final());

  auto decl = decl_t(std::monostate{}, name, ctx.increment(),
                     var_index<DT, decl_var>::value);
  auto res = ctx.try_insert<DT>(name, std::make_shared<decl_t>(decl));
  return res;
}

// using table_t = pathing<typename FNT, Valid_Code_Type CODE>

template <size_t Index, typename... Ts>
auto get_under_impl(cursor_t &cursor, std::tuple<Ts...> &tup) -> auto {
  if constexpr (Index < sizeof...(Ts)) {
    using Type = std::tuple_element_t<Index, std::tuple<Ts...>>;
    std::get<Index>(tup) = cursor->as<Type>();
    cursor.advance();
    get_under_impl<Index + 1, Ts...>(cursor, tup);
  }
}

template <typename... Ts>
auto get_under(cursor_t &cursor) -> std::tuple<Ts...> {
  std::tuple<Ts...> tup;
  get_under_impl<0, Ts...>(cursor, tup);
  return tup;
}

namespace type{
auto chain(context_t &ctx, median_t med) -> sptr<type_t> { return nullptr; }
auto fnsig(context_t &ctx, median_t med) -> sptr<type_t> { return nullptr; }
auto primitive(context_t &ctx, median_t med) -> sptr<type_t> { return nullptr; }
auto ptr_ref(context_t &ctx, median_t med) -> sptr<type_t> { return nullptr; }
} // namespace type

auto get_type(context_t &ctx, median_t med) -> sptr<type_t> {
  auto t = med.children().begin();
  return ovisit(
      t->node(),
      [](const final_t &val) -> sptr<type_t> {
        switch(val->type_){
        case tokc::TYPE_INT:
        case tokc::TYPE_UINT:
        case tokc::TYPE_FLOAT:
        default:
          return nullptr;
          break;

        //   std::unreachable();
        //   break;
        }
      },
      [&](const median_t &val) -> sptr<type_t> {
        // return nullptr;
        sptr<type_t> result;
        runtime::path(
            runtime::pathing::table_make(runtime::pathing::pair_t{
                medianc::CHAIN,
                [&result](context_t &ctx, auto med) -> void {
                  for (auto &child : med.children()) {
                    auto id_node = &child;

                    // Assuming `id_node` is a final node with an identifier
                    // (adjust this if necessary)
                    const auto id = ctx.toks.str(id_node->as_final());

                    // Perform the lookup in the current scope
                    auto lookup_result = ctx.locale->lookup(id);

                    if (lookup_result) {
                      // Found a symbol in the current scope
                      auto [scope, symbol] = *lookup_result;
                    } else {
                      // Handle unresolved symbols if necessary
                      std::println("Error: Symbol '{}' not found in current scope.", id);
                      break;
                    }
                  }
                }

              }),
            ctx, val);
        return result;
      },
      [](const auto &val) -> sptr<type_t> {
        std::unreachable();
      });
}

namespace decl {
auto var_decl path_fnsig {
  auto res = create_decl<var_decl_t>(ctx, med);

  auto &[name, entry] = res;
  auto &[insert, symbol] = entry;

  if (!insert) {
    std::println("Error: The symbol '{}' already exists.", name);
    return;
  }

  std::println("Successfully declared variable '{}'.", name);
  auto span = med.children();

  auto cursor = span.at(1);
  auto med2 = cursor->as_median();


  const auto value_fn = [](context_t &ctx, median_t med) -> void {
    // std::println("Value, there should be an expresion here");
  };
  const auto type_fn = [&](context_t &ctx, median_t med) -> void {
    // std::println("Type, something freaky is here");
    // do type stuff

    get_type(ctx, med);

    cursor.advance();
    if (!span.within(cursor))
      return;

    // do stuff
    value_fn(ctx, cursor->as_median());
  };

  runtime::path(runtime::pathing::table_make(
                    runtime::pathing::pair_t{medianc::TYPE, type_fn},
                    runtime::pathing::pair_t{medianc::VALUE, value_fn}),
                ctx, med2);
}

auto type_decl path_fnsig {
  auto res = create_decl<var_decl_t>(ctx, med);
  auto &[name, entry] = res;
  auto &[insert, symbol] = entry;
  if (!insert) {
    std::println("Error: The symbol '{}' already exists.", name);
    return;
  }
  std::println("Successfully declared type alias '{}'.", name);
}

auto scope_decl path_fnsig {
  auto res = create_decl<scope_decl_t>(ctx, med);
  auto &[name, entry] = res;
  auto &[insert, symbol] = entry;
  std::println("Successfully declared a scope'{}'.", name);
}
auto fn_decl path_fnsig {
  auto res = create_decl<fn_decl_t>(ctx, med);
  auto &[name, entry] = res;
  auto &[insert, symbol] = entry;
  if (!insert) {
    std::println("Error: The symbol '{}' already exists.", name);
    return;
  }
  std::println("Successfully declared a function '{}'.", name);
}
} // namespace decl

auto for_loop path_fnsig { std::println("For loop"); }
auto expr path_fnsig { std::println("Expresion"); }

auto decls(context_t &ctx, median_t &elm) -> void {
  constexpr auto table =
      pathing::table_make<{medianc::DECL, decl::var_decl},
                          {medianc::TYPE_DECL, decl::type_decl},
                          {medianc::SCOPE_DECL, decl::scope_decl},
                          {medianc::FN_DECL, decl::fn_decl},
                          {medianc::FOR, for_loop},
                          {medianc::EXPR, expr}>();

  auto val = elm.children().begin()->as_median();
  path<table>(ctx, val);
}

auto visit(final_fn f, median_fn m) -> node_fn {
  return [f, m](context_t &ctx, node_t &node) -> void {
    ovisit(
        node, [&](std::monostate &val) -> void {},
        [&](final_t &val) -> void { f(ctx, val); },
        [&](median_t &val) -> void {
          auto proxy = val;
          m(ctx, proxy);
        },
        [&](err_t &val) -> void { std::unreachable(); });
  };
}

auto unreachable_final(context_t &ctx, final_t &fin) -> void {
  std::unreachable();
};

auto entry(parser::node_buffer_t &nodes,
           token_buffer_t &toks) -> sptr<locale_t> {
  auto global_scope = std::make_shared<semantics::locale_t>();
  auto ctx = context_t{nodes, toks, global_scope};
  auto span = nodes.front().as_median().children();
  for_each(ctx, span, visit(unreachable_final, decls));
  return nullptr;
}

} // namespace semantics
