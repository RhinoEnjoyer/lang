#pragma once

// #define SEMANTICS_DEBUG

#include "../mesure.hpp"
#include "../nicknames.hpp"
#include "../table.hpp"
#include "./parser.hpp"

#include <cstdint>
#include <functional>
#include <print>
#include <source_location>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>

// Multi files
//  start form one file
//  discover other files
//  spawn threads
//  when resolving a symbol
//  how to be able to retrieve the data???
//    virtual locale class??
//  ban variable declarations on top level ??
//  (separate functions form variables a bit more??)
//
//

#define VAR_MACRO(name, ...)                                                   \
  using name##_var = var<__VA_ARGS__>;                                         \
  struct name##_t : public name##_var {                                        \
    using name##_var::variant;                                                 \
    using name##_var::operator=;                                               \
  }

namespace semantics {

struct decl_t;
struct type_t;
struct expr_t;
struct stmts_t;
struct locale_t;
struct context_t;

using node_t = grammar::node_t::external_node;
using span_t = grammar::node_t::median_proxy_t<false>::span_t;
using cursor_t = span_t::iterator;
using median_t = grammar::node_t::median_proxy_t<false>;
using final_t = grammar::node_t::final_t;
using err_t = grammar::node_t::err_t;

struct ask_t {
  virtual sptr<locale_t> get_locale() = 0;
};

struct unresolved_t {
  median_t med;
};

namespace type_s {
struct template_args_t;
}
namespace decl_s {
struct scope_decl_t : public ask_t {
  sptr<locale_t> loc;
  sptr<stmts_t> stmts;

  scope_decl_t(sptr<locale_t> l, sptr<stmts_t> s) : loc(l), stmts(s) {}

  sptr<locale_t> get_locale() override { return loc; }
};

struct var_decl_t {
  opt<bool> mut;
  sptr<type_t> type;
  sptr<expr_t> expr;
};

struct type_decl_t {
  struct template_module_t {
    sptr<locale_t> locale;
    sptr<type_s::template_args_t> targs;
  };

  opt<template_module_t> mod;
  sptr<type_t> type;
};

struct unwrap_decl_elm_t {
  size_t index;
  sptr<expr_t> init_val;
};

struct fn_decl_t {
  sptr<type_t> sig;
  sptr<expr_t> body;
};

struct unresolved_t {
  median_t type;
  opt<median_t> value;
};

using var = var<empty_t, unresolved_t, fn_decl_t, unwrap_decl_elm_t, var_decl_t,
                scope_decl_t, type_decl_t>;
} // namespace decl_s

struct decl_t : public decl_s::var {
  using decl_s::var::variant;
  std::string_view name;
  size_t dindex;

  decl_t(decl_s::var v, std::string_view n, size_t i)
      : decl_s::var(v), name(n), dindex(i) {}

  decl_t(std::string_view n, size_t i)
      : decl_s::var(empty_t{}), name(n), dindex(i) {}

  decl_t &operator=(const auto &rhs) {
    // only update the underlying variant portion, leaving name and dindex
    // intact.
    static_cast<decl_s::var &>(*this) = rhs;
    return *this;
  }

  // (optional) overload move assignment operator for decl_s::var
  decl_t &operator=(auto &&rhs) {
    static_cast<decl_s::var &>(*this) = std::move(rhs);
    return *this;
  }
};

namespace type_s {
using type_ptr = sptr<type_t>;

struct bitsize_t {
  size_t size;

  bitsize_t(size_t s) : size(s) {}
  bitsize_t(std::string_view str)
      : size(std::stoull(std::string(str.substr(1)))) {}

  auto tobyte() -> size_t { return size / 8; }
};
struct float_t : public bitsize_t {
  // could make it bettr but it works and i do not think i will ever change it
  // so it is fine
  float_t(const size_t len) : bitsize_t(len) {
    if (size != 16 && size != 32 && size != 64 && size != 80 && size != 128) {
      throw std::invalid_argument(
          "invalid bit size for float_t. allowed: 16, 32, 64, 80, 128.");
    }
  }
  float_t(std::string_view str) : bitsize_t(str) {
    if (size != 16 && size != 32 && size != 64 && size != 80 && size != 128) {
      throw std::invalid_argument(
          "invalid bit size for float_t. allowed: 16, 32, 64, 80, 128.");
    }
  }
};
struct sint_t : public bitsize_t {
  using bitsize_t::bitsize_t;
};
struct uint_t : public bitsize_t {
  using bitsize_t::bitsize_t;
};
struct boolean_t : public bitsize_t {
  using bitsize_t::bitsize_t;
};
VAR_MACRO(numeric, float_t, sint_t, uint_t, boolean_t);

struct ref_t {
  sptr<type_t> type;
};
struct ptr_t {
  sptr<type_t> type;
};
struct optr_t {};
using indirection_t = var<ref_t, ptr_t, optr_t>;

struct void_t {};
VAR_MACRO(primitive, void_t, indirection_t, numeric_t);

struct template_args_t {
  list<sptr<decl_t>> args;
};

struct rec_t : public ask_t {
  sptr<locale_t> loc;
  // sptr<template_args_t> targs;
  sptr<stmts_t> members;

  rec_t(sptr<locale_t> l, /* sptr<template_args_t>& t, */ sptr<stmts_t> m)
      : ask_t(), loc(l), /* targs(t), */ members(m) {}

  sptr<locale_t> get_locale() override { return loc; }
};
struct tup_t {
  list<sptr<type_t>> types;
};
VAR_MACRO(aggregate, rec_t, tup_t);

struct fntype_t {
  list<sptr<type_t>> arg_types;
  sptr<type_t> ret_type;
};

struct fntemplate_t {
  sptr<locale_t> locale;
  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;
};

struct fnsig_t : public ask_t {
  sptr<locale_t> locale;
  sptr<template_args_t> template_args;
  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;

  fnsig_t(sptr<locale_t> l, sptr<template_args_t> ta,
          list<ssptr<decl_s::var_decl_t, decl_t>> &a, sptr<type_t> r)
      : ask_t(), locale(l), template_args(ta), args(a), ret_type(r) {}

  fnsig_t(sptr<locale_t> l, sptr<template_args_t> ta,
          list<ssptr<decl_s::var_decl_t, decl_t>> &&a, sptr<type_t> r)
      : ask_t(), locale(l), template_args(ta), args(std::move(a)), ret_type(r) {
  }

  sptr<locale_t> get_locale() override { return locale; }
};
struct fn_t : public ask_t {
  sptr<fnsig_t> sig;

  fn_t(sptr<fnsig_t> s) : ask_t(), sig(s) {}

  // sptr<stmts_t> body;
  sptr<locale_t> get_locale() override { return sig->get_locale(); }
};
struct closure_t : public ask_t {
  sptr<fnsig_t> sig;
  closure_t(sptr<fnsig_t> s) : ask_t(), sig(s) {}
  sptr<locale_t> get_locale() override { return sig->get_locale(); }
  // sptr<stmts_t> body;
};
VAR_MACRO(callable, fn_t, closure_t);

struct infered_t {};

struct type_ref_t {
  sptr<type_t> ref;
};

// struct template_stamper_t {
//   using ret_t = sptr<type_t>;
//   using list_t = list<var<sptr<type_t>, sptr<expr_t>>>;
//   using fntype_t = ret_t(context_t &, sptr<locale_t>, list_t);

//   std::map<list_t, sptr<type_t>> cache;
//   std::function<fntype_t> stamp_fn;

//   ret_t operator()(context_t &ctx, sptr<locale_t> locale, list_t& args) {
//     auto find = cache.find(args);
//     if (find != cache.end())
//       return find->second;
//     return stamp_fn(ctx, locale, args);
//   }
// };

using var = var<empty_t, fntemplate_t, fntype_t, type_ref_t, infered_t,
                primitive_t, aggregate_t, callable_t, unresolved_t>;
} // namespace type_s
struct type_t : public type_s::var {
  using type_s::var::variant;

  type_s::var &base() { return *this; }
};

namespace expr_s {
enum class op_assoc_e : int { LEFT, RIGHT };
enum class op_type_e : int { UNARY, BINARY, TERNARY };

template <op_type_e TYPE, size_t P, op_assoc_e A, size_t AR = 2>
struct op_base {
  static constexpr op_type_e type() { return TYPE; }
  static constexpr size_t prec() { return P; }
  static constexpr op_assoc_e assoc() { return A; }
  static constexpr size_t arity() { return AR; }
  auto &base() { return *this; }
};

template <size_t P, op_assoc_e A>
struct op_unary_base : op_base<op_type_e::UNARY, P, A, 1> {};
template <size_t P, op_assoc_e A>
struct op_bin_base : op_base<op_type_e::BINARY, P, A, 2> {};
template <size_t P, op_assoc_e A>
struct op_ternary_base : op_base<op_type_e::TERNARY, P, A, 3> {};

struct plusplus_t : op_unary_base<12, op_assoc_e::LEFT> {};
struct minusminus_t : op_unary_base<12, op_assoc_e::LEFT> {};
struct plus_t : op_bin_base<8, op_assoc_e::LEFT> {};
struct minus_t : op_bin_base<8, op_assoc_e::LEFT> {};
struct mult_t : op_bin_base<9, op_assoc_e::LEFT> {};
struct div_t : op_bin_base<9, op_assoc_e::LEFT> {};
struct mod_t : op_bin_base<9, op_assoc_e::LEFT> {};
VAR_MACRO(arithmetic, plus_t, minus_t, mult_t, div_t, mod_t, plusplus_t,
          minusminus_t);

struct eq_t : op_bin_base<5, op_assoc_e::LEFT> {};
struct neq_t : op_bin_base<5, op_assoc_e::LEFT> {};
struct greater_t : op_bin_base<6, op_assoc_e::LEFT> {};
struct less_t : op_bin_base<6, op_assoc_e::LEFT> {};
struct leq_t : op_bin_base<6, op_assoc_e::LEFT> {};
struct geq_t : op_bin_base<6, op_assoc_e::LEFT> {};
VAR_MACRO(comparison, eq_t, neq_t, greater_t, less_t, leq_t, geq_t);

struct not_t : op_unary_base<10, op_assoc_e::RIGHT> {};
struct and_t : op_bin_base<4, op_assoc_e::LEFT> {};
struct xor_t : op_bin_base<3, op_assoc_e::LEFT> {};
struct or_t : op_bin_base<2, op_assoc_e::LEFT> {};
struct sleft_t : op_bin_base<7, op_assoc_e::LEFT> {};
struct sright_t : op_bin_base<7, op_assoc_e::LEFT> {};
VAR_MACRO(binary, not_t, and_t, xor_t, or_t, sleft_t, sright_t);

struct assign_t : op_bin_base<0, op_assoc_e::RIGHT> {};
struct plusassign_t : op_bin_base<0, op_assoc_e::RIGHT> {};
struct minusassign_t : op_bin_base<0, op_assoc_e::RIGHT> {};
struct multassign_t : op_bin_base<0, op_assoc_e::RIGHT> {};
struct divassign_t : op_bin_base<0, op_assoc_e::RIGHT> {};
VAR_MACRO(assignment, assign_t, plusassign_t, minusassign_t, multassign_t,
          divassign_t);

struct pipe_t : op_bin_base<1, op_assoc_e::LEFT> {};
struct neg_t : op_bin_base<11, op_assoc_e::RIGHT> {};
struct deref_t : op_unary_base<11, op_assoc_e::RIGHT> {};
struct address_t : op_unary_base<11, op_assoc_e::RIGHT> {};
struct as_t : op_unary_base<10, op_assoc_e::RIGHT> {
  sptr<type_t> type;
};
VAR_MACRO(operator, pipe_t, neg_t, deref_t, address_t, as_t, assignment_t,
          binary_t, comparison_t, arithmetic_t);

struct number_t {
  std::string_view val;
};

struct fncall_t {
  list<sptr<expr_t>> args;
};

struct decl_access_t {
  sptr<decl_t> decl;
};

// the parser needs to change to allow different representations
//  the function one should make basicaly a new locale_t and stmts_t
//  but anyother should be just an expresion list
struct complit_t {
  sptr<type_t> type;
  list<sptr<expr_t>> init_vals;
};

struct sizeof_t {
  var<sptr<type_t>, sptr<expr_t>> val;
};

// this a chain like
struct result_t {
  sptr<locale_t> loc;
  sptr<stmts_t> stmts;
  // plus whatever a chain has here
};

struct if_t {
  struct if_intro_t {
    sptr<expr_t> ctrl_expr;
    sptr<stmts_t> stmts;
  };
  struct elif_t {
    sptr<expr_t> ctrl_expr;
    sptr<stmts_t> stmts;
  };
  struct else_t {
    sptr<stmts_t> stmts;
  };

  if_intro_t intro;
  list<elif_t> elifs;
  std::optional<else_t> el;
};

// chain like
struct fn_lit_t {
  ssptr<type_s::callable_t, type_t> sig;
  sptr<stmts_t> body;
};

struct decl_ref_t {
  sptr<decl_t> decl;
};

VAR_MACRO(operand, unresolved_t, decl_ref_t, fn_lit_t, if_t, complit_t,
          fncall_t, number_t, sizeof_t, decl_access_t, result_t);
using var = var<empty_t, operator_t, operand_t>;
} // namespace expr_s
struct expr_elm_t : public expr_s::var {
  using expr_s::var::variant;
};
struct expr_t {
  list<sptr<expr_elm_t>> exprs;
};

namespace stmt_s {

struct unwrap_decl_arr_t {
  list<sptr<decl_t>> wraps;
};

struct import_t {
  std::string_view file;
};

struct ret_t {
  sptr<expr_t> expr;
};

struct forloop_t {
  sptr<locale_t> loc;
  sptr<stmts_t> ctrl_expr_stmts;
  sptr<stmts_t> body_stmts;
  void a() {}
};
using var = var<empty_t, unwrap_decl_arr_t, forloop_t, import_t, ret_t,
                sptr<decl_t>, sptr<expr_t>>;
} // namespace stmt_s
struct stmt_t : stmt_s::var {
  using stmt_s::var::variant;
  using stmt_s::var::operator=;
};
struct stmts_t {
  list<sptr<stmt_t>> stmts;
};

struct locale_t {
  using val_t = sptr<decl_t>;
  using entry_t = val_t;
  using map_entry_t = std::tuple<std::string_view, val_t>;
  // using insert_t = std::tuple<bool, std::string_view, val_t>;

  struct path_t {
    using val_t = sptr<locale_t>;
    list<val_t> path;

    void push(const val_t &locale) { path.push_back(locale); }
    void push(const val_t &&locale) { path.push_back(std::move(locale)); }

    val_t latest() const noexcept {
      // the path can't be empty
      return path.back();
    }
    const list<val_t> &all() const { return path; }
    bool empty() const { return path.empty(); }
    size_t size() const { return path.size(); }
    void clear() { path.clear(); }
  };

  struct insert_t {
    bool inserted;
    std::string_view name;
    val_t entry;
  };

  struct lookup_t {
    const bool found;
    const path_t path;
    const entry_t symbol;
  };

  template <typename SPECIALISED_TYPE> struct slookup_t {
    const bool found;
    const path_t path;
    const ssptr<SPECIALISED_TYPE, decl_t> symbol;
  };

  sptr<locale_t> parent_;
  list<sptr<locale_t>> children_;

  // general
  static sptr<locale_t> make_child(auto &ctx, sptr<locale_t> self) {
    auto ptr = ctx.make_sptr(locale_t{self, {}});
    self->children_.push_back(ptr);
    return ptr;
  }

  sptr<locale_t> parent() { return parent_; }

  // implementation specific
  struct internal {
    std::unordered_map<std::string_view, entry_t> table = {};
    static lookup_t ancestor_lookup(sptr<locale_t> self, path_t &path,
                                    const std::string_view name) {
      return lookup<false>(self, path, name);
    }
    static lookup_t local_lookup(sptr<locale_t> self, path_t &path,
                                 const std::string_view name) {
      return lookup<true>(self, path, name);
    }
    template <bool is_local_search>
    static lookup_t lookup(sptr<locale_t> &self, path_t &path,
                           const std::string_view name) {
      path.push(self);
      auto it = self->internals.table.find(name);

      if (it != self->internals.table.end())
        return {true, path, it->second};

      if constexpr (!is_local_search)
        if (self->parent_) {
          return ancestor_lookup(self->parent_, path, name);
        }
      return {false, path, {nullptr}};
    }

    insert_t try_insert(std::string_view name, entry_t &&val) {
      auto res = table.try_emplace(name, std::forward<entry_t>(val));
      return {res.second, name, res.first->second};
    }
  } internals;

  insert_t try_insert(std::string_view name, entry_t &&val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  static lookup_t ancestor_lookup(sptr<locale_t> self,
                                  const std::string_view name) {
    path_t path;
    return internal::lookup<false>(self, path, name);
  }
  static lookup_t local_lookup(sptr<locale_t> self,
                               const std::string_view name) {
    path_t path;
    return internal::lookup<true>(self, path, name);
  }

  template <typename VAR_TYPE, auto lookup_fn = local_lookup>
  static auto expect_lookup(sptr<locale_t> self, const std::string_view name)
      -> slookup_t<VAR_TYPE> {
    using ret_type = slookup_t<VAR_TYPE>;

    static_assert(lookup_fn == locale_t::local_lookup ||
                      lookup_fn == locale_t::ancestor_lookup,
                  "lookup_fn must be local_lookup or ancestor_lookup");

    const lookup_t lookup = lookup_fn(self, name);

    if (!lookup.found || !std::holds_alternative<VAR_TYPE>(*lookup.symbol))
      return ret_type{false, std::move(lookup.path), sptr<decl_t>{}};

    return ret_type{true, std::move(lookup.path), lookup.symbol};
  }
};

struct resolve_callback_t {
  template <typename Y, typename T> struct call_t {
    ssptr<Y, T> ptr;
    std::function<void(ssptr<Y, T>)> call;
    void operator()() { call(ptr); }
  };

  var<call_t<unresolved_t, type_t>, call_t<decl_s::unresolved_t, decl_t>> call;

  template <typename Y, typename T>
  auto get_ptr() -> opt<std::reference_wrapper<sptr<T>>> {
    if (rholds<call_t<Y, T>>(call))
      return std::get<call_t<Y, T>>(call).ptr;
    return std::nullopt;
  }
  template <typename Y, typename T>
  auto get_callback_obj() -> opt<std::reference_wrapper<call_t<Y, T>>> {
    if (rholds<call_t<Y, T>>(call))
      return std::get<call_t<Y, T>>(call);
    return std::nullopt;
  }
  void operator()() {
    ovisit(call, [](auto &val) -> auto { return val(); });
  }

  template <typename... FNS> auto visit(FNS... fns) -> auto {
    return ovisit(call, std::forward<FNS>(fns)...);
  }
};

struct context_t {
  const token_buffer_t &toks;
  size_t cindex;
  allocator_t &allocator;

  std::map<uintptr_t, resolve_callback_t> callback_map = {};

#ifdef SEMANTICS_DEBUG
  struct callstack_entry_t {
    std::string function;
    std::string file;
    int line;
  };
  list<callstack_entry_t> dbg_callstack = {};
#endif

  void
  dbg_add_call(std::source_location loc = std::source_location::current()) {
#ifdef SEMANTICS_DEBUG
    dbg_callstack.push_back(
        {loc.function_name(), loc.file_name(), static_cast<int>(loc.line())});
#endif
  }

  template <typename T> void insert_callback(sptr<T> ptr, auto callback) {
    callback_map.insert({(uintptr_t)ptr.get_ptr(), callback});
  }

  auto insert_call() {}

  size_t operator++() { return cindex++; }
  static std::size_t size;

  template <typename T, typename... Args>
  auto make_sptr(Args &&...val) -> sptr<T> {
    return make_ptr<T, Args...>(allocator, std::forward<Args>(val)...);
  }
  template <typename T> auto make_sptr(T &&val) -> sptr<T> {
    return make_ptr<T>(allocator, val);
  }
  template <typename T> auto make_sptr(T &val) -> sptr<T> {
    return make_ptr<T>(allocator, val);
  }
};

struct cursor_helper_t {
  cursor_helper_t(const span_t s) : span_(s), cursor_(s.begin()) {}

  template <auto in = medianc::any, typename in_t = decltype(in),
            typename t = std::conditional_t<std::is_same_v<in_t, medianc::e>,
                                            median_t, final_t>>
  std::optional<t> extract() {
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

  bool within() const { return span_.contains(cursor_); }

private:
  span_t span_;
  cursor_t cursor_;
};
namespace symbols {
auto entry(allocator_t &allocator, const token_buffer_t &toks,
           grammar::node_t &root_node)
    -> std::tuple<sptr<locale_t>, sptr<stmts_t>>;
}
} // namespace semantics
