#pragma once

// #define SEMANTICS_DEBUG
#include <list>
#include <atomic>
#include <boost/container/flat_map.hpp>
#include <boost/core/typeinfo.hpp>
#include "../nicknames.hpp"
#include "../table.hpp"
#include "./parser.hpp"

#include <set>
#include <cstdint>
#include <functional>
#include <mutex>
#include <print>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>

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

struct unresolved_t {
  median_t med;
};

namespace type_s {
struct template_args_t;
struct callable_t;
} // namespace type_s

struct frame_t {
  sptr<locale_t> locale;
  sptr<stmts_t> stmts;
  frame_t(sptr<locale_t> l, sptr<stmts_t> s) : locale(l), stmts(s) {}
};

namespace decl_s {

struct scope_decl_t {
  frame_t frame;
  scope_decl_t(sptr<locale_t> l, sptr<stmts_t> s) : frame(l, s) {}
  sptr<locale_t> get_locale() { return frame.locale; }
};

struct var_decl_t {
  bool mut;
  sptr<type_t> type;
  sptr<expr_t> expr;
};

struct type_decl_t {
  sptr<type_t> type;
};

struct rec_member_t {
  ssptr<var_decl_t, decl_t> decl;
  size_t index;
};

struct template_stamp_decl_t {
  struct template_module_t {
    sptr<locale_t> locale;
    sptr<type_s::template_args_t> targs;
  };
  template_module_t mod;
  sptr<type_t> type;
};

struct unwrap_decl_elm_t {
  size_t index;
  sptr<expr_t> init_val;
};

struct fn_decl_t {
  ssptr<type_s::callable_t, type_t> sig;
  sptr<expr_t> body;
};

struct unresolved_t {
  median_t type;
  opt<median_t> value;
};
using template_type_input_t = ssptr<type_decl_t, type_t>;
using template_var_input_t = ssptr<var_decl_t, decl_t>;
using var = var<empty_t, template_type_input_t, template_var_input_t,
                unresolved_t, fn_decl_t, unwrap_decl_elm_t, var_decl_t,
                rec_member_t, scope_decl_t, type_decl_t, template_stamp_decl_t>;
} // namespace decl_s

struct decl_t : public decl_s::var {
  using decl_s::var::variant;

  std::string name;
  size_t dindex;

  decl_t(decl_s::var v, std::string& n, size_t i)
      : decl_s::var(v), name(n), dindex(i) {}
  decl_t(decl_s::var v, std::string&& n, size_t i)
      : decl_s::var(v), name(std::move(n)), dindex(i) {}
  decl_t(decl_s::var v, std::string_view n, size_t i)
      : decl_s::var(v), name(n), dindex(i) {}

  decl_t(std::string_view n, size_t i)
      : decl_s::var(empty_t{}), name(n), dindex(i) {}

  const std::string_view get_name() const noexcept { return name; }
  size_t get_index() const noexcept { return dindex; }
  decl_t &operator=(const auto &rhs) {
    static_cast<decl_s::var &>(*this) = rhs;
    return *this;
  }
  decl_t &operator=(auto &&rhs) {
    static_cast<decl_s::var &>(*this) = std::move(rhs);
    return *this;
  }
};

namespace type_s {
struct bitsize_t {
  size_t size;

  bitsize_t(size_t s) : size(s) {}
  bitsize_t(std::string_view str)
      : size(std::stoull(std::string(str.substr(1)))) {}

  auto &get_bitsize() { return *this; }

  auto bytesize() const -> size_t { return size / 8; }
  auto bitsize() const -> size_t { return size; }

  auto operator<=>(const bitsize_t &other) { return this->size <=> other.size; }
};

#define numeric_var_cmp_fn_macro(T)                                            \
  inline auto operator<=>(const T &other) const {                              \
    return this->size <=> other.size;                                          \
  }
#define numeric_var_bitsize_constructor(T)                                     \
  T(const size_t val) : bitsize_t(val) {}                                      \
  T(const std::string_view val) : bitsize_t(val) {}

struct float_t : bitsize_t {
  // could make it better but it works and i do not think i will ever change it
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
  numeric_var_cmp_fn_macro(float_t)
};

struct sint_t : bitsize_t {
  numeric_var_bitsize_constructor(sint_t)
  numeric_var_cmp_fn_macro(sint_t)
};
struct uint_t : bitsize_t {
  numeric_var_bitsize_constructor(uint_t)
  numeric_var_cmp_fn_macro(uint_t)
};
struct boolean_t : bitsize_t {
  numeric_var_bitsize_constructor(boolean_t)
  numeric_var_cmp_fn_macro(boolean_t)
};

using numeric_var = var<float_t, sint_t, uint_t, boolean_t>;
struct numeric_t : public numeric_var {
  using numeric_var ::variant;
  using numeric_var ::operator=;
};

struct ptr_t {sptr<type_t> type;};
struct iptr_t {sptr<type_t> type;};
struct optr_t {};
struct array_t {
  sptr<expr_t> len;
  sptr<type_t> type;
};

using indirection_var = var<ptr_t, iptr_t, optr_t, array_t>;
struct indirection_t : public indirection_var {
  using indirection_var ::variant;
  using indirection_var ::operator=;
};
struct void_t {};
VAR_MACRO(primitive, void_t, numeric_t);

struct template_args_t {
  using type_input_ptr =ssptr<decl_s::template_type_input_t, decl_t>;
  using var_input_ptr =ssptr<decl_s::var_decl_t, decl_t>;
  using var = var<type_input_ptr, var_input_ptr>;
  using list_t = list<var>;
  list_t args;

  size_t size() const noexcept { return args.size(); }
  var at(size_t i) const { return args[i]; }
};

struct rec_t {
  sptr<locale_t> loc;

  using membholder_t = list<sptr<decl_t>>;
  membholder_t members;

  sptr<locale_t> get_locale() { return loc; }
};

struct tup_t {
  list<sptr<type_t>> types;
  sptr<locale_t> get_locale() { return nullptr; }
};

struct collection_t {};

using aggregate_var = var<rec_t, tup_t>;
struct aggregate_t : public aggregate_var {
  using aggregate_var ::variant;
  using aggregate_var ::operator=;

  aggregate_var &var() { return static_cast<aggregate_var &>(*this); }
  const aggregate_var &var() const {return static_cast<const aggregate_var &>(*this);}

  bool is_rec(){ return holds<rec_t>(var());}
  // bool is_rec() const { return holds<const rec_t>(var());}

  bool is_tup(){ return holds<tup_t>(var());}
  // bool is_tup() const { return holds<const tup_t>(var());}

  rec_t &get_as_rec() { return std::get<rec_t>(var()); }
  tup_t &get_as_tup() { return std::get<tup_t>(var()); }

  // const rec_t &get_as_rec() const { return std::get<rec_t>(var()); }

  sptr<locale_t> get_locale() {
    return ovisit(var(), [](auto &val) { return val.get_locale(); });
  }
};

struct fntype_t {
  list<sptr<type_t>> arg_types;
  sptr<type_t> ret_type;
};

struct fntemplate_t {
  sptr<locale_t> locale;
  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;
};

struct fnsig_t {
  sptr<locale_t> locale;
  sptr<template_args_t> template_args;
  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;
  sptr<locale_t> get_locale() { return locale; }
};

struct fn_t {};
struct closure_t {};

using callable_var = var<fn_t, closure_t>;
struct callable_t : public callable_var {
  using callable_var::variant;
  using callable_var::operator=;

  sptr<fnsig_t> sig;

  // Constructor for fn_t + fnsig_t
  callable_t(fn_t &fn, sptr<fnsig_t> sig)
      : callable_var(fn), sig(std::move(sig)) {}
  callable_t(fn_t &&fn, sptr<fnsig_t> sig)
      : callable_var(fn), sig(std::move(sig)) {}

  // Constructor for closure_t + fnsig_t
  callable_t(const closure_t &closure, sptr<fnsig_t> sig)
      : callable_var(closure), sig(std::move(sig)) {}
  callable_t(const closure_t &&closure, sptr<fnsig_t> sig)
      : callable_var(closure), sig(std::move(sig)) {}

  sptr<locale_t> get_locale() const { return sig->get_locale(); }
  sptr<type_t> get_ret_type() const { return sig->ret_type; }
  auto get_template_args() const { return sig->template_args; }
  const auto &get_args() const { return sig->args; }
};
struct infered_t {};

struct type_ref_t {
  sptr<type_t> ref;
};

struct template_type_input_t {
  ssptr<decl_s::template_type_input_t, decl_t> ref;
};
using var = var<empty_t, indirection_t, template_type_input_t, fntemplate_t,
                fntype_t, type_ref_t, infered_t, primitive_t, aggregate_t,
                callable_t, unresolved_t>;
} // namespace type_s
struct type_t : public type_s::var {
  using type_s::var::variant;
  type_s::var &base() { return *this; }
};

struct expr_elm_t;
namespace expr_s {

enum class op_type_e : int { UNARY = 1, BINARY = 2, TERNARY = 3 };
enum class op_assoc_e : int { LEFT, RIGHT };
enum class op_operation_e : int {
  PLUS,
  MINUS,
  MULT,
  DIV,
  MOD,
  PLUSPLUS,
  MINUSMINUS,

  EQ,
  NEQ,
  GREATER,
  LESS,
  LEQ,
  GEQ,

  NOT,
  AND,
  XOR,
  OR,
  SLEFT,
  SRIGHT,

  ASSIGN,
  PLUSASSIGN,
  MINUSASSIGN,
  MULTASSIGN,
  DIVASSIGN,

  PIPE,
  NEG,
  DEREF,
  ADDRESS,
  AS,

  last
};
inline const char* str(op_operation_e op) {
  switch (op) {
    case op_operation_e::PLUS: return "PLUS";
    case op_operation_e::MINUS: return "MINUS";
    case op_operation_e::MULT: return "MULT";
    case op_operation_e::DIV: return "DIV";
    case op_operation_e::MOD: return "MOD";
    case op_operation_e::PLUSPLUS: return "PLUSPLUS";
    case op_operation_e::MINUSMINUS: return "MINUSMINUS";
    case op_operation_e::EQ: return "EQ";
    case op_operation_e::NEQ: return "NEQ";
    case op_operation_e::GREATER: return "GREATER";
    case op_operation_e::LESS: return "LESS";
    case op_operation_e::LEQ: return "LEQ";
    case op_operation_e::GEQ: return "GEQ";
    case op_operation_e::NOT: return "NOT";
    case op_operation_e::AND: return "AND";
    case op_operation_e::XOR: return "XOR";
    case op_operation_e::OR: return "OR";
    case op_operation_e::SLEFT: return "SLEFT";
    case op_operation_e::SRIGHT: return "SRIGHT";
    case op_operation_e::ASSIGN: return "ASSIGN";
    case op_operation_e::PLUSASSIGN: return "PLUSASSIGN";
    case op_operation_e::MINUSASSIGN: return "MINUSASSIGN";
    case op_operation_e::MULTASSIGN: return "MULTASSIGN";
    case op_operation_e::DIVASSIGN: return "DIVASSIGN";
    case op_operation_e::PIPE: return "PIPE";
    case op_operation_e::NEG: return "NEG";
    case op_operation_e::DEREF: return "DEREF";
    case op_operation_e::ADDRESS: return "ADDRESS";
    case op_operation_e::AS: return "AS";
    case op_operation_e::last: return "last";
    default: return "UNKNOWN";
  }
}
struct op_meta_t {
  const op_operation_e op;
  const op_type_e type;
  const op_assoc_e assoc;
  const std::int32_t prec;
  const bool has_payload;
};

// static constexpr auto op_table = std::array<op_t, static_cast<size_t>(op_operation_e::last)>{
//   op_t{op_operation_e::PLUS, op_type_e::BINARY, op_assoc_e::LEFT, 8},
//   op_t{op_operation_e::MINUS, op_type_e::BINARY, op_assoc_e::LEFT, 8},
//   op_t{op_operation_e::MULT, op_type_e::BINARY, op_assoc_e::LEFT, 9},
//   op_t{op_operation_e::DIV, op_type_e::BINARY, op_assoc_e::LEFT, 9},
//   op_t{op_operation_e::PLUSPLUS, op_type_e::UNARY, op_assoc_e::LEFT, 12},
//   op_t{op_operation_e::MINUSMINUS, op_type_e::UNARY, op_assoc_e::LEFT, 12},
// };

static constexpr auto op_table = std::array<op_meta_t, static_cast<size_t>(op_operation_e::last)>{
  // Arithmetic
  op_meta_t{op_operation_e::PLUS,        op_type_e::BINARY, op_assoc_e::LEFT, 8, false},
  op_meta_t{op_operation_e::MINUS,       op_type_e::BINARY, op_assoc_e::LEFT, 8, false},
  op_meta_t{op_operation_e::MULT,        op_type_e::BINARY, op_assoc_e::LEFT, 9, false},
  op_meta_t{op_operation_e::DIV,         op_type_e::BINARY, op_assoc_e::LEFT, 9, false},
  op_meta_t{op_operation_e::MOD,         op_type_e::BINARY, op_assoc_e::LEFT, 9, false},
  op_meta_t{op_operation_e::PLUSPLUS,    op_type_e::UNARY,  op_assoc_e::LEFT, 12, false},
  op_meta_t{op_operation_e::MINUSMINUS,  op_type_e::UNARY,  op_assoc_e::LEFT, 12, false},
  // Comparison
  op_meta_t{op_operation_e::EQ,          op_type_e::BINARY, op_assoc_e::LEFT, 5, false},
  op_meta_t{op_operation_e::NEQ,         op_type_e::BINARY, op_assoc_e::LEFT, 5, false},
  op_meta_t{op_operation_e::GREATER,     op_type_e::BINARY, op_assoc_e::LEFT, 6, false},
  op_meta_t{op_operation_e::LESS,        op_type_e::BINARY, op_assoc_e::LEFT, 6, false},
  op_meta_t{op_operation_e::LEQ,         op_type_e::BINARY, op_assoc_e::LEFT, 6, false},
  op_meta_t{op_operation_e::GEQ,         op_type_e::BINARY, op_assoc_e::LEFT, 6, false},
  // Binary logic & shift
  op_meta_t{op_operation_e::NOT,         op_type_e::UNARY,  op_assoc_e::RIGHT, 10,false},
  op_meta_t{op_operation_e::AND,         op_type_e::BINARY, op_assoc_e::LEFT, 4,false},
  op_meta_t{op_operation_e::XOR,         op_type_e::BINARY, op_assoc_e::LEFT, 3,false},
  op_meta_t{op_operation_e::OR,          op_type_e::BINARY, op_assoc_e::LEFT, 2,false},
  op_meta_t{op_operation_e::SLEFT,       op_type_e::BINARY, op_assoc_e::LEFT, 7,false},
  op_meta_t{op_operation_e::SRIGHT,      op_type_e::BINARY, op_assoc_e::LEFT, 7,false},
  // Assignment
  op_meta_t{op_operation_e::ASSIGN,      op_type_e::BINARY, op_assoc_e::RIGHT, 0,false},
  op_meta_t{op_operation_e::PLUSASSIGN,  op_type_e::BINARY, op_assoc_e::RIGHT, 0,false},
  op_meta_t{op_operation_e::MINUSASSIGN, op_type_e::BINARY, op_assoc_e::RIGHT, 0,false},
  op_meta_t{op_operation_e::MULTASSIGN,  op_type_e::BINARY, op_assoc_e::RIGHT, 0,false},
  op_meta_t{op_operation_e::DIVASSIGN,   op_type_e::BINARY, op_assoc_e::RIGHT, 0,false},
  // Misc
  op_meta_t{op_operation_e::PIPE,        op_type_e::BINARY, op_assoc_e::LEFT, 1, false},
  op_meta_t{op_operation_e::NEG,         op_type_e::UNARY,  op_assoc_e::RIGHT, 11, false}, 
  op_meta_t{op_operation_e::DEREF,       op_type_e::UNARY,  op_assoc_e::RIGHT, 11, false},
  op_meta_t{op_operation_e::ADDRESS,     op_type_e::UNARY,  op_assoc_e::RIGHT, 11, false},
  op_meta_t{op_operation_e::AS,          op_type_e::UNARY,  op_assoc_e::RIGHT, 10, true},
};
struct operand_t;
struct operator_t;

// struct operator_base_t {
//   struct as_payload_t {
//     sptr<type_t> type;
//   };
//   union payload_t {
//     empty_t empty;
//     as_payload_t as;
//   };
//   op_operation_e type;
//   payload_t payload;

//   // RAWDOGGING THE UNION
//   auto get_as_payload() -> as_payload_t & { return payload.as; }

//   const op_meta_t &meta() { return op_table.at(static_cast<size_t>(type)); }
// };

struct uopt_t{
  ssptr<operand_t, expr_elm_t> operand;
};
struct bopt_t{
  ssptr<operand_t, expr_elm_t> lhs;
  ssptr<operand_t, expr_elm_t> rhs;
};

using operator_var = var<empty_t, uopt_t, bopt_t>;
struct operator_t : public operator_var {
  using operator_var ::variant;
  using operator_var ::operator=;
  struct as_payload_t {
    sptr<type_t> type;
  };
  union payload_t {
    empty_t empty;
    as_payload_t as;
  };
  op_operation_e type;
  payload_t payload;

  operator_t(op_operation_e t, payload_t p)
      : operator_var(empty_t{}), type(t), payload(p) {}

  operator_t(op_operation_e t, payload_t p, operator_var val)
      : operator_var(val), type(t), payload(p) {}

  // RAWDOGGING THE UNION
  auto get_as_payload() -> as_payload_t & { return payload.as; }

  const op_meta_t &meta() { return op_table.at(static_cast<size_t>(type)); }

  operator_t &operator=(const auto &rhs) {
    static_cast<operator_var &>(*this) = rhs;
    return *this;
  }
  operator_t &operator=(auto &&rhs) {
    static_cast<operator_var &>(*this) = std::move(rhs);
    return *this;
  }
};

// struct operator_t {
//   struct as_payload_t {
//     sptr<type_t> type;
//   };
//   union payload_t {
//     empty_t empty;
//     as_payload_t as;
//   };
//   op_operation_e type;
//   payload_t payload;

//   // RAWDOGGING THE UNION
//   auto get_as_payload() -> as_payload_t & { return payload.as; }

//   const op_meta_t &meta() { return op_table.at(static_cast<size_t>(type)); }
// };


struct number_t {
  std::string_view val;
};

namespace post {

struct deref_t{};
struct address_t{};

struct index_access_t {
  sptr<expr_t> index;
  sptr<type_t> type; //maybe uncomment this?
};
struct fn_access_t {
  ssptr<decl_s::fn_decl_t, decl_t> val;
};
struct var_access_t {
  ssptr<decl_s::var_decl_t, decl_t> val;
};
struct rec_member_access_t {
  ssptr<decl_s::rec_member_t, decl_t> val;
};
} // namespace post

VAR_MACRO(postfix, post::index_access_t, post::var_access_t, post::fn_access_t, post::rec_member_access_t);

struct chain_t {
  list<postfix_t> chain;
};

struct pipe_t {
  sptr<expr_t> lhs;
  opt<chain_t> chain;
};

struct complit_t {
  sptr<type_t> type;
  list<sptr<expr_t>> init_vals;
};

struct sizeof_t {
  var<sptr<type_t>, sptr<expr_t>> val;
};

// this a chain like
struct result_t {
  sptr<expr_t> expr;
};
struct block_t {
  frame_t frame;
  block_t(sptr<locale_t> l, sptr<stmts_t> s) : frame(l, s) {}
  // plus whatever a chain has here
};

struct if_t {
  struct if_link_t {
    sptr<expr_t> ctrl_expr;
    sptr<stmts_t> stmts;
  };
  //we could change this to a static array
  list<if_link_t> ifs;
};

// chain like
struct fn_lit_t {
  ssptr<type_s::callable_t, type_t> sig;
  sptr<stmts_t> body;
};

struct subexpr_t {
  ssptr<subexpr_t, expr_elm_t> prev;
  sptr<expr_t> expr;
};

VAR_MACRO(operand, chain_t, fn_lit_t, if_t, complit_t, number_t, sizeof_t,
          result_t, block_t);
using var = var<empty_t, unresolved_t, subexpr_t, operator_t, operand_t>;
} // namespace expr_s
struct expr_elm_t : public expr_s::var {
  using expr_s::var::variant;
};
struct expr_t {
  sptr<expr_elm_t> val;
  // list<sptr<expr_elm_t>> exprs;
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

struct break_t{ 
  sptr<expr_t> expr;
};

struct unreachable_t{};
using var = var<empty_t, unreachable_t, break_t, unwrap_decl_arr_t, forloop_t, import_t, ret_t,
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
    operator bool(){return found;}
  };

  template <typename SPECIALISED_TYPE> struct slookup_t {
    const bool found;
    const path_t path;
    const ssptr<SPECIALISED_TYPE, decl_t> symbol;
  };

  sptr<locale_t> parent_;
  // list<sptr<locale_t>> children_;

  // general
  static sptr<locale_t> make_child(auto &ctx, sptr<locale_t> self) {
    auto ptr = ctx.make_sptr(locale_t{self, {}});
    // self->children_.push_back(ptr);
    return ptr;
  }

  sptr<locale_t> parent() { return parent_; }

  // implementation specific
  struct internal {
    std::unordered_map<std::string_view, entry_t> table = {};
    mutable std::mutex mutex = {};

    internal() = default;

    // Custom copy constructor
    internal(const internal &other) : table(other.table) {
      // mutex is default-constructed, not copied
    }

    // Custom copy assignment operator
    internal &operator=(const internal &other) {
      if (this != &other) {
        table = other.table;
        // mutex remains default
      }
      return *this;
    }
    static lookup_t ancestor_lookup(sptr<locale_t> self, path_t &path,
                                    const std::string_view name) {
      self->internals.mutex.lock();
      auto val = lookup<false>(self, path, name);
      self->internals.mutex.unlock();
      return val;
    }
    static lookup_t local_lookup(sptr<locale_t> self, path_t &path,
                                 const std::string_view name) {
      self->internals.mutex.lock();
      auto val = lookup<true>(self, path, name);
      self->internals.mutex.unlock();
      return val;

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
      mutex.lock();
      auto res = table.try_emplace(name, std::forward<entry_t>(val));
      mutex.unlock();
      return {res.second, name, res.first->second};
    }
    insert_t try_insert(std::string_view name, entry_t &val) {
      mutex.lock();
      auto res = table.try_emplace(name, val);
      mutex.unlock();
      return {res.second, name, res.first->second};
    }
  } internals;

  insert_t try_insert(std::string name, entry_t &&val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(std::string_view name, entry_t &&val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(std::string name, entry_t &val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(std::string_view name, entry_t &val) {
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
  template <typename Y, typename T> struct unresolved_call_t {
    ssptr<Y, T> ptr;
    std::function<void(ssptr<Y, T>)> call;
    void operator()() { call(ptr); }
  };

  var<unresolved_call_t<unresolved_t, type_t>,
      unresolved_call_t<decl_s::unresolved_t, decl_t>,
      unresolved_call_t<unresolved_t, expr_elm_t>>
      call;

  template <typename Y, typename T>
  auto get_ptr() -> opt<std::reference_wrapper<sptr<T>>> {
    if (rholds<unresolved_call_t<Y, T>>(call))
      return std::get<unresolved_call_t<Y, T>>(call).ptr;
    return std::nullopt;
  }
  template <typename Y, typename T>
  auto get_callback_obj()
      -> opt<std::reference_wrapper<unresolved_call_t<Y, T>>> {
    if (rholds<unresolved_call_t<Y, T>>(call))
      return std::get<unresolved_call_t<Y, T>>(call);
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
  const std::map<size_t, size_t> &toks_symetrical_map;

  std::atomic_uint64_t cindex;
  allocator_t &allocator;
  std::map<uintptr_t, resolve_callback_t> callback_map;

  using post_resolve_callback_t = std::function<void()>;
  std::list<post_resolve_callback_t> post_resolve_callbacks;

  std::unordered_map<uintptr_t, sptr<type_t>> expr_type_map;
  
  std::mutex mut; //not needed?

  context_t(const token_buffer_t &t, const std::map<size_t, size_t> &tsm, allocator_t &a)
      : toks(t), toks_symetrical_map(tsm), cindex(0), allocator(a),
        callback_map({}), mut() {}

  void push_recursion_check(post_resolve_callback_t callback) {
    post_resolve_callbacks.push_back(callback);
  }

  template <typename T> void insert_callback(sptr<T> ptr, auto callback) {
    mut.lock();
    callback_map.insert({(uintptr_t)ptr.get_ptr(), callback});
    mut.unlock();
  }

  context_t(const context_t &) = delete;
  context_t &operator=(const context_t &) = delete;

  size_t operator++() { return cindex++; }

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
  template <typename S, typename B> auto make_ssptr(S &val) -> ssptr<S, B> {
    return make_ptr<B>(allocator, val);
  }
  template <typename S, typename B> auto make_ssptr(S &&val) -> ssptr<S, B> {
    return make_ptr<B>(allocator, val);
  }
};

namespace symbols {
auto entry(allocator_t &allocator, const token_buffer_t &toks,
           const std::map<size_t, size_t> &toks_symetrical_map,
           grammar::node_t &root_node)
    -> std::tuple<sptr<locale_t>, sptr<stmts_t>>;
} // namespace symbols

void print(sptr<decl_t> &val, const size_t indent = 0);
void print(sptr<type_t> &val, const size_t indent = 0);
void print(sptr<expr_elm_t> &val, const size_t indent = 0);
void print(sptr<expr_t> &val, const size_t indent = 0);
void print(sptr<stmt_t> &val, const size_t indent = 0);
void print(sptr<stmts_t> &val, const size_t indent = 0);
void print(sptr<type_s::fnsig_t>&val, const size_t indent = 0);

struct template_init_list_t {
  using template_elm_t = var<sptr<type_t>, sptr<expr_t>>;
  list<template_elm_t> list;
};

} // namespace semantics
