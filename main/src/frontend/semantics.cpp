#include "semantics.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include <bits/types/locale_t.h>
#include <cstdint>
#include <iostream>
#include <ostream>
#include <set>
#include <stdexcept>
#include <tuple>
#include <utility>
#include <variant>

namespace semantics {
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

namespace expr_s {
auto token_to_operator(const tokc::e token) -> operator_t{
  using enum op_operation_e; 
  switch (token) {
  case tokc::EQUALS:          return {EQ, {empty_t{}}};
  case tokc::GEQUALS:         return {GEQ, {empty_t{}}};
  case tokc::LEQUALS:         return {LEQ, {empty_t{}}};
  case tokc::EMARKEQUALS:     return {NEQ, {empty_t{}}};
  case tokc::PLUSASIGN:       return {PLUSASSIGN, {empty_t{}}};
  case tokc::MINUSASIGN:      return {MINUSASSIGN, {empty_t{}}};
  case tokc::DIVASIGN:        return {DIVASSIGN, {empty_t{}}};
  case tokc::MULASIGN:        return {MULTASSIGN, {empty_t{}}};
  case tokc::ASIGN:           return {ASSIGN, {empty_t{}}};
  case tokc::PLUSPLUS:        return {PLUSPLUS, {empty_t{}}};
  case tokc::MINUSMINUS:      return {MINUSMINUS, {empty_t{}}};
  case tokc::GREATERGREATER:  return {SRIGHT, {empty_t{}}};
  case tokc::LESSLESS: 				return {SLEFT, {empty_t{}}};
  case tokc::LESSGREATER:     return {NEQ, {empty_t{}}};
  case tokc::XOR:             return {XOR, {empty_t{}}};
  case tokc::AND:             return {AND, {empty_t{}}};
  case tokc::OR:              return {OR, {empty_t{}}};
  case tokc::MODULO:          return {MOD, {empty_t{}}};
  case tokc::PLUS:            return {PLUS, {empty_t{}}};
  case tokc::MINUS:           return {MINUS, {empty_t{}}};
  case tokc::DIV:             return {DIV, {empty_t{}}};
  case tokc::MUL:             return {MULT, {empty_t{}}};
  case tokc::LESS:            return {LESS, {empty_t{}}};
  case tokc::GREATER:         return {GREATER, {empty_t{}}};
  case tokc::EMARK:           return {NOT, {empty_t{}}};
  // case tokc::PERISPOMENI:     return {DEREF, {empty_t{}}};
  case tokc::AMPERSAND:       return {ADDRESS, {empty_t{}}};
  case tokc::ANDASIGN:        return {ASSIGN, {empty_t{}}};
  case tokc::ORASIGN:         return {ASSIGN, {empty_t{}}};
  case tokc::MINUSGREATER:    return {PIPE, {empty_t{}}};
  default:throw std::runtime_error("This token is not an operator");
  }
}
} // namespace expr_s

namespace resolve {
auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t;
auto decl_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<decl_s::unresolved_t, decl_t> ptr) -> resolve_callback_t;
auto expr_elm_chain_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, expr_elm_t> ptr) -> resolve_callback_t;

namespace expresion_type_resolution_pass {
sptr<type_t> expresion_elm_operator_fn(context_t &ctx, sptr<locale_t> locale,
                                       expr_s::operator_t &val);

sptr<type_t> expresion_elm_operand_fn(context_t &ctx, sptr<locale_t> locale,
                                      expr_s::operand_t &val);

void expresion_elm_fn(context_t &ctx, sptr<locale_t> locale,
                      sptr<expr_elm_t> ptr);

void expresion_type_fn(context_t &ctx, sptr<locale_t> locale, sptr<expr_t> ptr);
} // namespace expresion_type_resolution_pass

} // namespace resolve

namespace symbols {

auto consume_stmts_fn(context_t &ctx, sptr<locale_t> loc, sptr<stmts_t> stmts,
                      span_t span) -> void;
auto type_fn(context_t &ctx, sptr<locale_t> loc, const median_t type_med)
    -> sptr<type_t>;
auto decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t>;
auto expr_val_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_t;
auto expr_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_t>;

template <void (*fn)(sptr<decl_t>, context_t &, sptr<locale_t>,
                     cursor_helper_t &)>
auto decl_spec_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t>;

auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void;

template <bool allow_init_val = true>
auto var_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                 cursor_helper_t &cursor) -> void;

auto todo_stmts_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  throw std::runtime_error(std::string(__func__) + " todo function");
}

struct deepcopy_t {
  using entry_ptr_t =
      varptr<locale_t, decl_t, type_s::template_args_t, type_s::fnsig_t, type_t,
             expr_elm_t, expr_t, stmt_t, stmts_t>;

  struct entry_t {
    entry_ptr_t newptr;
  };

  context_t &ctx;
  using umap_t = std::unordered_map<std::uintptr_t, entry_t>;
  umap_t map;

  template <typename T> struct lookup_t {
    bool found;
    sptr<T> val;
    operator bool() { return found; }
  };
  template <typename T> auto copy(sptr<T> ptr) -> sptr<T> {
    auto newptr = create_entry(ptr);
    insert2map(ptr);
    force_copy();
    return newptr;
  }
  template <size_t INDEX, size_t END, typename... T>
  auto tuple_copy(std::tuple<sptr<T>...> &query_tup,
                  std::tuple<sptr<T>...> &insert_tup) -> void {
    auto &query_val = std::get<INDEX>(query_tup);
    auto &insert_val = std::get<INDEX>(insert_tup);
    insert_val = create_entry(query_val);
    insert2map(query_val);

    if constexpr (INDEX + 1 >= END)
      return;
    else
      return tuple_copy<INDEX + 1, END, T...>(query_tup, insert_tup);
  }

  auto force_copy() -> void {
    for (auto &elm : map)
      ovisit(elm.second.newptr.ptr, [this](auto &val) { this->replace(val); });
  }

  template <typename... T>
  auto copy(std::tuple<sptr<T>...> query_tup) -> std::tuple<sptr<T>...> {
    using TUP_T = std::tuple<sptr<T>...>;
    TUP_T insert_tup;
    tuple_copy<0, (size_t)(std::tuple_size<TUP_T>())>(query_tup, insert_tup);

    for (auto &elm : map)
      ovisit(elm.second.newptr.ptr, [this](auto &val) { this->replace(val); });

    return insert_tup;
  }
  template <typename T> sptr<T> lookup(const sptr<T> ptr) {
    auto find = map.find((uintptr_t)(ptr.get_ptr()));
    if (find != map.end())
      return find->second.newptr.to_sptr<T>();
    return ptr;
  }

  template <typename T> sptr<T> create_entry(sptr<T> oldptr, sptr<T> newptr) {
    auto oldkey = reinterpret_cast<uintptr_t>(oldptr.get_ptr());
    if (map.contains(oldkey))
      return oldptr;

    if (!oldptr) [[unlikely]]
      return nullptr;

    map.emplace(oldkey, entry_t{newptr});

    return newptr;
  }

  private:
  template <typename T> sptr<T> create_entry(sptr<T> oldptr) {
    auto oldkey = reinterpret_cast<uintptr_t>(oldptr.get_ptr());
    if (map.contains(oldkey) || !oldptr) [[unlikely]]
      return oldptr;

    auto newptr = ctx.make_sptr<T>(oldptr.get_val());
    map.emplace(oldkey, entry_t{newptr});

    return newptr;
  }

  public:
  void insert2map(sptr<locale_t> val) {
    create_entry(val);
    insert2map(val.get_val());
  }
  void insert2map(locale_t &locale) {
    for (auto &elm : locale.internals.table)
      create_entry(elm.second);
  }
  void insert2map(sptr<stmts_t> val) {
    create_entry(val);
    insert2map(val.get_val());
  }
  void insert2map(sptr<decl_t> val) {
    create_entry(val);
    insert2map(val.get_val());
  }
  void insert2map(decl_s::fn_decl_t &val) {
    insert2map(val.body);
    if (val.sig.ptr())
      insert2map(val.sig.ptr());
  }
  void insert2map(decl_s::unwrap_decl_elm_t &val) { insert2map(val.init_val); }
  void insert2map(decl_s::field_t &val) {
    create_entry(val.decl.ptr());
    insert2map(*val.decl.ptr());
  }
  void insert2map(decl_t &val) {
    ovisit(
        val, 
        [this](decl_s::scope_decl_t &val) { insert2map(val); },
        [this](decl_s::var_decl_t &val) { insert2map(val); },
        [this](decl_s::field_t& val) { insert2map(val); },
        [this](decl_s::type_decl_t &val) { insert2map(val); },
        [this](decl_s::fn_decl_t &val) { insert2map(val); },
        [this](decl_s::unwrap_decl_elm_t &val) { insert2map(val); },
        [this](decl_s::template_var_input_t &val) { insert2map(val.ptr()); },
        [](decl_s::template_type_input_t &val) {}, [](auto &) {});
  }
  void insert2map(stmts_t &val) {
    for (auto &elm : val.stmts) {
      create_entry(elm);
      ovisit(
          *elm, [this](sptr<decl_t> &val) { insert2map(val); },
          [this](sptr<expr_t> &val) { insert2map(val); }, [](auto &val) {});
    }
  }

  void insert2map(type_s::rec_t &val) {
    insert2map(val.loc);
    // for (auto &elm : val.fields)
    //   insert2map(elm.ptr());
    // for (auto &elm : val.idecls)
    //   insert2map(elm);
  }
  void insert2map(type_s::tup_t &val) {
    for (auto &elm : val.types)
      insert2map(elm);
  }
  void insert2map(type_s::aggregate_t &val) {
    ovisit(val, [this](auto &val) { insert2map(val); });
  }
  void insert2map(type_s::fntemplate_t &val) {
    insert2map(val.ret_type);
    insert2map(val.locale);
  }
  void insert2map(type_s::fntype_t &val) {
    insert2map(val.ret_type);
    for (auto &elm : val.arg_types)
      insert2map(elm);
  }
  void insert2map(type_s::indirection_t &val) {
    // deepcopy because it is better to do so
    ovisit(
        val, 
        [this](type_s::ptr_t &val) { insert2map(val.type); },
        [this](type_s::iptr_t &val) { insert2map(val.type); },
        [this](type_s::array_t& val) { insert2map(val.len);      insert2map(val.type); },
        [](type_s::optr_t &val) {});
  }
  void insert2map(type_s::callable_t &val) {
    insert2map(val.get_locale());
    insert2map(val.sig);
  }
  void insert2map(type_s::template_type_input_t &val) {
    insert2map(val.ref.ptr());
  }
  void insert2map(sptr<type_t> val) {
    create_entry(val);
    if (!val) [[unlikely]] {
      return;
    }
    ovisit(
        val.get_val(), [this](type_s::fntype_t &val) { insert2map(val); },
        [this](type_s::fntemplate_t &val) { insert2map(val); },
        [this](type_s::aggregate_t &val) { insert2map(val); },
        [this](type_s::indirection_t &val) { insert2map(val); },
        [this](type_s::callable_t &val) { insert2map(val); },
        [this](type_s::template_type_input_t &val) { insert2map(val); },
        // [](type_s::type_ref_t &val) {
        //   /*
        //    skip because we either have already found it
        //    will find it
        //    or is external (we do not touch those)
        //    so when we do the replacement pass
        //    we will get it right
        //    we will make another copy of the reference holder though (the
        //    type_t that holds the type_ref_t)
        //   */
        // },
        [](auto &) {});
  }
  void insert2map(sptr<expr_elm_t> val) {
    create_entry(val);
    // TODO: since we haven't decided on the final ast layout we can't really do
    // this part yet
  }
  void insert2map(sptr<expr_t> val) {
    create_entry(val);
    insert2map(val->val);
  }
  void insert2map(sptr<type_s::fnsig_t> val) {
    create_entry(val);
    insert2map(val->locale);
    if (val->ret_type)
      insert2map(val->ret_type);

    // we might not need to do this because we have the locale
    if (val->template_args)
      insert2map(val->template_args);
    // for (auto &elm : val->template_args->args)
    //   ovisit(elm, [this](auto &val) { insert2map(val.ptr()); });
  }

  void insert2map(decl_s::var_decl_t &val) {
    if (val.type)
      insert2map(val.type);
    if (val.expr)
      insert2map(val.expr);
  }

  void insert2map(frame_t &val) {
    insert2map(val.locale);
    insert2map(val.stmts);
  }
  void insert2map(decl_s::scope_decl_t &val) { insert2map(val.frame); }

  void insert2map(sptr<type_s::template_args_t> &val) {
    create_entry(val);
    for (auto &elm : val->args)
      ovisit(elm, [this](auto &val) { insert2map(val.ptr()); });
  }

  void insert2map(decl_s::type_decl_t &val) {
    if (val.type)
      insert2map(val.type);
  }

  void insert2map(decl_s::template_stamp_decl_t &val) {
    if (val.type)
      insert2map(val.type);
    auto &mod = val.mod;
    insert2map(mod.locale);
    insert2map(mod.targs);
  }

  //     locale_t, decl_t, type_s::template_args_t, type_s::fnsig_t, type_t,
  //            expr_elm_t, expr_t, stmt_t, stmts_t;
  private:
  void replace(locale_t *ptr) {
    for (auto &elm : ptr->internals.table)
      elm.second.ptr = lookup(elm.second).ptr;
  }

  void replace(frame_t& val){
    if (val.locale)
      val.locale.ptr = lookup(val.locale).ptr;
    if (val.stmts)
      val.stmts.ptr = lookup(val.stmts).ptr;
  }
  void replace(decl_t *ptr) {
    ovisit(
        *ptr,
        [this](decl_s::var_decl_t &val) {
          if (val.type)
            val.type.ptr = lookup(val.type).ptr;
          if (val.expr)
            val.expr.ptr = lookup(val.expr).ptr;
        },
        [this](decl_s::field_t &val) {
          val.decl.ptr().ptr = lookup(val.decl.ptr()).ptr;
        },
        [this](decl_s::type_decl_t &val) {
          val.type.ptr = lookup(val.type).ptr;
        },
        [this](decl_s::template_stamp_decl_t &val) {
          if (val.type)
            val.type.ptr = lookup(val.type).ptr;

          val.mod.locale.ptr = lookup(val.mod.locale).ptr;
          val.mod.targs.ptr = lookup(val.mod.targs).ptr;
        },
        [this](decl_s::scope_decl_t &val) { replace(val.frame); },
        [this](decl_s::template_var_input_t &val) {
          if (val.val)
            val.val.ptr = lookup(val.val).ptr;
        },
        [](decl_s::template_type_input_t &val) {
          // TODO I am not sure how to handle this part yet
        },
        [this](decl_s::fn_decl_t &val) {
          if (val.sig.ptr())
            val.sig.ptr().ptr = lookup(val.sig.ptr()).ptr;
          if (val.body)
            val.body.ptr = lookup(val.body).ptr;
        },
        [](decl_s::unwrap_decl_elm_t &val) {}, [](auto &val) {});
  }
  void replace(empty_t *ptr) {}
  void replace(type_s::template_args_t *ptr) {}
  void replace(type_s::fnsig_t *ptr) {}
  void replace(type_t *ptr) {
    ovisit(
        *ptr,
        [this](type_s::indirection_t &val) {
          ovisit(
              val,
              [this](type_s::ptr_t &val) {
                val.type.ptr = lookup(val.type).ptr;
              },
              [this](type_s::iptr_t &val) {
                val.type.ptr = lookup(val.type).ptr;
              },
              [this](type_s::array_t &val) {
                val.type.ptr = lookup(val.type).ptr;
                val.len.ptr = lookup(val.len).ptr;
              },
              [](type_s::optr_t &) {});
        },
        [this](type_s::template_type_input_t &val) {
          val.ref.val.ptr = lookup(val.ref.val).ptr;
        },
        [this](type_s::fntemplate_t &val) {
          val.ret_type.ptr = lookup(val.ret_type).ptr;
          val.locale.ptr = lookup(val.locale).ptr;
          for (auto &elm : val.args)
            elm.val.ptr = lookup(elm.val).ptr;
        },
        [this](type_s::fntype_t &val) {
          for (auto &elm : val.arg_types)
            elm.ptr = lookup(elm).ptr;
          if (val.ret_type)
            val.ret_type.ptr = lookup(val.ret_type).ptr;
        },
        [this](type_s::type_ref_t &val) { val.ref.ptr = lookup(val.ref).ptr; },
        [](type_s::primitive_t &val) {
          // No need to do anything since templates don't affect them
        },
        [this](type_s::aggregate_t &val) {
          ovisit(
              val,
              [this](type_s::rec_t &val) {
                val.loc.ptr = lookup(val.loc).ptr;

                for (auto &elm : val.fields)
                  elm.ptr().ptr = lookup(elm.ptr()).ptr;

                for (auto &elm : val.idecls)
                  elm.ptr = lookup(elm).ptr;
              },
              [this](type_s::tup_t &val) {
                for (auto &elm : val.types) {
                  elm.ptr = lookup(elm).ptr;
                }
              });
        },
        [this](type_s::callable_t &val) {
          if (val.sig)
            val.sig = lookup(val.sig).ptr;
        },
        [](auto &) {});
  }

  void replace(expr_elm_t *ptr) {}
  void replace(expr_t *ptr) { ptr->val.ptr = lookup(ptr->val).ptr; }

  void replace(stmt_t *ptr) {
    ovisit(
        *ptr,
        [this](sptr<decl_t> &val) {
          if (val)
            val.ptr = lookup(val).ptr;
        },
        [this](sptr<expr_t> &val) {
          if (val)
            val.ptr = lookup(val).ptr;
        },
        [](auto &) {});
  }
  void replace(stmts_t *ptr) {
    for (auto &elm : ptr->stmts)
      elm.ptr = lookup(elm).ptr;
  }

  public:
};

auto rec2tup(context_t &ctx, sptr<type_t> type) -> sptr<type_t> {
  auto &rec =
      std::get<type_s::rec_t>(std::get<type_s::aggregate_t>(type.get_val()));

  auto newptr = ctx.make_sptr<type_t>(type_s::aggregate_t{type_s::tup_t{}});
  auto &tup =
      std::get<type_s::tup_t>(std::get<type_s::aggregate_t>(newptr.get_val()));

  for (auto &elm : rec.fields) {
    // if (!holds<decl_s::var_decl_t>(elm.get_val()))
    //   throw std::runtime_error("Can't convert rec to tup");

    auto &decl = elm.get().decl.get();
    if (decl.type.is_null())
      throw std::runtime_error("This shouldn't be here");

    tup.types.push_back(decl.type);
  }
  return newptr;
}

auto tup2rec(context_t &ctx, const sptr<type_t> type) -> sptr<type_t> {
  const auto &tup =
      std::get<type_s::tup_t>(std::get<type_s::aggregate_t>(type.get_val()));
  auto newptr = ctx.make_sptr<type_t>(type_s::aggregate_t{type_s::rec_t{}});

  auto &rec =
      std::get<type_s::rec_t>(std::get<type_s::aggregate_t>(newptr.get_val()));

  size_t n = 0;
  auto num2name = [](const size_t n) -> std::string {
    return "_" + std::to_string(n);
  };

  for (auto &elm : tup.types) {
    const auto id = n;
    const auto name = num2name(n++);
    auto ptr =
        ctx.make_sptr<decl_t>(decl_s::var_decl_t{true, elm, nullptr}, name, id);
    rec.loc->try_insert(name, ptr);
    rec.fields.push_back(ptr);
  }
  return newptr;
}

auto typeref_get_final(const type_s::type_ref_t& ptr) -> sptr<type_t> {
  auto &ref = ptr.ref;
  if (holds<type_s::type_ref_t>(ptr.ref.get_val()))
    return typeref_get_final(std::get<type_s::type_ref_t>(ref.get_val()));
  return ref;
}
template <typename Q>
auto typeref_holds(const type_s::type_ref_t &ptr) -> bool {
  return holds<Q>(typeref_get_final(ptr));
}

std::pair<size_t, sptr<type_t>>
indirection_final(sptr<type_t> ptr, const size_t n, const size_t limit) {
  using ret = std::pair<size_t, sptr<type_t>>;

  const auto typeref_visit = [&](type_s::type_ref_t &val) -> ret {
    return indirection_final(typeref_get_final(val), n, limit);
  };

  if (n >= limit) [[unlikely]]
    return ovisit(*ptr, typeref_visit,
                  [&](auto &val) -> ret { return {n, ptr}; });
  return ovisit(
      *ptr, typeref_visit,
      [&](type_s::indirection_t &val) -> ret {
        return ovisit(
            val, [&](type_s::optr_t &val) -> ret { return {n, ptr}; },
            [&](auto &val) -> ret {
              return indirection_final(val.type, n + 1, limit);
            });
      },
      [&](auto &val) -> ret { return {n, ptr}; });
}

auto fntemplate2fntype(context_t &ctx, type_s::fntemplate_t &fn_tem)
    -> type_s::fntype_t {
  deepcopy_t dp(ctx);

  dp.insert2map(fn_tem.ret_type);
  for (auto &elm : fn_tem.args)
    dp.insert2map(elm.get().type);
  dp.force_copy();

  auto new_ret_type = dp.lookup(fn_tem.ret_type);
  list<sptr<type_t>> new_type_list;
  for (auto &elm : fn_tem.args)
    new_type_list.push_back(dp.lookup(elm.get().type));

  return {std::move(new_type_list), new_ret_type};
}

auto fntemplate2fnsig(context_t &ctx, type_s::fntemplate_t &fn_tem)
    -> type_s::fnsig_t {
  deepcopy_t dp(ctx);
  dp.insert2map(fn_tem.locale);
  for (const auto &elm : fn_tem.args)
    dp.insert2map(elm.ptr());
  dp.insert2map(fn_tem.ret_type);
  dp.force_copy();

  auto new_locale = dp.lookup(fn_tem.locale);
  auto new_args = list<ssptr<decl_s::var_decl_t, decl_t>>{};
  for (const auto &elm : fn_tem.args)
    new_args.push_back(dp.lookup(elm.ptr()));
  auto new_ret_type = dp.lookup(fn_tem.ret_type);
  auto new_template_args = nullptr;
  auto new_val =
      type_s::fnsig_t{new_locale, new_template_args, new_args, new_ret_type};
  return new_val;
}

auto fntemplate2fn(context_t &ctx, type_s::fntemplate_t &fn_tem)
    -> type_s::callable_t {
  auto sig = symbols::fntemplate2fnsig(ctx, fn_tem);
  auto fn = (type_s::fn_t{});
  return {fn, ctx.make_sptr(sig)};
}
auto fntemplate2fndecl(context_t &ctx, type_s::fntemplate_t &fn_tem,
                       sptr<expr_t> init_expr)
    -> decl_s::fn_decl_t {
  auto sig = ctx.make_sptr(symbols::fntemplate2fnsig(ctx, fn_tem));
  auto fntype_ptr =
      ctx.make_sptr<type_t>(type_s::callable_t{type_s::fn_t{}, sig});
  auto fnval = decl_s::fn_decl_t{fntype_ptr, init_expr};
  return fnval;
}

auto fntemplate2fndecl(context_t &ctx, type_s::fntemplate_t &fn_tem,
                       median_t med)
    -> decl_s::fn_decl_t{
  auto sig = ctx.make_sptr(symbols::fntemplate2fnsig(ctx, fn_tem));
  auto fntype_ptr =
      ctx.make_sptr<type_t>(type_s::callable_t{type_s::fn_t{}, sig});

  auto init_expr =
      symbols::expr_fn(ctx, sig->get_locale(), med.fchild().as_median());

  auto fnval = decl_s::fn_decl_t{fntype_ptr, init_expr};
  return fnval;
}

auto get_decl_fin_fn(const bool localy_indistinct, context_t &ctx,
                     sptr<locale_t> loc, const final_t id_fin) -> sptr<decl_t> {
  const auto name_fin = id_fin;
  const auto name_str = ctx.toks.str(name_fin);

  auto [inserted, name, entry] =
      loc->try_insert(name_str, ctx.make_sptr<decl_t>(name_str, ++ctx));
  if (!inserted) {
    if (localy_indistinct) [[unlikely]]
      throw std::runtime_error(
          "declaration name must be unique in it's local scope");
  }
  return entry;
}

auto get_decl_fn(const bool localy_indistinct, context_t &ctx,
                 sptr<locale_t> loc, const median_t decl_med)
    -> std::tuple<sptr<decl_t>, cursor_helper_t> {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t{decl_med.children()};
  auto name_fin = cursor.extract<tokc::ID>();

  // grammar ensures that for us
  //  these are here just to make sure everything works
  //  in the future i will add a macro or smth to condionaly compile this

  if (!name_fin) [[unlikely]]
    throw std::runtime_error("Booba");

  auto ptr = get_decl_fin_fn(localy_indistinct, ctx, loc, name_fin.value());

  return {ptr, cursor};
}

auto template_arg_list_fn(context_t &ctx, sptr<locale_t> loc,
                          const median_t med) -> type_s::template_args_t {
  auto list = type_s::template_args_t::list_t{};
  for (auto &elm : med.children()) {
    auto med = elm.as_median().fchild().as_median();
    auto cursor = cursor_helper_t{med.children()};
    auto decl_ptr =
        get_decl_fin_fn(false, ctx, loc, cursor.extract<tokc::ID>().value());
    switch (med.type()) {
    case medianc::DECL:
      var_decl_fn(decl_ptr, ctx, loc, cursor);
      list.push_back(ssptr<decl_s::var_decl_t, decl_t>(decl_ptr));
      break;
    case medianc::TYPE_DECL:
      *decl_ptr = decl_s::template_type_input_t{};
      list.push_back(ssptr<decl_s::template_type_input_t, decl_t>(decl_ptr));
      break;
    default:
      std::unreachable();
      break;
    }
  }
  return {list};
}

auto rec_fn(context_t &ctx, sptr<locale_t> loc, const median_t rec_med)
    -> type_s::rec_t {
  auto cursor = cursor_helper_t{rec_med.children()};

  auto body_med = cursor.extract<medianc::BODY>();

  auto locale = locale_t::make_child(ctx, loc);
  auto fields = type_s::rec_t::fieldholder_t{};
  auto idecls = type_s::rec_t::internaldecls_t{};
  size_t n = 0;
  {
    auto ch = body_med->children();
    auto cursor = cursor_helper_t{ch};
    while (cursor.within()) {
      auto med = cursor.extract<medianc::ELEMENT>()->fchild().as_median();
      auto dptr = decl_fn(ctx, locale, med);

      if (rholds<decl_s::var_decl_t>(dptr.get_val())) {
        //SUBHUMAN HACK
        auto& name = dptr->name;
        auto mptr = ctx.make_sptr<decl_t>(decl_s::field_t{dptr, n++}, name, dptr->index());
        locale->internals.table.at(name) =
            mptr; // REPLACE THE DECALRATION IN THE LOCALE WITH THE MEMBER ONE
        fields.emplace_back(mptr);
      } else {
        idecls.emplace_back(dptr);
      }
    }
  }

  return type_s::rec_t{locale, std::move(fields), std::move(idecls)};
}

template <typename T>
auto indirection_fn(context_t &ctx, sptr<locale_t> loc, const median_t in_med)
    -> type_t {
  // ctx.dbg_add_call();
  static_assert(std::is_same_v<T, type_s::ptr_t> || std::is_same_v<T, type_s::iptr_t>,
                "indirection type must be a type_s::ptr_t or type_s::iptr_t");
  auto cursor = cursor_helper_t{in_med.children()};
  auto type_med = cursor.extract<medianc::TYPE>();
  return T{type_fn(ctx, loc, *type_med)};
}

auto tup_fn(context_t &ctx, sptr<locale_t> loc, const median_t tup_med)
    -> type_t {
  auto ch = tup_med.children();
  auto types = list<sptr<type_t>>{};
  for (auto &elm : ch) {
    auto elm_med = elm.as_median();
    auto elm_ch = elm_med.children();
    auto med = elm_ch.begin()->as_median();
    types.push_back(type_fn(ctx, loc, med));
  }
  return type_s::tup_t{std::move(types)};
}

//TODO
auto typeof_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  auto cursor = cursor_helper_t{med.children()};
  auto expr_med = cursor.extract<medianc::EXPR>();
  return empty_t{};
}

auto fntemplate_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t{med.children()};
  const auto [args_med, ret_med] =
      cursor.tuple_extract<medianc::FN_ARGS, medianc::FN_RET>();

  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;
  auto locale = locale_t::make_child(ctx, loc);
  {
    auto cursor = cursor_helper_t{args_med->children()};
    while (cursor.within()) {
      auto type_med = cursor.extract<medianc::ARGUMENT>();
      auto ptr = decl_spec_fn<var_decl_fn>(
          ctx, locale, type_med.value().fchild().as_median());
      args.emplace_back(ptr);
    }
  }

  if (ret_med)
    ret_type = type_fn(ctx, loc, ret_med->fchild().as_median());

  return type_s::fntemplate_t{locale, args, ret_type};
}

auto fntype_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  auto cursor = cursor_helper_t{med.children()};
  const auto [args_med, ret_med] =
      cursor.tuple_extract<medianc::FN_ARGS, medianc::FN_RET>();

  list<sptr<type_t>> args;
  sptr<type_t> ret_type;

  if (args_med) {
    auto cursor = cursor_helper_t{args_med->children()};
    while (cursor.within()) {
      auto type_med = cursor.extract<medianc::ARGUMENT>();
      auto ptr = type_fn(ctx, loc, type_med.value().fchild().as_median());
      args.push_back(ptr);
    }
  }

  if (ret_med)
    ret_type = type_fn(ctx, loc, ret_med->fchild().as_median());

  return type_s::fntype_t{std::move(args), ret_type};
}
auto callable_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t{med.children()};
  const auto [state_med, template_med, args_med, ret_med] =
      cursor.tuple_extract<medianc::FN_STATE_LIST,
                           medianc::TEMPLATE_ARGUMENT_LIST, medianc::FN_ARGS,
                           medianc::FN_RET>();

  bool is_closure = false;
  if (state_med) [[unlikely]]
    if (state_med->len() > 0)
      is_closure = true;

  auto locale = locale_t::make_child(ctx, loc);
  auto targs_list = ctx.make_sptr(type_s::template_args_t{});
  if (template_med) [[unlikely]]
    *targs_list = template_arg_list_fn(ctx, locale, template_med.value());

  list<ssptr<decl_s::var_decl_t, decl_t>> args;
  sptr<type_t> ret_type;

  if (args_med) {
    auto cursor = cursor_helper_t{args_med->children()};
    while (cursor.within()) {
      auto decl_med = cursor.extract<medianc::ARGUMENT>();
      // decl_spec_fn<var_decl_fn>(ctx, loc, decl_med);

      args.emplace_back(decl_spec_fn<var_decl_fn>(ctx, locale,
                                               decl_med->fchild().as_median()));
    }
  }

  if (ret_med)
    ret_type = type_fn(ctx, loc, ret_med->fchild().as_median());

  auto sig = ctx.make_sptr(
      type_s::fnsig_t{locale, targs_list, std::move(args), ret_type});

  if (!is_closure)
    return type_t{type_s::callable_t{type_s::fn_t{}, sig}};
  else
    return type_t{type_s::callable_t{type_s::closure_t{}, sig}};
}

struct recursion_check_context_t {
  std::set<uintptr_t> &forbiden;
  list<sptr<type_t>> current_path;
  list<list<sptr<type_t>>> recursive_paths;

  bool check(const sptr<type_t> &ptr) const {
    return forbiden.contains(reinterpret_cast<uintptr_t>(ptr.get_ptr()));
  }

  auto insert(const sptr<type_t> &ptr) {
    return forbiden.insert(reinterpret_cast<uintptr_t>(ptr.get_ptr()));
  }

  void enter(const sptr<type_t> &ptr) {
    current_path.push_back(ptr);
  }

  void leave() {
    current_path.pop_back();
  }

  void record_cycle(const sptr<type_t> &ptr) {
    current_path.push_back(ptr);
    recursive_paths.push_back(current_path);
    current_path.pop_back();  // remove again after recording
  }
};
bool try_insert(recursion_check_context_t &ctx, const sptr<type_t> &ptr) {
  auto [_, inserted] = ctx.insert(ptr);
  if (!inserted) {
    ctx.record_cycle(ptr);
    return false;
  }
  return true;
}
void recursion_check(recursion_check_context_t& ctx, const type_s::rec_t &rec,
                     const sptr<type_t> holder);
void recursion_check(recursion_check_context_t& ctx,
                     const type_s::type_ref_t &tref, const sptr<type_t> holder);
void recursion_check(recursion_check_context_t& ctx, 
                      const type_s::tup_t &tup,
                     const sptr<type_t> holder);
void recursion_check(recursion_check_context_t& ctx,
                     const type_s::aggregate_t &agg,
                     const sptr<type_t> holder);

void defvisit(const auto &val) {
  std::println(std::cerr, "type:{}", boost::core::demangled_name(typeid(val)));
}

template <typename T>
void recursion_check_next(recursion_check_context_t &ctx, const T &val,
                          const sptr<type_t> holder) {
  ovisit(
      val,
      [&ctx, &holder](const type_s::aggregate_t &val) {
        recursion_check(ctx, val, holder);
      },
      [&ctx, &holder](const type_s::type_ref_t &val) {
        recursion_check(ctx, val, holder);
      },
      [](const auto &) {});
}

void recursion_check(recursion_check_context_t &ctx,
                     const type_s::aggregate_t &agg,
                     const sptr<type_t> holder) {
  ovisit(
      agg, [](const type_s::optr_t &) {},
      [&ctx, &holder](const auto &val) { recursion_check(ctx, val, holder); });
}

void recursion_check(recursion_check_context_t &ctx, 
                     const type_s::tup_t &tup,
                     const sptr<type_t> holder) {
  if (!try_insert(ctx, holder)) [[unlikely]]
    return;

  ctx.enter(holder);

  for (const auto &type_ptr : tup.types) {
    recursion_check_next(ctx, *type_ptr, type_ptr);
  }
  ctx.leave();
}
void recursion_check(recursion_check_context_t &ctx,
                     const type_s::type_ref_t &tref,
                     const sptr<type_t> holder) {
  if (!try_insert(ctx, holder))
    return;
  ctx.enter(holder);

  recursion_check_next(ctx, *tref.ref, tref.ref);

  ctx.leave();
}
void recursion_check(recursion_check_context_t &ctx, const type_s::rec_t &rec,
                     const sptr<type_t> holder) {
  if (!try_insert(ctx, holder))
    return;
  ctx.enter(holder);

  for (const auto &mem : rec.fields) {
    ovisit(
        *mem.ptr(),
        [&ctx](const decl_s::var_decl_t &val) {
          const auto type_ptr = val.type;
          recursion_check_next(ctx, *type_ptr, type_ptr);
        },
        [](const auto &) {});
  }
  ctx.leave();
}
template <typename T>
void run_recursion_check(const T& type, sptr<type_t> holder,
                         void (*checker)(recursion_check_context_t&, const T&, sptr<type_t>)) {
  auto forbiden = std::set<uintptr_t>{};
  recursion_check_context_t ctx{forbiden, {}, {}};

  checker(ctx, type, holder);

  if (!ctx.recursive_paths.empty()) {
    std::cerr << "Detected recursion cycles:\n";
    for (const auto& path : ctx.recursive_paths) {
      std::cerr << "  Path:\n";
      for (const auto& node : path) {
        std::cerr << "    -> " << reinterpret_cast<void*>(node.get_ptr()) << "\n";
      }
    }
  }
}

void tup_recursion_check(const type_s::tup_t &tup, sptr<type_t> holder) {
  run_recursion_check(tup, holder, recursion_check);
}

void rec_recursion_check(const type_s::rec_t& rec, sptr<type_t> holder) {
  run_recursion_check(rec, holder, recursion_check);
}

void type_ref_recursion_check(const type_s::type_ref_t& tref, sptr<type_t> holder) {
  run_recursion_check(tref, holder, recursion_check);
}

auto type_fn(context_t &ctx, sptr<locale_t> loc, const median_t type_med)
    -> sptr<type_t> {
  const auto node = type_med.children().begin()->node();
  auto ptr = ctx.make_sptr(type_t{});
  auto val = ovisit(
      node,
      [&ctx](const final_t &fin) -> type_t {
        switch (fin->type()) {
        case tokc::BUILTIN_PTR:
          return type_s::optr_t{};
        case tokc::BUILTIN_VOID:
          return type_s::void_t{};
        case tokc::TYPE_INT:
          return type_s::sint_t{ctx.toks.str(fin)};
        case tokc::TYPE_UINT:
          return type_s::uint_t{ctx.toks.str(fin)};
        case tokc::TYPE_FLOAT:
          return type_s::float_t{ctx.toks.str(fin)};
        case tokc::TYPE_BOOLEAN:
          return type_s::boolean_t{8};
        default:
          std::unreachable();
          break;
          // throw std::runtime_error(std::string(token_code_str(fin->type())) +
          //                          " " + "unknown type (final_t)");
        }
      },
      [&ctx, &loc, &ptr](const median_t &val) -> type_t {
        auto type = val.type();
        switch (type) {
        case medianc::CHAIN: {
          *ptr = unresolved_t{val};
          ctx.insert_callback(
              ptr,
              resolve::type_fn(ctx, loc, ssptr<unresolved_t, type_t>(ptr)));
          // ctx.callbacks.push_back(
          //     resolve::type_fn(ctx, loc, ssptr<unresolved_t, type_t>(ptr)));
          return unresolved_t{val};
        }
        case medianc::FN_SIG:
          return fntype_fn(ctx, loc, val);
        case medianc::FN_TEMPLATE:
          return fntemplate_fn(ctx, loc, val);
        case medianc::PTR:
          return indirection_fn<type_s::ptr_t>(ctx, loc, val);
        case medianc::IMMUTABLE_PTR:
          return indirection_fn<type_s::iptr_t>(ctx, loc, val);
        case medianc::TUPLE:
          return tup_fn(ctx, loc, val);
        case medianc::RECORD: {
          auto rec = rec_fn(ctx, loc, val);
          ctx.push_recursion_check([&ctx, ptr]() -> void {
            // this could be a problem for templates???
            auto rec = std::get<type_s::rec_t>(
                std::get<type_s::aggregate_t>(ptr.get_val()));
            rec_recursion_check(rec, ptr);
          });
          return rec;
        }
        case medianc::TYPEOF:
          return typeof_fn(ctx, loc, val);
        default:
          std::unreachable();
          break;
          // throw std::runtime_error(std::string(medianc::str(type)) + " " +
          //                          std::string("unknown type (median_t)"));
        }
      },
      [](const auto &val) -> type_t { std::unreachable(); });
  *ptr = val;
  return ptr;
}

// this can be a function or a variable
//  this is because i want to be able to use type aliases to be able to declare
//  functions which kindof complicates things
template <bool allow_init_val>
auto var_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                 cursor_helper_t &cursor) -> void {


  const auto [mut_fin, type_med, val_med] =
      cursor.tuple_extract<tokc::any, medianc::TYPE, medianc::VALUE>();
  const auto mut_val = [&mut_fin] -> bool {
    constexpr bool default_mut = false;
    if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_IMMUTABLE))
      return false;
    else if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_MUTABLE))
      return true;
    else
      return default_mut;
  }();

  if constexpr (true) {
    // make a callback to resolve later the declaration
    auto type_val = sptr<type_t>{nullptr};
    {
      // std::println(std::cerr,std::cerr,"{}", medianc::str(type_med->type()));
      if (!type_med) {
        type_val = ctx.make_sptr<type_t>(type_s::infered_t{}); // no type
      } else if (type_med->fchild().is_median() &&
                 type_med->fchild().as_median().type() == medianc::CHAIN) {
        // we can't resolve without it having effects on the value so we move
        // this to the callback stage
        // we asign a monostate to the ptr or unresolved
        *ptr = decl_s::unresolved_t{type_med->fchild().as_median(), val_med};
        ctx.insert_callback(ptr, resolve::decl_fn(ctx, loc, ptr));
        return;
      } else {
        type_val = type_fn(ctx, loc,
                           type_med.value()); // resolve the literal type
      }

      sptr<expr_t> init_val;
      {
        if (val_med) {
          if constexpr (!allow_init_val) {
            throw std::runtime_error("Init values on variable declaration are "
                                     "conditionally not allowed");
          } else {
            init_val = expr_fn(ctx, loc, val_med->fchild().as_median());
          }
        }
      };
      *ptr = decl_s::var_decl_t{mut_val, type_val, init_val};
      return;
    }
  } else {
    sptr<type_t> type_val;
    {
      if (!type_med) {
        type_val = ctx.make_sptr<type_t>(type_s::infered_t{}); // no type
      } else {
        type_val = type_fn(ctx, loc,
                           type_med.value()); // resolve the literal type
      }
    };

    sptr<expr_t> init_val;
    {
      if (val_med) {
        if constexpr (!allow_init_val) {
          throw std::runtime_error("Init values on variable declaration are "
                                   "conditionally not allowed");
        } else {
          init_val = expr_fn(ctx, loc, val_med->fchild().as_median());
        }
      }
    };

    *ptr = decl_s::var_decl_t{mut_val, type_val, init_val};
  }
}

auto scope_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                   cursor_helper_t &cursor) -> void {
  sptr<locale_t> locale;
  sptr<stmts_t> stmts;
  if (std::holds_alternative<decl_s::scope_decl_t>(*ptr)) {
    auto &scope = std::get<decl_s::scope_decl_t>(*ptr);
    locale = scope.frame.locale;
    stmts = scope.frame.stmts;
  } else {
    locale = locale_t::make_child(ctx, loc);
    stmts = ctx.make_sptr(stmts_t{});
    *ptr = decl_s::scope_decl_t{locale, stmts};
  }
  const auto body = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body->children());
}

auto fn_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                cursor_helper_t &cursor) -> void {
  auto [sig_med, body_med] =
      cursor.tuple_extract<medianc::FN_SIG, medianc::BODY>();
  auto callable_type = callable_fn(ctx, loc, sig_med.value());
  auto callable_type_ptr = ctx.make_sptr<type_t>(callable_type);
  auto sig = ssptr<type_s::callable_t, type_t>{callable_type_ptr};

  auto body = expr_fn(ctx, sig.get().get_locale(),
                      body_med.value().fchild().as_median());

  *ptr = decl_s::fn_decl_t{sig.ptr(), body};
}

auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void {
  auto [template_args_med, type_med] =
      cursor.tuple_extract<medianc::TEMPLATE_ARGUMENT_LIST, medianc::TYPE>();
  auto type = sptr<type_t>{nullptr};
  auto locale = loc;
  if (template_args_med) {
    auto template_list = sptr<type_s::template_args_t>{nullptr};
    auto new_locale = locale_t::make_child(ctx, loc);

    template_list = ctx.make_sptr(
        template_arg_list_fn(ctx, new_locale, template_args_med.value()));

    locale = new_locale;

    auto template_mod = decl_s::template_stamp_decl_t::template_module_t{
        new_locale, template_list};
    *ptr = decl_s::template_stamp_decl_t{
        template_mod, type_fn(ctx, locale, type_med.value())};
  } else {
    *ptr = decl_s::type_decl_t{type_fn(ctx, locale, type_med.value())};
  }
}

template <void (*fn)(sptr<decl_t>, context_t &, sptr<locale_t>,
                     cursor_helper_t &)>
auto decl_spec_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t> {
  constexpr auto localy_indistinct = (fn != scope_decl_fn);
  auto [ptr, cursor] = get_decl_fn(localy_indistinct, ctx, loc, decl_med);
  // std::println(std::cerr,std::cerr,"{}", ptr->name);

  fn(ptr, ctx, loc, cursor);
  return ptr;
}

auto unwrap_decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<stmt_t> {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t(decl_med.children());
  const auto [ids_med, body_med] =
      cursor.tuple_extract<medianc::UNWRAP_IDS, medianc::UNWRAP_BODY>();

  auto arr_ptr = ssptr<stmt_s::unwrap_decl_arr_t, stmt_t>(
      ctx.make_sptr<stmt_t>(stmt_s::unwrap_decl_arr_t{}));

  auto value_ptr = ctx.make_sptr<expr_t>();
  {
    auto id_span = ids_med->children();
    size_t elmindex = 0;
    for (auto &elm : id_span) {
      const auto fin = elm.as_final();
      auto ptr = get_decl_fin_fn(false, ctx, loc, fin);
      *ptr = decl_s::unwrap_decl_elm_t{elmindex, value_ptr};
      arr_ptr.get().wraps.push_back(ptr);
      ++elmindex;
    }
  }
  *value_ptr = expr_val_fn(ctx, loc, body_med.value().fchild().as_median());
  return arr_ptr.ptr();
}

auto import_fn(context_t &ctx, const median_t import_med) -> sptr<stmt_t> {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t{import_med.children()};
  auto strlit_node = cursor.extract<tokc::STRLIT>();

  return ctx.make_sptr<stmt_t>(
      stmt_s::import_t{ctx.toks.str(strlit_node.value())});
}

auto forloop_fn(context_t &ctx, sptr<locale_t> ploc, const median_t for_med)
    -> sptr<stmt_t> {
  // ctx.dbg_add_call();
  auto cursor = cursor_helper_t{for_med.children()};
  auto [ctrl_expr_med, body_med] =
      cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

  auto locale = locale_t::make_child(ctx, ploc);
  // ctrl_expr
  sptr<stmts_t> ctrl_expr_stmts;
  {
    auto stmt_list = ctx.make_sptr(stmts_t{});
    auto cursor = cursor_helper_t{ctrl_expr_med->children()};

    auto [decl_med, cmp_expr_med, it_expr_med] =
        cursor.tuple_extract<medianc::DECL, medianc::EXPR, medianc::EXPR>();

    auto decl_ptr = decl_spec_fn<var_decl_fn>(ctx, locale, decl_med.value());
    auto cmp_expr_ptr = expr_fn(ctx, locale, cmp_expr_med.value());
    auto it_expr_ptr = expr_fn(ctx, locale, it_expr_med.value());

    stmt_list->stmts.emplace_back(ctx.make_sptr<stmt_t>(decl_ptr));
    stmt_list->stmts.emplace_back(ctx.make_sptr<stmt_t>(cmp_expr_ptr));
    stmt_list->stmts.emplace_back(ctx.make_sptr<stmt_t>(it_expr_ptr));
    ctrl_expr_stmts = stmt_list;
  }
  // body
  sptr<stmts_t> body_stmts;
  {
    auto stmts = ctx.make_sptr(stmts_t{});
    auto span = body_med->children();
    consume_stmts_fn(ctx, locale, stmts, span);
    body_stmts = stmts;
  }
  return ctx.make_sptr<stmt_t>(
      stmt_s::forloop_t{locale, ctrl_expr_stmts, body_stmts});
}

auto as_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::operator_t::as_payload_t {
  auto type_med = med.fchild().as_median();
  return expr_s::operator_t::as_payload_t{type_fn(ctx, loc, type_med)};
}

auto expr_operator_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
  auto node = med.fchild().node();
  auto base = ovisit(
      node,
      [](const final_t &val) -> expr_s::operator_t {
        return expr_s::token_to_operator(val->type());
      },
      [&ctx, &loc](const median_t &val) -> expr_s::operator_t {
        switch (val.type()) {
        case medianc::AS:
          return expr_s::operator_t{
              expr_s::op_operation_e::AS,
              expr_s::operator_t::payload_t{.as = as_fn(ctx, loc, val)}};
        default:
          std::unreachable();
        }
      },
      [](const auto &) -> expr_s::operator_t { std::unreachable(); });
  auto& meta = base.meta();
  base = [&] -> expr_s::operator_var {
    if (meta.type == expr_s::op_type_e::BINARY) {
      return expr_s::bop_t{{nullptr}, {nullptr}};
    } else if (meta.type == expr_s::op_type_e::UNARY) {
      return expr_s::uop_t{{nullptr}};
    } else {
      std::unreachable();
    }
  }();
  return ctx.make_sptr<expr_elm_t>(base, ctx.make_sptr<type_t>(empty_t{}));
}

// TODO add the chain
// auto self_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
//     -> expr_s::self{
//   return empty_t{};
// }

// TODO add the chain
auto block_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::block_t {
  auto locale = locale_t::make_child(ctx, loc);
  auto stmts = ctx.make_sptr(stmts_t{});
  auto cursor = cursor_helper_t{med.children()};

  auto body_med = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body_med->children());

  return {locale, stmts};
}

auto result_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::result_t {
  auto cursor = cursor_helper_t{med.children()};

  auto body_med = cursor.extract<medianc::BODY>();
  auto expr_ptr = expr_fn(ctx, loc, body_med.value().fchild().as_median());

  return {expr_ptr};
}

auto pipe_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::pipe_t{
  /* auto ch = med.children();
  auto cursor = ch.begin();
  while (ch.contain(cursor)) {
    cursor.advance();
  } */
  return expr_s::pipe_t{{nullptr}, {std::nullopt}};
}

auto sizeof_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::sizeof_t {
  auto decl_or_type = med.fchild().as_median();
  switch (decl_or_type.type()) {
  case medianc::TYPE:
    return expr_s::sizeof_t{type_fn(ctx, loc, decl_or_type)};
  case medianc::EXPR:
    return expr_s::sizeof_t{expr_fn(ctx, loc, decl_or_type)};
  default:
    std::unreachable();
  }
}

auto complit_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::var {
  auto cursor = cursor_helper_t{med.children()};
  auto [ctype_med, cinit_med] =
      cursor.tuple_extract<medianc::COMPOUND_LITERAL_TYPE,
                           medianc::COMPOUND_LITERAL_INIT>();

  auto type = type_fn(ctx, loc, ctype_med->fchild().as_median());

  // if (cinit_med->children().begin()->as_median().type() == medianc::STMT) {
  if (rholds<type_s::callable_t>(type->base())) {
    auto locale = ovisit(
        type->base(),
        [](type_s::callable_t &val) -> sptr<locale_t> {
          return val.get_locale();
        },
        [](const auto &val) -> sptr<locale_t> { std::unreachable(); });
    auto stmts = ctx.make_sptr(stmts_t{});
    consume_stmts_fn(ctx, locale, stmts, cinit_med->children());

    return expr_s::fn_lit_t{type, stmts};
  } else {
    list<sptr<expr_t>> exprs;
    auto ch = cinit_med->children();
    auto cursor = ch.begin();
    while (ch.contains(cursor)) {
      auto med = cursor->as_median();
      // if (med.type() != medianc::EXPR)
      //   throw std::runtime_error(std::string(medianc::str(med.type())));

      exprs.push_back(expr_fn(ctx, loc, med));
      cursor.advance();
    }
    return expr_s::complit_t{type, exprs};
  }
}

auto if_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::if_t {
  auto cursor = cursor_helper_t{med.children()};
  auto val = expr_s::if_t{{}};
  bool one_else = false;
  while (cursor.within()) {
    auto var_med =
        cursor.extract<medianc::any>();
    if (!var_med) [[unlikely]]
      throw std::runtime_error(std::string(__PRETTY_FUNCTION__) +
                               " This shouldn't happen");

    if (var_med->type() == medianc::IF) {
      auto cursor = cursor_helper_t{var_med->children()};
      auto [ctrl_expr_med, body_med] =
          cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();
      auto elif = expr_s::if_t::if_link_t{nullptr, ctx.make_sptr(stmts_t{})};
      elif.ctrl_expr = expr_fn(
          ctx, loc, ctrl_expr_med.value().children().begin()->as_median());
      consume_stmts_fn(ctx, loc, elif.stmts, body_med->children());
      val.ifs.emplace_back(std::move(elif));
    } else if (var_med->type() == medianc::ELSE) {

      {
        if (cursor.within()) [[unlikely]]
          throw std::runtime_error(
              "The else part of the if expr should be last");
        if (one_else) [[unlikely]]
          throw std::runtime_error(
              "Can't have more than one else in a if expr");
      }

      one_else = true;
      auto body_med =
          cursor_helper_t{var_med->children()}.extract<medianc::BODY>();
      auto el = expr_s::if_t::if_link_t{{nullptr}, ctx.make_sptr(stmts_t{})};
      consume_stmts_fn(ctx, loc, el.stmts, body_med->children());
      val.ifs.push_back({el});
    } else {
      throw std::runtime_error(std::string(__PRETTY_FUNCTION__) +
                               "This shouldn't happen");
    }
  }
  return val;
}
auto expr_operand_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
  // ctx.dbg_add_call();
  auto node = med.children().begin()->node();
  auto ptr = ctx.make_sptr<expr_elm_t>();
  auto var_val = ovisit(
      node,
      [&ctx](const final_t &val) -> expr_s::var {
        switch (val->type()) {
        // this is kind of retarded but I am not sure
        // how to represent it
        case tokc::INT:
          return expr_s::number_t{expr_s::int_t{}, ctx.toks.str(val)};
        case tokc::FLOAT:
          return expr_s::number_t{expr_s::float_t{}, ctx.toks.str(val)};
        default:
          std::unreachable();
        }
      },
      [&ctx, &loc, &ptr](const median_t &val) -> expr_s::var {
        switch (val.type()) {
        case medianc::CHAIN:
          // TODO:
          ctx.insert_callback(ptr, resolve::expr_elm_chain_fn(ctx, loc, ptr));
          return unresolved_t{val};
        case medianc::RESULT:
          return result_fn(ctx, loc, val);
        case medianc::BLOCK_EXPR:
          return block_fn(ctx, loc, val);
        case medianc::SIZEOF:
          return sizeof_fn(ctx, loc, val);
        case medianc::COMPOUND_LITERAL:
          return complit_fn(ctx, loc, val);
        case medianc::IF_EXPR:
          return if_fn(ctx, loc, val);
        case medianc::PIPE:
          return pipe_fn(ctx, loc, val);
        // case medianc::SELF:
        //   return self_fn(ctx, loc, val);
        default:
          std::unreachable();
        }
      },
      [](const auto &val) -> expr_s::var{ std::unreachable(); });
  *ptr = expr_elm_t(var_val, ctx.make_sptr<type_t>(empty_t{}));
  return ptr;
}

auto expr_eval_fn(std::span<sptr<expr_elm_t>> queue) {
  auto stack = list<sptr<expr_elm_t>>{};
  stack.reserve(10);
  for(auto& ptr: queue){
    ovisit(
        *ptr, 
        [&](expr_s::operand_t&) { stack.push_back(ptr); },
        [&](unresolved_t&) { stack.push_back(ptr); },
        [&](empty_t&) { },
        [&](expr_s::operator_t &val) {
          ovisit(val, 
            [&](expr_s::bop_t &op) {
              if(stack.size() < 2)
                throw std::runtime_error("Not enough operands");
              op.rhs = std::move(stack.back()); stack.pop_back();
              op.lhs = std::move(stack.back()); stack.pop_back();
            }, 
            [&](expr_s::uop_t &op) {
              if(stack.size() < 1)
                throw std::runtime_error("Not enough operands");
              op.operand= stack.back(); stack.pop_back();
            },
            [](auto& val){}
          );
          stack.push_back(ptr);
        },
        [](auto &val) {std::unreachable();});
  }

  if (stack.size() != 1)
    throw std::runtime_error("Expresion stack needs to be 1 otherwise we did "
                             "not consume every operand");

  return stack.front();
}
//add checking for prefix and postfix operators
//so they don't get treated as both
auto expr_val_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_t {
  auto ch = med.children();
  auto cursor = ch.begin();

  // Shunting yard algorithm:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm
  list<sptr<expr_elm_t>> queue;
  podlist_t<ssptr<expr_s::operator_t, expr_elm_t>> stack;
  queue.reserve(ch.size2());
  stack.resize(ch.size2());

  while (ch.contains(cursor)) {
    auto med = cursor->as_median();
    switch (med.type()) {
    case medianc::OPERAND: {
      auto op = expr_operand_fn(ctx, loc, med);
      queue.push_back(op);
    } break;
    case medianc::OPERATOR: {
      ssptr<expr_s::operator_t, expr_elm_t> op =
          expr_operator_fn(ctx, loc, med);
      const auto &o1m = op.get().meta();

      while (stack.size() > 0) {
        auto &o2 = stack.back();
        const auto &o2m = o2.get().meta();
        if (o1m.prec < o2m.prec ||
            (o1m.prec == o2m.prec &&
             o1m.assoc == expr_s::op_assoc_e::LEFT)) {
          queue.push_back(o2.ptr());
          stack.pop_back();
        } else {
          break;
        }
      }
      stack.push_back_assume_size(op);
    } break;
    default:
      std::unreachable();
      break;
    }
    cursor.advance();
  }

  for (auto it=  stack.end()-1; it >= stack.begin(); --it) 
    queue.push_back(it->ptr());
  stack.release();
  return {expr_eval_fn(queue), ctx.make_sptr<type_t>(empty_t{})};
  // return expr_t{queue};
}

auto expr_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_t> {
  auto ptr = ctx.make_sptr<expr_t>(expr_val_fn(ctx, loc, med));
  ctx.expresion_type_resolve_callbacks.push_back([&ctx, loc, ptr] {
    resolve::expresion_type_resolution_pass::expresion_type_fn(ctx, loc, ptr);
  });
  return ptr;
}

auto ret_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<stmt_t> {
  // ctx.dbg_add_call();
  auto expr_med = cursor_helper_t(med.children()).extract<medianc::EXPR>();
  return ctx.make_sptr<stmt_t>(
      stmt_s::ret_t{expr_fn(ctx, loc, expr_med.value())});
}
auto break_fn(context_t &ctx, sptr<locale_t> loc, const median_t med) -> sptr<stmt_t> {
  if(med.len() == 0){
    return ctx.make_sptr<stmt_t>(stmt_s::break_t{});
  }
  auto expr_med = med.fchild().as_median();
  auto  expr_ptr = expr_fn(ctx, loc, expr_med);
  return ctx.make_sptr<stmt_t>(stmt_s::break_t{expr_ptr});
}

auto decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t> {
  const auto type = decl_med.type();
  switch (type) {
  case medianc::DECL:
    return decl_spec_fn<var_decl_fn>(ctx, loc, decl_med);
  case medianc::FN_DECL:
    return decl_spec_fn<fn_decl_fn>(ctx, loc, decl_med);
  case medianc::SCOPE_DECL:
    return decl_spec_fn<scope_decl_fn>(ctx, loc, decl_med);
  case medianc::TYPE_DECL:
    return decl_spec_fn<type_decl_fn>(ctx, loc, decl_med);
  default:
    std::unreachable();
  }
}
auto stmt_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  // ctx.dbg_add_call();
  auto& n = stmt_med.fchild();
  if (n.is_median()) [[likely]] {
    const auto &med = n.as_median();
    const auto type = med.type();
    switch (type) {
    case medianc::DECL:
      return ctx.make_sptr<stmt_t>(decl_spec_fn<var_decl_fn>(ctx, loc, med));
    case medianc::FN_DECL:
      return ctx.make_sptr<stmt_t>(decl_spec_fn<fn_decl_fn>(ctx, loc, med));
    case medianc::SCOPE_DECL:
      return ctx.make_sptr<stmt_t>(decl_spec_fn<scope_decl_fn>(ctx, loc, med));
    case medianc::TYPE_DECL:
      return ctx.make_sptr<stmt_t>(decl_spec_fn<type_decl_fn>(ctx, loc, med));
    case medianc::UNWRAP_DECL:
      return unwrap_decl_fn(ctx, loc, med);
    case medianc::FOR:
      return forloop_fn(ctx, loc, med);
    case medianc::IMPORT:
      return import_fn(ctx, med);
    case medianc::RETURN:
      return ret_fn(ctx, loc, med);
    case medianc::EXPR:
      return ctx.make_sptr<stmt_t>(expr_fn(ctx, loc, med));
    case medianc::BREAK:
      return break_fn(ctx, loc, med);
    default:
      std::unreachable();
      break;
      // throw std::runtime_error(std::string(medianc::str(type)) + " " +
      // "Unknown stmt type");
    }
  } else {
    const auto &fin = n.as_final();
    switch (fin->type()) {
    case tokc::BUILTIN_UNREACHABLE:
      return ctx.make_sptr<stmt_t>(stmt_s::unreachable_t{});
    default: {
      throw std::runtime_error(std::string(__PRETTY_FUNCTION__) +
                               " This shouldn't happen");
      // std::unreachable();
    }
    }
  }
}

auto consume_stmts_fn(context_t &ctx, sptr<locale_t> loc, sptr<stmts_t> stmts,
                      span_t span) -> void {
  for (auto &elm : span)
    (stmts->stmts).push_back(stmt_fn(ctx, loc, elm.as_median()));
}

__attribute__((visibility("default"))) auto
entry(allocator_t &allocator, 
      const token_buffer_t &toks,
      const std::map<size_t, size_t> &toks_symetrical_map,
      grammar::node_t &root_node) 
  -> std::tuple<sptr<locale_t>, sptr<stmts_t>> {
  context_t ctx(toks, toks_symetrical_map, allocator);

  auto locale = ctx.make_sptr(locale_t{});
  auto stmts = ctx.make_sptr(stmts_t{});
  auto span = root_node.as_median().children();

  // this is the "expensive part"
  consume_stmts_fn(ctx, locale, stmts, span);

  for (auto &callback : ctx.callback_map)
    callback.second();

  for (const auto &callback : ctx.type_recursion_check_callbacks)
    callback();

  for (const auto &callback : ctx.expresion_type_resolve_callbacks)
    callback();


  return std::tuple{locale, stmts};
}

sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, sptr<decl_t> ptr);
sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, sptr<expr_elm_t> ptr);
sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, sptr<expr_t> ptr);

sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, sptr<expr_t> ptr){
  return ptr->type;
}

sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, expr_s::postfix_t& val) {
  return ovisit(
      val,
      [](expr_s::post::index_access_t& val) -> sptr<type_t> {
        return val.type;
      },
      [](expr_s::post::field_access_t &val) -> sptr<type_t> {
        return val.val.get().decl.get().type;
      },
      [](expr_s::post::var_access_t &val) -> sptr<type_t> {
        return val.val.get().type;
      },
      [](expr_s::post::fn_access_t &val) -> sptr<type_t> {
        return val.val.get().sig.ptr();
      },
      [](auto &val) -> sptr<type_t> { return {nullptr}; });
}
sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, expr_s::operand_t& val) {
  return ovisit(
      val,
      [&](expr_s::chain_t &val) -> sptr<type_t> {
        return get_type(ctx, locale, val.chain.back());
      },
      [&](expr_s::number_t &val) {
        return ctx.make_sptr<type_t>(type_s::float_t{64});
      },
      [&](expr_s::fn_lit_t& val){
        return val.sig.ptr();
      },
      [&](expr_s::complit_t& val){
        return val.type;
      },
      [&](expr_s::result_t& val){
        return get_type(ctx,locale, val.expr);
      },
      [](auto &val) -> sptr<type_t> { return {nullptr}; });
}
sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale,
                      sptr<expr_elm_t> ptr) {
  return ptr->type;
}
sptr<type_t> get_type(context_t &ctx, sptr<locale_t> locale, sptr<decl_t> ptr) {
  return ovisit(
      *ptr, 
      [](decl_s::var_decl_t &val) -> sptr<type_t> { return val.type; },
      [](decl_s::type_decl_t &val) -> sptr<type_t> { return val.type; },
      [](decl_s::field_t &val) -> sptr<type_t> { return val.decl.get().type; },
      [](decl_s::fn_decl_t &val) -> sptr<type_t> { return val.sig.ptr(); },
      [](auto &val) -> sptr<type_t> {
        throw std::runtime_error("Can't exctract type from this");
      });
}

} // namespace symbols

namespace resolve {
namespace resolve_callaback_s {
auto template_init_input_res(context_t &ctx, sptr<locale_t> locale,
                             type_s::template_args_t &targs, median_t med)
    -> template_init_list_t {
  auto nodes = ctx.allocator.podalloc<grammar::node_buffer_t>(
      [](grammar::node_buffer_t *ptr) { return [ptr] { ptr->release(); }; },
      grammar::node_buffer_t::create(32));
  auto gctx = grammar::context_t{ctx.toks, ctx.toks_symetrical_map, *nodes};
  auto ch = med.children();

  if (ch.size2() != targs.args.size())
    throw std::runtime_error(
        "Template init list doesn't match the input count of the template");

  auto cursor = cursor_helper_t{ch}; // we get ambigous medians out of this

  using elm_t = template_init_list_t::template_elm_t;
  template_init_list_t tlist;

  for (auto &targ : targs.args) {
    auto imed = cursor.extract<medianc::AMBIGUOUS>();
    {
      if (!imed)
        throw std::runtime_error(
            "something has gone wrong in the extraction of AMBIGUOUS");
    }
    auto internal_cursor = imed->first();
    // for now because of the way the parser works we just throw and have no
    // recovery (the same is for the whole program btw)
    tlist.list.push_back(ovisit(
        targ,
        [&gctx, &internal_cursor, &locale,
         &ctx](type_s::template_args_t::type_input_ptr &val) -> elm_t {
          grammar::type(gctx, internal_cursor); // these might not be correct
          // this is wrong because when inserting to the vector we have
          // everything together and we need the last top level median inserted
          // not the last element
          auto ptr =
              symbols::type_fn(ctx, locale, gctx.nodes.back().as_median());
          return ptr;
        },
        [&gctx, &internal_cursor, &ctx,
         &locale](type_s::template_args_t::var_input_ptr &val) -> elm_t {
          grammar::expr(gctx, internal_cursor);
          auto ptr =
              symbols::expr_fn(ctx, locale, gctx.nodes.back().as_median());
          return ptr;
        }));
  }
  return tlist;
}

template <bool doancestor>
auto type_impl_fn(context_t &ctx, sptr<locale_t> locale, span_t::iterator it,
                  const span_t::iterator end) -> sptr<type_t> {
  auto val = it->node();
  return ovisit(
      val,
      [&](final_t &val) -> sptr<type_t> {
        if (!val->isa(tokc::ID)) [[unlikely]]
          throw std::runtime_error(
              "Only IDs are supported in unresoved symbols");

        const auto str = ctx.toks.str(val);
        auto lookup = (doancestor == true)
                          ? locale_t::ancestor_lookup(locale, str)
                          : locale_t::local_lookup(locale, str);

        if (!lookup.found) [[unlikely]] {
          throw std::runtime_error("Failed to find symbol " + std::string(str) +
                                   " path: ");
        }

        if (std::ptrdiff_t(end.ptr - it.ptr)) {
          if (auto next = it.next(); next->is_median()) {
            auto med = next->as_median();
            if (med.type() == medianc::TEMPLATE_INSTATIATION) [[unlikely]] {
              // auto template_init_list =
              return ovisit(
                  *lookup.symbol,
                  [&locale, &ctx,
                   &med](decl_s::template_stamp_decl_t &val) -> sptr<type_t> {
                    auto til = template_init_input_res(ctx, locale,
                                                       *val.mod.targs, med);

                    std::println(std::cerr, "til size:{}", til.list.size());

                    return val.type;
                  },
                  [](auto &) -> sptr<type_t> {
                    throw std::runtime_error(
                        "This Symbol does not support templates");
                  });
              // throw std::runtime_error("This type is a template, which are "
              //                          "currently not currently supported");

              // this means we have to limit what type of symbol is valid
              //  only type declarations are valid
              // and then we have to parse the template_instatiation according
              // to the template parameters that we have, do type parsing for
              // type inputs, expresion parsing for value inputs
              // convert the frontend to a big blob of a lib
              // so everything is easier
            }
          }
        }
        return ovisit(
            *lookup.symbol,
            [&](decl_s::scope_decl_t &val) -> sptr<type_t> {
              if (it.next() >= end)
                throw std::runtime_error("scopes can't be types");
              return type_impl_fn<false>(ctx, val.get_locale(), it.next(), end);
            },
            [&](decl_s::template_type_input_t &val) -> sptr<type_t> {
              return ctx.make_sptr<type_t>(
                  type_s::template_type_input_t{lookup.symbol});
            },
            [&](decl_s::type_decl_t &val) -> sptr<type_t> {
              if (it.next() >= end)
                return val.type;
              return ovisit(
                  *val.type,
                  [&](type_s::aggregate_t &val) -> sptr<type_t> {
                    return ovisit(
                        val,
                        [&](type_s::rec_t &val) -> sptr<type_t> {
                          return type_impl_fn<false>(ctx, val.loc, it.next(),
                                                     end);
                        },
                        [&](auto &) -> sptr<type_t> {
                          throw std::runtime_error(
                              "Not supported aggregate type");
                        });
                  },
                  [&](type_s::type_ref_t &val) -> sptr<type_t> {
                    return val.ref;
                  },
                  [&](unresolved_t &v) -> sptr<type_t> {
                    auto lookup =
                        (doancestor == true)
                            ? locale_t::ancestor_lookup(
                                  locale, ctx.toks.str(v.med.first()))
                            : locale_t::local_lookup(
                                  locale, ctx.toks.str(v.med.first()));
                    if (!lookup.found)
                      throw std::runtime_error("Unable to find symbol");

                    auto node =
                        ctx.callback_map.find((uintptr_t)(val.type.get_ptr()));

                    if (node == ctx.callback_map.end()) [[unlikely]]
                      throw std::runtime_error("Unknown symbol");

                    node->second();

                    auto &callback = node->second;

                    using ret = sptr<type_t>;
                    const auto type_lam = [&](sptr<type_t> &type,
                                              auto &self) -> ret {
                      return ovisit(
                          *type,
                          [&](type_s::type_ref_t &val) -> ret {
                            return type;
                          },
                          [&](type_s::aggregate_t &val) -> ret {
                            return ovisit(
                                val,
                                [&](type_s::rec_t &val) -> ret {
                                  return type_impl_fn<false>(
                                      ctx, val.get_locale(), it.next(), end);
                                },
                                [](auto &) -> ret {
                                  throw std::runtime_error(
                                      "call_t::type_t::aggregate_t Iliigal "
                                      "type");
                                });
                          },
                          [](auto &) -> ret {
                            throw std::runtime_error(
                                "call_t::type_t Illigal type");
                          });
                    };

                    auto type_ptr = callback.visit(
                        [&](resolve_callback_t::unresolved_call_t<
                            unresolved_t, type_t> &val) -> ret {
                          auto ptr = val.ptr.ptr();
                          if (it >= end)
                            return ptr;
                          return type_lam(ptr, type_lam);
                        },
                        [&](resolve_callback_t::unresolved_call_t<
                            decl_s::unresolved_t, decl_t> &val) -> ret {
                          auto ptr = val.ptr.ptr();
                          return ovisit(
                              *ptr,
                              [&](decl_s::scope_decl_t &val) -> ret {
                                return type_impl_fn<false>(
                                    ctx, val.get_locale(), it.next(), end);
                              },
                              [&](decl_s::type_decl_t &val) -> ret {
                                return type_lam(val.type, type_lam);
                              },
                              [](auto &) -> ret {
                                throw std::runtime_error(
                                    "call_t::decl_t, Illigal type");
                              });
                        },
                        [](auto &val) -> ret {
                          throw std::runtime_error(
                              std::string(__PRETTY_FUNCTION__) +
                              " This shouldn't happen");
                        });
                    return type_ptr;
                  },
                  [](auto &val) -> sptr<type_t> {
                    throw std::runtime_error(
                        "Type '" + boost::core::demangled_name(typeid(val)) +
                        "' can't be queried.");
                  });
            },
            [&](auto &value) -> sptr<type_t> {
              throw std::runtime_error(
                  "Type '" + std::string(str) + " " +
                  boost::core::demangled_name(typeid(value)) +
                  "' is not supported as a valid symbol to extract a type "
                  "from.");
            });
      },
      [&](median_t &val) -> sptr<type_t> {
        std::unreachable();
        throw std::runtime_error(
            "For now type resolution of unresoved symbols is not supported "
            "for median values");
      },
      [](auto &) -> sptr<type_t> { throw std::runtime_error("Unreachable"); });
}

auto type_ptr_asignment(context_t &ctx, sptr<locale_t> locale,
                        ssptr<unresolved_t, type_t> ptr) -> sptr<type_t> {
  if (rholds<unresolved_t>(*ptr.ptr())) [[likely]] {
    auto span = ptr.get().med.children();
    // creating type_ref_t
    *(ptr.ptr()) = type_s::type_ref_t{resolve_callaback_s::type_impl_fn<true>(
        ctx, locale, span.begin(), span.end())};

    auto tptr = ptr.ptr();

    ctx.push_recursion_check([tptr]() {
      auto tref = std::get<type_s::type_ref_t>(*tptr);
      symbols::type_ref_recursion_check(tref, tptr);
    });
  }

  return ptr.ptr();
}

} // namespace resolve_callaback_s

auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t {
  return resolve_callback_t{
      resolve_callback_t::unresolved_call_t<unresolved_t, type_t>{
          ptr, [&ctx, locale](ssptr<unresolved_t, type_t> ptr) -> void {
            resolve_callaback_s::type_ptr_asignment(ctx, locale, ptr);
          }}};
}

// TODO:
void decl_visitor(context_t &ctx, sptr<locale_t> locale, sptr<decl_t> ptr,
                  sptr<type_t> type_ptr, opt<median_t> &val_med) {

  auto default_visit = [&](auto &) {
    auto body_ptr = sptr<expr_t>{};
    if (val_med)
      body_ptr =
          symbols::expr_fn(ctx, locale, val_med.value().fchild().as_median());
    *ptr = decl_s::var_decl_t{true, type_ptr, body_ptr};
    return;
  };

  auto fntemplate_visitor = [&](type_s::fntemplate_t &val) {
    if (!val_med)
      throw std::runtime_error("Functions need to have a body");
    auto fn = symbols::fntemplate2fndecl(ctx, val, val_med.value());
    *ptr = fn;
  };

  return ovisit(
      *type_ptr,
      [&](type_s::type_ref_t &val) {
        auto fin = symbols::typeref_get_final(
            std::get<type_s::type_ref_t>(type_ptr.get_val()));
        return ovisit(*fin, fntemplate_visitor,
                      [&](auto &) { default_visit(type_ptr); });
      },
      default_visit);
}

auto decl_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<decl_s::unresolved_t, decl_t> ptr) -> resolve_callback_t {
  return resolve_callback_t{
      resolve_callback_t::unresolved_call_t<decl_s::unresolved_t, decl_t>{
          ptr, [&ctx, locale](ssptr<decl_s::unresolved_t, decl_t> ptr) -> void {
            auto &[type_med, val_med] = ptr.get();
            auto type_ptr = ctx.make_sptr<type_t>(unresolved_t{type_med});
            resolve_callaback_s::type_ptr_asignment(ctx, locale, type_ptr);
            decl_visitor(ctx, locale, ptr.ptr(), type_ptr, val_med);
          }}};
}


namespace chain_state {
auto invalid(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
             cursor_t &cursor) -> void {
  throw std::runtime_error("Invalid state");
}

auto resolve_symbol(context_t &ctx, sptr<locale_t> locale, final_t final)
    -> sptr<decl_t> {
  const auto name = ctx.toks.str(final);
  auto lookup = locale_t::ancestor_lookup(locale, name);
  if (!lookup)
    throw std::runtime_error(("Unable to find symbol: ") + std::string(name));

  return lookup.symbol;
}

void member(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
            cursor_t &cursor, expr_s::chain_t &chain,
            ssptr<decl_s::field_t, decl_t> val);

void var(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
         cursor_t &cursor, expr_s::chain_t &chain,
         ssptr<decl_s::var_decl_t, decl_t> val);

void scope(context_t &ctx, const span_t &ch, cursor_t &cursor,
           expr_s::chain_t &chain, ssptr<decl_s::scope_decl_t, decl_t> val);

void fncall(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
            cursor_t &cursor, expr_s::chain_t &chain, median_t med);

void index(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
           cursor_t &cursor, expr_s::chain_t &chain, median_t &med,
           ssptr<type_s::indirection_t, type_t> ptr);

void deref(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
           cursor_t &cursor, expr_s::chain_t &chain,
           ssptr<type_s::indirection_t, type_t> ptr);

void handle_after_var_and_member(context_t &ctx, sptr<locale_t> locale,
                                 const span_t &ch, cursor_t &cursor,
                                 expr_s::chain_t &chain, sptr<type_t> type);

void deref(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
           cursor_t &cursor, expr_s::chain_t &chain, ssptr<type_s::indirection_t, type_t> ptr) {
  auto [depth, type] = symbols::indirection_final(ptr.ptr(), 0, 1);
  chain.chain.emplace_back(expr_s::post::deref_t{type});

  cursor.advance();
  if (!ch.contains(cursor))
    return;

  handle_after_var_and_member(ctx, locale, ch, cursor, chain, type);
}

void index(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
           cursor_t &cursor, expr_s::chain_t &chain, median_t &med,
           ssptr<type_s::indirection_t, type_t> ptr) {
  auto exprs = list<sptr<expr_t>>{};
  auto med_cursor = cursor_helper_t{med.children()};
  while (med_cursor.within()) {
    auto med = med_cursor.extract<medianc::EXPR>();
    auto ptr = semantics::symbols::expr_fn(ctx, locale, med.value());
    exprs.push_back(ptr);
  }
  // check if you can dereference with that depth
  auto [depth, type] = symbols::indirection_final(ptr.ptr(), 0, exprs.size());

  if (depth < exprs.size())
    throw std::runtime_error("Can't dereference that deep");

  for (auto &expr : exprs)
    chain.chain.push_back(expr_s::post::index_access_t{expr});

  cursor.advance();
  if (!ch.contains(cursor))
    return;

  handle_after_var_and_member(ctx, locale, ch, cursor, chain, type);
}
void handle_after_var_and_member(context_t &ctx, sptr<locale_t> locale,
                                 const span_t &ch, cursor_t &cursor,
                                 expr_s::chain_t &chain, sptr<type_t> type) {
  if (cursor->is_final()) {
    auto fin = cursor->as_final();
    if (rholds<type_s::aggregate_t, type_s::rec_t>(*type)) {
      auto locale = std::get<type_s::aggregate_t>(*type).get_locale();
      auto symbol = resolve_symbol(ctx, locale, fin);
      return ovisit(
          *symbol,
          [&](decl_s::field_t &) {
            member(ctx, locale, ch, cursor, chain, symbol);
          },
          [&](decl_s::scope_decl_t &) {
            scope(ctx, ch, cursor, chain, symbol);
          },
          [&](auto &val) { invalid(ctx, locale, ch, cursor); });
    } else if (fin->isa(tokc::PERISPOMENI) &&
               rholds<type_s::indirection_t>(*type)) {
      return deref(ctx, locale, ch, cursor, chain, type);
    } else {
      ovisit(*type, [](auto &val) {
        std::println(std::cerr, "type:{}",
                     boost::core::demangled_name(typeid(val)));
      });
      throw std::runtime_error("This variable does not have members");
    }
  } else if (cursor->is_median()) {
    auto med = cursor->as_median();
    // TODO: add support for arrays
    if (med.type() == medianc::FUNCTION_CALL &&
        rholds<type_s::fntype_t>(*type)) {
      throw std::runtime_error(
          "Found function call but it is not supported yet");
      return fncall(ctx, locale, ch, cursor, chain, med);
    } else if (med.type() == medianc::ARRAY_ACCESS &&
               rholds<type_s::indirection_t>(*type)) {
      return index(ctx, locale, ch, cursor, chain, med, type);
    } else {
      throw std::runtime_error("Variable does not support this feature");
    }
  }
}
void var(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
         cursor_t &cursor, expr_s::chain_t &chain,
         ssptr<decl_s::var_decl_t, decl_t> val) {
  chain.chain.emplace_back(expr_s::post::var_access_t{val});
  cursor.advance();
  if (!ch.contains(cursor))
    return;
  handle_after_var_and_member(ctx, locale, ch, cursor, chain, val.get().type);
}

void member(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
            cursor_t &cursor, expr_s::chain_t &chain,
            ssptr<decl_s::field_t, decl_t> val) {
  chain.chain.emplace_back(expr_s::post::field_access_t{val});
  cursor.advance();
  if (!ch.contains(cursor))
    return;
  handle_after_var_and_member(ctx, locale, ch, cursor, chain,
                              val.get().decl.get().type);
}

void fncall(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
            cursor_t &cursor, expr_s::chain_t &chain, median_t med) {}

void function(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
              cursor_t &cursor, expr_s::chain_t &chain,
              ssptr<decl_s::fn_decl_t, decl_t> val) {
  chain.chain.emplace_back(expr_s::post::fn_access_t{val});
  if (!ch.contains(cursor))
    return;
  cursor.advance();

  if (!cursor->is_median())
    throw std::runtime_error(
        "Functions can be used as function pointers or with calls");

  auto med = cursor->as_median();
  if (med.type() != medianc::FUNCTION_CALL)
    throw std::runtime_error(
        "Functions can be used as function pointers or with calls");

  return fncall(ctx, locale, ch, cursor, chain, med);
}

void scope(context_t &ctx, const span_t &ch, cursor_t &cursor,
           expr_s::chain_t &chain, ssptr<decl_s::scope_decl_t, decl_t> val) {
  if (!ch.contains(cursor))
    throw std::runtime_error("Scopes can't be used in expresions as values");
  cursor.advance();

  if (!cursor->is_final())
    throw std::runtime_error("Scope doesn't support median child tokens");

  auto locale = val.get().get_locale();
  auto symbol = resolve_symbol(ctx, locale, cursor->as_final());
  ovisit(
      *symbol,
      [&](decl_s::scope_decl_t &) { scope(ctx, ch, cursor, chain, symbol); },
      [&](decl_s::var_decl_t &) {
        var(ctx, locale, ch, cursor, chain, symbol);
      },
      [&](decl_s::fn_decl_t &) {
        function(ctx, locale, ch, cursor, chain, symbol);
      },
      [&](auto &val) { invalid(ctx, locale, ch, cursor); });
}

void entry(context_t &ctx, sptr<locale_t> locale, const span_t &ch,
           cursor_t &cursor, expr_s::chain_t &chain) {
  auto symbol = resolve_symbol(ctx, locale, cursor->as_final());
  ovisit(
      *symbol,
      [&](decl_s::scope_decl_t &) {
        chain_state::scope(ctx, ch, cursor, chain, symbol);
      },
      [&](decl_s::var_decl_t &) {
        chain_state::var(ctx, locale, ch, cursor, chain, symbol);
      },
      [&](auto &val) { invalid(ctx, locale, ch, cursor); });
}
}; // namespace chain_state

auto resolve_chain_fn(context_t &ctx, sptr<locale_t> locale,
                      const median_t &med) -> expr_elm_t {
  auto ch = med.children();
  auto cursor = ch.begin();
  auto chain = expr_s::chain_t{};
  chain_state::entry(ctx, locale, ch, cursor, chain);
  return chain;
}
auto expr_elm_chain_fn(context_t &ctx, sptr<locale_t> locale,
                 ssptr<unresolved_t, expr_elm_t> ptr) -> resolve_callback_t {
  static constexpr auto pfun = __PRETTY_FUNCTION__;
  return resolve_callback_t{
      resolve_callback_t::unresolved_call_t<unresolved_t, expr_elm_t>{
          ptr,
          [&ctx,locale](ssptr<semantics::unresolved_t, expr_elm_t> ptr) -> void {
            if (!rholds<semantics::unresolved_t>(*ptr.ptr()))
              return;
            auto valptr = ptr.ptr();
            auto t = ptr.val->type;
            auto med = ptr.get().med;
            *valptr = resolve_chain_fn(ctx, locale, med);
            valptr->type = t;
          }}};
}

namespace expresion_type_resolution_pass {
void  expresion_elm_fn(context_t &ctx, sptr<locale_t> locale,
                      sptr<expr_elm_t> ptr) ;

auto expresion_elm_operator_fn(context_t &ctx, sptr<locale_t> locale,
                               expr_s::operator_t &val) -> sptr<type_t> {
  const auto& meta = val.meta();
  auto& payload = val.payload;
  return ovisit(
      val,
      [&](expr_s::bop_t &val) -> sptr<type_t> {
        expresion_elm_fn(ctx, locale, val.lhs);
        auto lhs_type = val.lhs->type;

        expresion_elm_fn(ctx, locale, val.rhs);
        auto rhs_type = val.rhs->type;

        //for now it inherits the type of the of rhs
        return rhs_type;
      },
      [&](expr_s::uop_t &val) -> sptr<type_t> {
        expresion_elm_fn(ctx, locale, val.operand);
        auto operand_type = val.operand->type;
        if(meta.op == expr_s::op_operation_e::ADDRESS){
          return ctx.make_sptr<type_t>(type_s::ptr_t{operand_type});
        }
        else if(meta.op == expr_s::op_operation_e::AS){
          //maybe add a check here?
          return payload.as.type;
        }else{
          return operand_type;
        }
      },
      [](auto &val) -> sptr<type_t> { std::unreachable(); });
}

auto expresion_elm_operand_fn(context_t &ctx, sptr<locale_t> locale,
                              expr_s::operand_t &val) -> sptr<type_t> {
  return ovisit(val, [&](auto &val) -> sptr<type_t> {
    return ctx.make_sptr<type_t>(type_s::uint_t{69420});
  });
}

void expresion_elm_fn(context_t &ctx, sptr<locale_t> locale,
                      sptr<expr_elm_t> ptr) {
  if (!rholds<empty_t>(*ptr->type))
    return;
  auto &type_ptr = ptr->type;
  auto val = ovisit(
      *ptr,
      [&](expr_s::operand_t &val) -> sptr<type_t> {
        return expresion_elm_operand_fn(ctx, locale, val);
      },
      [&](expr_s::operator_t &val) -> sptr<type_t> {
        return expresion_elm_operator_fn(ctx, locale, val);
      },
      [](auto &) -> sptr<type_t> { std::unreachable(); });
  *type_ptr = type_s::type_ref_t{val};
}

void expresion_type_fn(context_t &ctx, sptr<locale_t> locale,
                       sptr<expr_t> ptr) {
 if(!rholds<empty_t>(*ptr->type))
   return;

 expresion_elm_fn(ctx, locale, ptr->val);
 *ptr->type = type_s::type_ref_t{ptr->val->type};
}

} // namespace expresion_type_resolution_pass

} // namespace resolve
} // namespace semantics
