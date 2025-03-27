#include "semantics.hpp"
#include <boost/core/typeinfo.hpp>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <utility>
#include <variant>

namespace semantics {
namespace expr_s {
auto token_to_operator(const tokc::e token) -> operator_t {
  switch (token) {
  case tokc::EQUALS:
    return eq_t{};
  case tokc::GEQUALS:
    return geq_t{};
  case tokc::LEQUALS:
    return leq_t{};
  case tokc::EMARKEQUALS:
    return neq_t{};
  case tokc::PLUSASIGN:
    return plusassign_t{};
  case tokc::MINUSASIGN:
    return minusassign_t{};
  case tokc::DIVASIGN:
    return divassign_t{};
  case tokc::MULASIGN:
    return multassign_t{};
  case tokc::ASIGN:
    return assign_t{};
  case tokc::PLUSPLUS:
    return plusplus_t{};
  case tokc::MINUSMINUS:
    return minusminus_t{};
  case tokc::GREATERGREATER:
    return sright_t{};
  case tokc::LESSLESS:
    return sleft_t{};
  case tokc::LESSGREATER:
    return neq_t{};
  case tokc::XOR:
    return xor_t{};
  case tokc::AND:
    return and_t{};
  case tokc::OR:
    return or_t{};
  case tokc::MODULO:
    return mod_t{};
  case tokc::PLUS:
    return plus_t{};
  case tokc::MINUS:
    return minus_t{};
  case tokc::DIV:
    return div_t{};
  case tokc::MUL:
    return mult_t{};
  case tokc::LESS:
    return less_t{};
  case tokc::GREATER:
    return greater_t{};
  case tokc::EMARK:
    return not_t{};
  case tokc::PERISPOMENI:
    return deref_t{};
  case tokc::AMPERSAND:
    return address_t{};
  case tokc::ANDASIGN:
    return assign_t{};
  case tokc::ORASIGN:
    return assign_t{};
  case tokc::MINUSGREATER:
    return pipe_t{};
  default:
    std::unreachable();
  }
}
} // namespace expr_s
namespace resolve {
auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t;
auto decl_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<decl_s::unresolved_t, decl_t> ptr) -> resolve_callback_t;

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
template <bool allow_init_val = true>
auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void;
template <bool allow_init_val = true>
auto var_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                 cursor_helper_t &cursor) -> void;
auto todo_stmts_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  throw std::runtime_error(std::string(__func__) + " todo function");
}

auto get_decl_fin_fn(const bool localy_indistinct, context_t &ctx,
                     sptr<locale_t> loc, const final_t id_fin) -> sptr<decl_t> {
  ctx.dbg_add_call();
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
  ctx.dbg_add_call();
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
  ctx.dbg_add_call();
  list<sptr<decl_t>> tlist;
  for (auto &elm : med.children()) {
    auto decl_med = elm.as_median().fchild().as_median();
    switch (decl_med.type()) {
    case medianc::DECL:
      tlist.push_back(decl_spec_fn<var_decl_fn>(ctx, loc, decl_med));
      break;
    case medianc::TYPE_DECL:
      // replace this with a proper type and not just a bastardised
      // type_declaration
      //  I do not remember why I didn't do it back when I made this
      tlist.push_back(decl_spec_fn<type_decl_fn>(ctx, loc, decl_med));
      break;
    default:
      std::unreachable();
    }
  }
  return {tlist};
}

auto rec_fn(context_t &ctx, sptr<locale_t> loc, const median_t rec_med)
    -> type_s::rec_t {
  ctx.dbg_add_call();
  auto cursor = cursor_helper_t{rec_med.children()};

  auto [templates_med, body_med] =
      cursor.tuple_extract<medianc::TEMPLATE_ARGUMENT_LIST, medianc::BODY>();

  auto locale = locale_t::make_child(ctx, loc);
  auto targs = ctx.make_sptr(type_s::template_args_t{});

  if (templates_med) [[unlikely]]
    *targs = template_arg_list_fn(ctx, locale, templates_med.value());
  // this pattern is on other places as well
  //  make a function to handle it
  auto stmt_list = ctx.make_sptr(stmts_t{});
  auto ch = body_med->children();
  consume_stmts_fn(ctx, locale, stmt_list, ch);

  return type_s::rec_t{locale, /* targs, */ stmt_list};
}

template <typename t>
auto indirection_fn(context_t &ctx, sptr<locale_t> loc, const median_t in_med)
    -> type_t {
  ctx.dbg_add_call();
  static_assert(std::is_same_v<t, type_s::ref_t> ||
                    std::is_same_v<t, type_s::ptr_t>,
                "indirection type must be a type_s::ref_t or a "
                "type_s::ptr_t");
  auto cursor = cursor_helper_t{in_med.children()};
  auto type_med = cursor.extract<medianc::TYPE>();
  return t{type_fn(ctx, loc, *type_med)};
}

auto tup_fn(context_t &ctx, sptr<locale_t> loc, const median_t tup_med)
    -> type_t {
  ctx.dbg_add_call();
  auto ch = tup_med.children();
  auto types = list<sptr<type_t>>{};
  // types.reserve(ch.size2());
  for (auto &elm : ch) {
    auto elm_med = elm.as_median();
    auto elm_ch = elm_med.children();
    auto med = elm_ch.begin()->as_median();
    types.push_back(type_fn(ctx, loc, med));
  }
  return type_s::tup_t{std::move(types)};
}

auto typeof_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  ctx.dbg_add_call();
  auto cursor = cursor_helper_t{med.children()};
  auto expr_med = cursor.extract<medianc::EXPR>();
  return empty_t{};
}

auto fntemplate_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  ctx.dbg_add_call();
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
      args.push_back(ptr);
    }
  }

  if (ret_med)
    ret_type = type_fn(ctx, loc, ret_med->fchild().as_median());

  return type_s::fntemplate_t{locale, args, ret_type};
}

auto fntype_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  ctx.dbg_add_call();
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
  ctx.dbg_add_call();
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

      args.push_back(decl_spec_fn<var_decl_fn>(ctx, locale,
                                               decl_med->fchild().as_median()));
    }
  }

  if (ret_med)
    ret_type = type_fn(ctx, loc, ret_med->fchild().as_median());

  auto sig = ctx.make_sptr(
      type_s::fnsig_t{locale, targs_list, std::move(args), ret_type});

  if (!is_closure)
    return type_t{type_s::callable_t{type_s::fn_t{sig}}};
  else
    return type_t{type_s::callable_t{type_s::closure_t{sig}}};
}

auto type_fn(context_t &ctx, sptr<locale_t> loc, const median_t type_med)
    -> sptr<type_t> {
  ctx.dbg_add_call();
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
        case medianc::TUPLE:
          return tup_fn(ctx, loc, val);
        case medianc::RECORD:
          return rec_fn(ctx, loc, val);
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
  ctx.dbg_add_call();
  const auto [mut_fin, type_med, val_med] =
      cursor.tuple_extract<tokc::any, medianc::TYPE, medianc::VALUE>();
  const auto mut_val = [&mut_fin] -> opt<bool> {
    if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_IMMUTABLE))
      return false;
    else if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_MUTABLE))
      return true;
    else
      return std::nullopt;
  }();

  if constexpr (true) {
    // make a callback to resolve later the declaration
    auto type_val = sptr<type_t>{nullptr};
    {
      // std::println(std::cerr,"{}", medianc::str(type_med->type()));
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

      auto init_val = [&val_med, &ctx, &loc] -> sptr<expr_t> {
        if (val_med) {
          if constexpr (!allow_init_val) {
            throw std::runtime_error("Init values on variable declaration are "
                                     "conditionally not allowed");
          } else {
            return expr_fn(ctx, loc, val_med->fchild().as_median());
          }
        }
        return nullptr;
      }();
      *ptr = decl_s::var_decl_t{mut_val, type_val, init_val};
      return;
    }
  } else {
    auto type_val = [&type_med, &ctx, &loc] -> sptr<type_t> {
      if (!type_med) {
        return ctx.make_sptr<type_t>(type_s::infered_t{}); // no type
      }
      return type_fn(ctx, loc,
                     type_med.value()); // resolve the literal type
    }();

    auto init_val = [&val_med, &ctx, &loc] -> sptr<expr_t> {
      if (val_med) {
        if constexpr (!allow_init_val) {
          throw std::runtime_error("Init values on variable declaration are "
                                   "conditionally not allowed");
        } else {
          return expr_fn(ctx, loc, val_med->fchild().as_median());
        }
      }
      return nullptr;
    }();

    *ptr = decl_s::var_decl_t{mut_val, type_val, init_val};
  }
}

auto scope_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                   cursor_helper_t &cursor) -> void {
  ctx.dbg_add_call();
  sptr<locale_t> locale;
  sptr<stmts_t> stmts;
  if (std::holds_alternative<decl_s::scope_decl_t>(*ptr)) {
    auto &scope = std::get<decl_s::scope_decl_t>(*ptr);
    locale = scope.loc;
    stmts = scope.stmts;
  } else {
    locale = locale_t::make_child(ctx, loc);
    stmts = ctx.make_sptr(stmts_t{});
    *ptr = decl_s::scope_decl_t{locale, stmts};
  }
  const auto body = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body->children());
  // return decl_s::scope_decl_t{locale, stmts};
}

auto fn_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                cursor_helper_t &cursor) -> void {
  ctx.dbg_add_call();
  auto [sig_med, body_med] =
      cursor.tuple_extract<medianc::FN_SIG, medianc::BODY>();
  auto callable_type = callable_fn(ctx, loc, sig_med.value());
  auto callable_type_ptr = ctx.make_sptr<type_t>(callable_type);
  auto sig = ssptr<type_s::callable_t, type_t>{callable_type_ptr};

  auto body = expr_fn(
      ctx,
      ovisit(sig.get(),
             [](auto &val) -> sptr<locale_t> { return val.get_locale(); }),
      body_med.value().fchild().as_median());

  *ptr = decl_s::fn_decl_t{sig.ptr(), body};
}

template <bool allow_init_val>
auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void {
  ctx.dbg_add_call();
  auto [template_args_med, type_med] =
      cursor.tuple_extract<medianc::TEMPLATE_ARGUMENT_LIST, medianc::TYPE>();
  auto type = sptr<type_t>{nullptr};

  auto locale = loc;
  auto template_mod = [&] -> opt<decl_s::type_decl_t::template_module_t> {
    if constexpr (allow_init_val) {
      if (template_args_med) {
        auto template_list = sptr<type_s::template_args_t>{nullptr};
        auto new_locale = locale_t::make_child(ctx, loc);
        template_list = ctx.make_sptr(
            template_arg_list_fn(ctx, new_locale, template_args_med.value()));
        return decl_s::type_decl_t::template_module_t{new_locale,
                                                      template_list};
      } else {
        if(template_args_med)
          throw std::runtime_error("When we do not allow init values we do not "
                                   "allow templates either");
      }
    }
    return std::nullopt;
  }();

  if constexpr (allow_init_val) {
    if (template_mod)
      locale = template_mod->locale;
  }

  if (type_med) [[likely]] {
    if constexpr (!allow_init_val) {
      throw std::runtime_error("Init values on variable declaration are "
                               "conditionally not allowed");
    } else {
      type = type_fn(ctx, locale, type_med.value());
    }
  }
  if constexpr (allow_init_val)
    *ptr = decl_s::type_decl_t{template_mod, type};
  else
    *ptr = decl_s::type_decl_t{std::nullopt, nullptr};
}

template <void (*fn)(sptr<decl_t>, context_t &, sptr<locale_t>,
                     cursor_helper_t &)>
auto decl_spec_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t> {
  ctx.dbg_add_call();
  constexpr auto localy_indistinct = (fn != scope_decl_fn);
  auto [ptr, cursor] = get_decl_fn(localy_indistinct, ctx, loc, decl_med);
  // std::println(std::cerr,"{}", ptr->name);

  fn(ptr, ctx, loc, cursor);
  return ptr;
}

auto unwrap_decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<stmt_t> {
  ctx.dbg_add_call();
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
  ctx.dbg_add_call();
  auto cursor = cursor_helper_t{import_med.children()};
  auto strlit_node = cursor.extract<tokc::STRLIT>();

  return ctx.make_sptr<stmt_t>(
      stmt_s::import_t{ctx.toks.str(strlit_node.value())});
}

auto forloop_fn(context_t &ctx, sptr<locale_t> ploc, const median_t for_med)
    -> sptr<stmt_t> {
  ctx.dbg_add_call();
  auto cursor = cursor_helper_t{for_med.children()};
  auto [ctrl_expr_med, body_med] =
      cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

  auto locale = locale_t::make_child(ctx, ploc);
  // ctrl_expr
  auto ctrl_expr_stmts = [&] -> sptr<stmts_t> {
    auto stmt_list = ctx.make_sptr(stmts_t{});
    auto ch = ctrl_expr_med->children();
    // if (ch.size2() != 3)
    //   throw std::runtime_error("for loop ctrl expresion under filled");
    auto cursor = cursor_helper_t{ctrl_expr_med->children()};

    auto [decl_med, cmp_expr_med, it_expr_med] =
        cursor.tuple_extract<medianc::DECL, medianc::EXPR, medianc::EXPR>();

    {
      if (!decl_med) [[unlikely]]
        throw std::runtime_error(
            "decl med of the control expresion doesn't exits");
      if (!cmp_expr_med) [[unlikely]]
        throw std::runtime_error(
            "cmp med of the control expresion doesn't exits");
      if (!it_expr_med) [[unlikely]]
        throw std::runtime_error(
            "it med of the control expresion doesn't exits");
    }

    auto decl_ptr = decl_spec_fn<var_decl_fn>(ctx, locale, decl_med.value());
    auto cmp_expr_ptr = expr_fn(ctx, locale, cmp_expr_med.value());
    auto it_expr_ptr = expr_fn(ctx, locale, it_expr_med.value());

    stmt_list->stmts.push_back(ctx.make_sptr<stmt_t>(decl_ptr));
    stmt_list->stmts.push_back(ctx.make_sptr<stmt_t>(cmp_expr_ptr));
    stmt_list->stmts.push_back(ctx.make_sptr<stmt_t>(it_expr_ptr));
    return stmt_list;
  }();
  // body
  auto body_stmts = [&] -> sptr<stmts_t> {
    auto stmts = ctx.make_sptr(stmts_t{});
    auto span = body_med->children();
    consume_stmts_fn(ctx, locale, stmts, span);
    return stmts;
  }();
  return ctx.make_sptr<stmt_t>(
      stmt_s::forloop_t{locale, ctrl_expr_stmts, body_stmts});
}

auto as_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();

  auto type_med = med.fchild().as_median();
  return expr_s::as_t{{}, type_fn(ctx, loc, type_med)};
}

auto expr_operator_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
  ctx.dbg_add_call();

  auto node = med.fchild().node();
  return ctx.make_sptr<expr_elm_t>(ovisit(
      node,
      [](const final_t &val) -> expr_elm_t {
        return expr_s::token_to_operator(val->type());
      },
      [&ctx, &loc](const median_t &val) -> expr_elm_t {
        switch (val.type()) {
        case medianc::AS:
          return as_fn(ctx, loc, val);
        default:
          std::unreachable();
        }
      },
      [](const auto &) -> expr_elm_t { std::unreachable(); }));
}

// part that will be reused on a chain
auto chain_auxilary_fn(context_t &ctx, sptr<locale_t> loc,
                       cursor_helper_t &cursor) -> expr_elm_t {
  ctx.dbg_add_call();

  return empty_t{};
}
auto chain_intro_fn(context_t &ctx, sptr<locale_t> loc, cursor_helper_t &cursor)
    -> expr_elm_t {
  ctx.dbg_add_call();

  return empty_t{};
}

auto chain_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();

  return empty_t{};
}

// TODO add the chain
auto self_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();

  return empty_t{};
}
// TODO add the chain
auto result_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::result_t {
  ctx.dbg_add_call();

  auto locale = locale_t::make_child(ctx, loc);
  auto stmts = ctx.make_sptr(stmts_t{});
  auto cursor = cursor_helper_t{med.children()};

  auto body_med = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body_med->children());

  // chain part
  // chain_loop_fn(ctx, loc, cursor);

  return {locale, stmts};
}

auto pipe_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();

  /* auto ch = med.children();
  auto cursor = ch.begin();
  while (ch.contain(cursor)) {
    cursor.advance();
  } */
  return empty_t{};
}

auto sizeof_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();

  auto decl_or_type = med.fchild().as_median();
  switch (decl_or_type.type()) {
  case medianc::TYPE:
    return expr_s::sizeof_t{type_fn(ctx, loc, decl_or_type)};
  case medianc::EXPR:
    return expr_s::sizeof_t{expr_fn(ctx, loc, decl_or_type)};
  default:
    std::unreachable();
  }
  std::unreachable();
}

auto complit_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  ctx.dbg_add_call();
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
          return ovisit(
              val,
              [](type_s::fn_t &val) -> sptr<locale_t> {
                return val.sig->locale;
              },
              [](type_s::closure_t &val) -> sptr<locale_t> {
                return val.sig->locale;
              },
              [](auto &val) -> sptr<locale_t> { std::unreachable(); });
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
    -> expr_elm_t {
  ctx.dbg_add_call();
  auto cursor = cursor_helper_t{med.children()};
  auto val =
      expr_s::if_t{{nullptr, ctx.make_sptr(stmts_t{})}, {}, std::nullopt};

  {
    auto if_med = cursor.extract<medianc::IF>();
    auto [ctrl_expr_med, body_med] =
        cursor_helper_t{if_med->children()}
            .tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

    val.intro.ctrl_expr = expr_fn(
        ctx, loc, ctrl_expr_med.value().children().begin()->as_median());
    consume_stmts_fn(ctx, loc, val.intro.stmts, body_med->children());
  }

  //
  {
    while (cursor.within()) {
      auto elif_med = cursor.extract<medianc::ELIF>();
      if (!elif_med) {
        auto el_med = cursor.extract<medianc::ELSE>();
        if (!el_med)
          throw std::runtime_error("This shouldn't happen");
        {
          auto body_med =
              cursor_helper_t{el_med->children()}.extract<medianc::BODY>();
          auto el = expr_s::if_t::else_t{ctx.make_sptr(stmts_t{})};
          consume_stmts_fn(ctx, loc, el.stmts, body_med->children());
          val.el = std::move(el);
        }
        break;
      }
      auto cursor = cursor_helper_t{elif_med->children()};
      auto [ctrl_expr_med, body_med] =
          cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

      auto elif = expr_s::if_t::elif_t{nullptr, ctx.make_sptr(stmts_t{})};
      elif.ctrl_expr = expr_fn(
          ctx, loc, ctrl_expr_med.value().children().begin()->as_median());
      consume_stmts_fn(ctx, loc, elif.stmts, body_med->children());
      val.elifs.emplace_back(std::move(elif));
    }
  }
  return val;
}
auto expr_operand_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
  ctx.dbg_add_call();
  auto node = med.children().begin()->node();
  return ctx.make_sptr(ovisit(
      node,
      [&ctx](const final_t &val) -> expr_elm_t {
        switch (val->type()) {
        // this is kind of retarded but I am not sure
        // how to represent it
        case tokc::INT:
        case tokc::FLOAT:
          return expr_s::number_t{ctx.toks.str(val)};
        default:
          std::unreachable();
        }
      },
      [&ctx, &loc](const median_t &val) -> expr_elm_t {
        switch (val.type()) {
        case medianc::CHAIN:
          return empty_t{};
        case medianc::RESULT:
          return result_fn(ctx, loc, val);
        case medianc::SIZEOF:
          return sizeof_fn(ctx, loc, val);
        case medianc::COMPOUND_LITERAL:
          return complit_fn(ctx, loc, val);
        case medianc::IF_EXPR:
          return if_fn(ctx, loc, val);
        case medianc::PIPE:
          return pipe_fn(ctx, loc, val);
        case medianc::SELF:
          return self_fn(ctx, loc, val);
        default:
          std::unreachable();
        }
      },
      [](const auto &val) -> expr_elm_t { std::unreachable(); }));
}

auto expr_val_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_t {
  ctx.dbg_add_call();
  auto ch = med.children();
  auto cursor = ch.begin();

  auto expr = expr_t{};
  while (ch.contains(cursor)) {
    auto med = cursor->as_median();
    switch (med.type()) {
    case medianc::OPERAND:
      expr.exprs.emplace_back(expr_operand_fn(ctx, loc, med));
      break;
    case medianc::OPERATOR:
      expr.exprs.emplace_back(expr_operator_fn(ctx, loc, med));
      break;
    default:
      std::unreachable();
      break;
    }
    cursor.advance();
  }
  return expr;
}
auto expr_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_t> {
  return ctx.make_sptr<expr_t>(expr_val_fn(ctx, loc, med));
}

auto ret_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<stmt_t> {
  ctx.dbg_add_call();
  auto expr_med = cursor_helper_t(med.children()).extract<medianc::EXPR>();
  return ctx.make_sptr<stmt_t>(
      stmt_s::ret_t{expr_fn(ctx, loc, expr_med.value())});
}

auto stmt_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  ctx.dbg_add_call();
  const auto med = stmt_med.fchild().as_median();
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
  default:
    std::unreachable();
    break;
    // throw std::runtime_error(std::string(medianc::str(type)) + " " +
    // "Unknown stmt type");
  }
}

auto consume_stmts_fn(context_t &ctx, sptr<locale_t> loc, sptr<stmts_t> stmts,
                      span_t span) -> void {
  for (auto &elm : span)
    stmts->stmts.push_back(stmt_fn(ctx, loc, elm.as_median()));
}

__attribute__((visibility("default"))) auto entry(allocator_t &allocator,
                                                  const token_buffer_t &toks,
                                                  grammar::node_t &root_node)
    -> std::tuple<sptr<locale_t>, sptr<stmts_t>> {

  auto ctx = context_t{toks, 0, allocator};
  ctx.dbg_add_call();

  auto locale = ctx.make_sptr(locale_t{});
  auto stmts = ctx.make_sptr(stmts_t{});
  auto span = root_node.as_median().children();

  auto [res, time] = mesure([&] {
    consume_stmts_fn(ctx, locale, stmts, span);
    return 0;
  });

  auto [res2, time2] = mesure([&] {
    for (auto &callback : ctx.callback_map)
      callback.second();
    return 0;
  });

  for (const auto &call : ctx.dbg_callstack)
    std::println(std::cerr, "{}, {}", call.function, call.line);
  return std::tuple{locale, stmts};
}

} // namespace symbols

namespace resolve {
namespace resolve_callaback_s {
template <bool doancestor>
auto type_impl_fn(context_t &ctx, sptr<locale_t> locale, span_t::iterator it,
                  const span_t::iterator end) -> sptr<type_t> {
  ctx.dbg_add_call();
  auto val = it->node();
  // std::println(std::cerr,"{}, {}, {}", doancestor,
  // ctx.toks.str(it->as_final()),
  //              (uintptr_t)locale.get_ptr());
  // std::println("Table");
  // for (auto &elm : locale.get_val().internals.table)
  //   std::println("\t{}", elm.first);

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

        if (!lookup.found) [[unlikely]]
          throw std::runtime_error("Failed to find symbol " + std::string(str));

        return ovisit(
            *lookup.symbol,
            [&](decl_s::scope_decl_t &val) -> sptr<type_t> {
              if (it.next() >= end)
                throw std::runtime_error("scopes can't be types");
              return type_impl_fn<false>(ctx, val.get_locale(), it.next(), end);
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
                    // this is what saves us form infinite recursion
                    //  if we had a recursive call here we would be suscibtable
                    //  to unending recursion
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
                            return self(val.ref, self);
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
                    return callback.visit(
                        [&](resolve_callback_t::call_t<unresolved_t, type_t>
                                &val) -> ret {
                          auto ptr = val.ptr.ptr();
                          if (it >= end)
                            return ptr;
                          return type_lam(ptr, type_lam);
                        },
                        [&](resolve_callback_t::call_t<decl_s::unresolved_t,
                                                       decl_t> &val) -> ret {
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
                        });
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
    *(ptr.ptr()) = type_s::type_ref_t{resolve_callaback_s::type_impl_fn<true>(
        ctx, locale, span.begin(), span.end())};
  }
  return ptr.ptr();
}
} // namespace resolve_callaback_s

auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t {
  return resolve_callback_t{resolve_callback_t::call_t<unresolved_t, type_t>{
      ptr, [&ctx, locale](ssptr<unresolved_t, type_t> ptr) -> void {
        resolve_callaback_s::type_ptr_asignment(ctx, locale, ptr);
      }}};
}

auto decl_visitor(context_t &ctx, sptr<locale_t> locale, sptr<decl_t> ptr,
                  sptr<type_t> type_ptr, opt<median_t> &val_med) -> void {
  auto default_visit = [&](auto &) {
    auto body_ptr = sptr<expr_t>{};
    if (val_med)
      body_ptr =
          symbols::expr_fn(ctx, locale, val_med.value().fchild().as_median());
    *ptr = decl_s::var_decl_t{true, type_ptr, body_ptr};
    return;
  };
  return ovisit(
      *type_ptr,
      [&ptr, &locale, &ctx, &val_med, &default_visit](type_s::type_ref_t &val) {
        if (val.ref.is_null()) {
          // this means that it is proably a template
          // argument so this should change soon
          return default_visit(ptr);
        } else {
          return decl_visitor(ctx, locale, ptr, val.ref, val_med);
        }
      },
      [&ptr, &locale, &ctx, &val_med](type_s::fntemplate_t &val) {
        // std::println("Function template");

        auto new_locale = ctx.make_sptr(locale_t{*val.locale});
        new_locale->parent_ = locale;

        auto fnsig_ptr = ctx.make_sptr<type_s::fnsig_t>(
            type_s::fnsig_t{new_locale, nullptr, val.args, val.ret_type});
        auto fntype_ptr = ctx.make_sptr<type_t>(type_s::fn_t{fnsig_ptr});

        auto body_ptr = sptr<expr_t>{};
        if (val_med) {
          body_ptr = symbols::expr_fn(ctx, fnsig_ptr->get_locale(),
                                      val_med.value().fchild().as_median());
        }

        auto fnval = decl_s::fn_decl_t{fntype_ptr, body_ptr};
        *ptr = fnval;
        return;
      },
      [&](auto &val) {
        auto body_ptr = sptr<expr_t>{};
        if (val_med)
          body_ptr = symbols::expr_fn(ctx, locale,
                                      val_med.value().fchild().as_median());
        *ptr = decl_s::var_decl_t{true, type_ptr, body_ptr};
        return;
      });
}
auto decl_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<decl_s::unresolved_t, decl_t> ptr) -> resolve_callback_t {
  return resolve_callback_t{
      resolve_callback_t::call_t<decl_s::unresolved_t, decl_t>{
          ptr, [&ctx, locale](ssptr<decl_s::unresolved_t, decl_t> ptr) -> void {
            ctx.dbg_add_call();
            auto &[type_med, val_med] = ptr.get();
            auto type_ptr = ctx.make_sptr(type_t{unresolved_t{type_med}});
            resolve_callaback_s::type_ptr_asignment(ctx, locale, type_ptr);
            decl_visitor(ctx, locale, ptr.ptr(), type_ptr, val_med);
            // std::println(std::cerr, "Declaration resolution is not done
            // yet");
          }}};
}

} // namespace resolve

} // namespace semantics
