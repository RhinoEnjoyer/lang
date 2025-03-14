#include "semantics_header.hpp"
#include <stdexcept>
#include <utility>
#include <variant>

namespace semantics {

namespace resolve {
auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t;
}

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
                          const median_t med) -> list<sptr<decl_t>> {
  list<sptr<decl_t>> tlist;
  for (auto &elm : med.children()) {
    auto decl_med = elm.as_median().fchild().as_median();
    switch (decl_med.type()) {
    case medianc::DECL:
      tlist.push_back(decl_spec_fn<var_decl_fn>(ctx, loc, decl_med));
      break;
    case medianc::TYPE_DECL:
      tlist.push_back(decl_spec_fn<type_decl_fn>(ctx, loc, decl_med));
      break;
    default:
      std::unreachable();
    }
  }
  return tlist;
}

auto rec_fn(context_t &ctx, sptr<locale_t> loc, const median_t rec_med)
    -> type_s::rec_t {
  auto cursor = cursor_helper_t{rec_med.children()};

  auto [templates_med, body_med] =
      cursor.tuple_extract<medianc::TEMPLATE_ARGUMENT_LIST, medianc::BODY>();

  auto locale = locale_t::make_child(ctx, loc);
  auto targs = list<sptr<decl_t>>{};

  if (templates_med) [[unlikely]]
    targs = template_arg_list_fn(ctx, locale, templates_med.value());
  // this pattern is on other places as well
  //  make a function to handle it
  auto stmt_list = ctx.make_sptr(stmts_t{});
  auto ch = body_med->children();
  consume_stmts_fn(ctx, locale, stmt_list, ch);

  return type_s::rec_t{locale, std::move(targs), stmt_list};
}

template <typename t>
auto indirection_fn(context_t &ctx, sptr<locale_t> loc, const median_t in_med)
    -> type_t {

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
  auto cursor = cursor_helper_t{med.children()};
  auto expr_med = cursor.extract<medianc::EXPR>();
  return empty_t{};
}

auto callable_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> type_t {
  auto cursor = cursor_helper_t{med.children()};

  const auto [chain_med, state_med, template_med, args_med, ret_med] =
      cursor.tuple_extract<medianc::DBRACES, medianc::FN_STATE_LIST,
                           medianc::TEMPLATE_ARGUMENT_LIST, medianc::FN_ARGS,
                           medianc::FN_RET>();

  // if (chain_med) [[unlikely]] {
  //   return type_s::inherit_decl_t{chain_med->fchild().as_median()};
  // }

  bool is_closure = false;
  if (state_med) [[unlikely]] {
    if (state_med->len() == 0) {
      is_closure = false;
    } else {
      is_closure = true;
      // do closure stuff
    }
  }

  auto locale = locale_t::make_child(ctx, loc);
  auto targs_list = list<sptr<decl_t>>{};

  if (template_med) [[unlikely]]
    targs_list = template_arg_list_fn(ctx, locale, template_med.value());

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

  auto sig = ctx.make_sptr(type_s::fnsig_t{
      locale, {std::move(targs_list)}, std::move(args), ret_type});

  if (is_closure)
    return type_s::fn_t{sig};
  else
    return type_s::closure_t{sig};
}

auto type_fn(context_t &ctx, sptr<locale_t> loc, const median_t type_med)
    -> sptr<type_t> {
  const auto node = type_med.children().begin()->node();
  auto ptr = ctx.make_sptr(type_t{});
  auto val = (ovisit(
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
          ctx.callbacks.push_back(resolve::type_fn(ctx, loc, ssptr<unresolved_t, type_t>(ptr)));
          return unresolved_t{val};
        }
        case medianc::FN_SIG:
          return callable_fn(ctx, loc, val);
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
      [](const auto &val) -> type_t { std::unreachable(); }));
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

  const auto mut_val = [&mut_fin] -> opt<bool> {
    if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_IMMUTABLE))
      return false;
    else if (mut_fin && mut_fin->base()->isa(tokc::BUILTIN_MUTABLE))
      return true;
    else
      return std::nullopt;
  }();

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

auto scope_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                   cursor_helper_t &cursor) -> void {
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
  auto [sig_med, body_med] =
      cursor.tuple_extract<medianc::FN_SIG, medianc::BODY>();

  auto sig = ctx.make_sptr<type_t>(callable_fn(ctx, loc, sig_med.value()));
  auto body =
      expr_fn(ctx,
              ovisit(
                  *sig,
                  [](type_s::callable_t &val) -> sptr<locale_t> {
                    return ovisit(val, [](auto &val) -> sptr<locale_t> {
                      return val.sig->locale;
                    });
                  },
                  [](auto &val) -> sptr<locale_t> { std::unreachable(); }),
              body_med.value().fchild().as_median());
  *ptr = decl_s::fn_decl_t{sig, body};
}

template <bool allow_init_val>
auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void {

  auto type_med = cursor.extract<medianc::TYPE>();
  auto type = sptr<type_t>{nullptr};
  if (type_med) [[likely]] {
    if constexpr (!allow_init_val) {
      throw std::runtime_error("Init values on variable declaration are "
                               "conditionally not allowed");
    } else {
      type = type_fn(ctx, loc, type_med.value());
    }
  }
  *ptr = decl_s::type_decl_t{type};
}

template <void (*fn)(sptr<decl_t>, context_t &, sptr<locale_t>,
                     cursor_helper_t &)>
auto decl_spec_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t> {
  constexpr auto localy_indistinct = (fn != scope_decl_fn);
  auto [ptr, cursor] = get_decl_fn(localy_indistinct, ctx, loc, decl_med);
  // std::println("{}", ptr->name);

  fn(ptr, ctx, loc, cursor);
  return ptr;
}

auto unwrap_decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<stmt_t> {
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
  auto cursor = cursor_helper_t{import_med.children()};
  auto strlit_node = cursor.extract<tokc::STRLIT>();

  return ctx.make_sptr<stmt_t>(
      stmt_s::import_t{ctx.toks.str(strlit_node.value())});
}

auto forloop_fn(context_t &ctx, sptr<locale_t> ploc, const median_t for_med)
    -> sptr<stmt_t> {
  auto cursor = cursor_helper_t{for_med.children()};
  auto [ctrl_expr_med, body_med] =
      cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

  auto locale = locale_t::make_child(ctx, ploc);
  // ctrl_expr
  auto ctrl_expr_stmts = [&] -> sptr<stmts_t> {
    auto stmts = ctx.make_sptr(stmts_t{});
    auto ch = ctrl_expr_med->children();
    // if (ch.size2() != 3)
    //   throw std::runtime_error("for loop ctrl expresion under filled");

    const auto decl_med = ch[0].as_median();
    const auto cmp_expr_med = ch[1].as_median();
    const auto it_expr_med = ch[2].as_median();

    auto decl_ptr = decl_spec_fn<var_decl_fn>(ctx, locale, decl_med);
    auto cmp_expr_ptr = expr_fn(ctx, locale, cmp_expr_med);
    auto it_expr_ptr = expr_fn(ctx, locale, it_expr_med);

    stmts->stmts.push_back(ctx.make_sptr<stmt_t>(decl_ptr));
    stmts->stmts.push_back(ctx.make_sptr<stmt_t>(cmp_expr_ptr));
    stmts->stmts.push_back(ctx.make_sptr<stmt_t>(it_expr_ptr));
    return stmts;
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
  auto type_med = med.fchild().as_median();
  return expr_s::as_t{{}, type_fn(ctx, loc, type_med)};
}

auto expr_operator_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
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
  return empty_t{};
}
auto chain_intro_fn(context_t &ctx, sptr<locale_t> loc, cursor_helper_t &cursor)
    -> expr_elm_t {
  return empty_t{};
}

auto chain_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  return empty_t{};
}

// TODO add the chain
auto self_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  return empty_t{};
}
// TODO add the chain
auto result_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::result_t {
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

  /* auto ch = med.children();
  auto cursor = ch.begin();
  while (ch.contain(cursor)) {
    cursor.advance();
  } */
  return empty_t{};
}

auto sizeof_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
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
  auto cursor = cursor_helper_t{med.children()};
  auto [ctype_med, cinit_med] =
      cursor.tuple_extract<medianc::COMPOUND_LITERAL_TYPE,
                           medianc::COMPOUND_LITERAL_INIT>();

  auto type = type_fn(ctx, loc, ctype_med->fchild().as_median());

  // if (cinit_med->children().begin()->as_median().type() == medianc::STMT) {
  if (rholds<type_s::callable_t>(type->base())) {

    // throw std::runtime_error("\n"
    //                          "Callable compound literals are not yet
    //                          supported " "and the grammar around it might
    //                          change");

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
  auto expr_med = cursor_helper_t(med.children()).extract<medianc::EXPR>();
  return ctx.make_sptr<stmt_t>(
      stmt_s::ret_t{expr_fn(ctx, loc, expr_med.value())});
}

auto stmt_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
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
    -> auto {

  auto ctx = context_t{toks, 0, allocator};
  auto locale = ctx.make_sptr(locale_t{});
  auto stmts = ctx.make_sptr(stmts_t{});
  auto span = root_node.as_median().children();

  consume_stmts_fn(ctx, locale, stmts, span);

  for (auto &callback : ctx.callbacks) {
    callback();
  }

  return std::tuple{locale, stmts};
}

} // namespace symbols

namespace resolve {
namespace resolve_callaback_s {

auto type_impl_fn(context_t &ctx, sptr<locale_t> locale, span_t::iterator it,
               const span_t::iterator end) -> sptr<type_t> {
  auto val = it->node();
  return ovisit(
      val,
      [&](final_t &val) -> sptr<type_t> {
        if (!val->isa(tokc::ID))
          throw std::runtime_error(
              "Only IDs are supported in unresoved symbols");

        const auto str = ctx.toks.str(val);
        auto lookup = locale_t::ancestor_lookup(locale, str);
        if (!lookup.found)
          throw std::runtime_error("Failed to find symbol " + std::string(str));

        return ovisit(
            *lookup.symbol,
            [&](decl_s::scope_decl_t &val) -> sptr<type_t> {
              if (it.next() >= end)
                throw std::runtime_error("scopes can't be types");
              return type_impl_fn(ctx, val.get_locale(), it.next(), end);
            },
            [&](decl_s::type_decl_t &val) -> sptr<type_t> {
              if (it.next() >= end)
                return val.type;
              return ovisit(
                  *val.type,
                  [&](type_s::callable_t &v) -> sptr<type_t> {
                    if (it.next() >= end)
                      return val.type;
                    throw std::runtime_error("Callables can't be queried");
                  },
                  [&](type_s::primitive_t &v) -> sptr<type_t> {
                    if (it.next() >= end)
                      return val.type;
                    throw std::runtime_error("Primitives can't be queried");
                  },
                  [&](type_s::aggregate_t &v) -> sptr<type_t> {
                    if (it.next() >= end)
                      return val.type;
                    // throw std::runtime_error("Primitives can't be queried");
                    return ovisit(
                        v,
                        [&](type_s::rec_t &v1) -> sptr<type_t> {
                          return type_impl_fn(ctx, v1.loc, it.next(), end);
                        },
                        // [&](type_s::tup_t& v1) -> sptr<type_t> {
                        //   return type_impl(ctx, v1.loc, it.next(), end);
                        // },
                        [&](auto &) -> sptr<type_t> {
                          throw std::runtime_error("Not supported type");
                        });
                  },
                  [](auto &val) -> sptr<type_t> {
                    throw std::runtime_error("Type '" +
                                             std::string(typeid(val).name()) +
                                             "' is not supported.");
                  });
            },
            [&](auto &value) -> sptr<type_t> {
              throw std::runtime_error("Type '" +
                                       std::string(typeid(value).name()) +
                                       "' is not supported.");
            });
      },
      [&](median_t &val) -> sptr<type_t> {
        throw std::runtime_error(
            "For now type resolution of unresoved symbols is not supported "
            "for median values");
      },
      [&](auto &) -> sptr<type_t> { throw std::runtime_error("Unreachable"); });
};
} // namespace resolve_callaback_s
auto type_fn(context_t &ctx, sptr<locale_t> locale,
             ssptr<unresolved_t, type_t> ptr) -> resolve_callback_t {
  return [&ctx, locale, ptr] {
    auto span = ptr.get().med.children();
    *(ptr.ptr()) = type_s::type_ref_t{
        resolve_callaback_s::type_impl_fn(ctx, locale, span.begin(), span.end())};
  };
}
} // namespace resolve

} // namespace semantics
