#include "semantics_header.hpp"
#include <stdexcept>
#include <utility>
#include <variant>

namespace semantics {
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

auto todo_stmts_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  throw std::runtime_error(std::string(__func__) + " todo function");
}

auto get_decl_fin_fn(const bool localy_indistinct, context_t &ctx,
                     sptr<locale_t> loc, const final_t id_fin) -> sptr<decl_t> {

  const auto name_fin = id_fin;
  const auto name_str = ctx.toks.str(name_fin);

  auto [inserted, name, entry] =
      loc->try_insert(name_str, make_sptr<decl_t>(name_str, ++ctx));
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
    throw std::runtime_error("declaration doesn't have a tokc::id for a name");

  auto ptr = get_decl_fin_fn(localy_indistinct, ctx, loc, name_fin.value());

  return {ptr, cursor};
}

auto rec_fn(context_t &ctx, sptr<locale_t> loc, const median_t rec_med)
    -> type_s::rec_t {
  auto cursor = cursor_helper_t{rec_med.children()};

  auto [templates_med, body_med] =
      cursor.tuple_extract<medianc::TEMPLATE_LIST, medianc::BODY>();

  if (templates_med) {
    // template code
  }

  // this pattern is on other places as well
  //  make a function to handle it
  auto locale = locale_t::make_child(loc);
  auto stmt_list = make_sptr(stmts_t{});

  consume_stmts_fn(ctx, locale, stmt_list, body_med->children());

  return type_s::rec_t{std::move(locale), std::move(stmt_list)};
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
  const auto [state_med, template_med, args_med, ret_med] =
      cursor.tuple_extract<medianc::FN_STATE_LIST, medianc::TEMPLATE_LIST,
                           medianc::FN_ARGS, medianc::FN_RET>();

  if (state_med) [[unlikely]] {
    // closure
    std::unreachable();
  }
  if (template_med) [[unlikely]] {
    // tempalte code
    std::unreachable();
  }

  auto locale = locale_t::make_child(loc);
  list<sptr<decl_t>> args;
  sptr<type_t> ret_type;

  if (args_med) {
    auto cursor = cursor_helper_t{args_med->children()};
    while (cursor.within()) {
      auto decl_med = cursor.extract<medianc::ARGUMENT>();
      if (!decl_med)
        throw std::runtime_error("Function arguments can only be declarations");
      args.push_back(
          decl_fn(ctx, locale, decl_med->fchild().as_median()));
    }
    if (ret_med)
      ret_type =
          type_fn(ctx, loc, ret_med->fchild().as_median());
  }

  return type_s::fn_t{make_sptr(type_s::fnsig_t{
      std::move(locale), std::move(args), std::move(ret_type)})};
}

auto type_fn(context_t &ctx, sptr<locale_t> loc, const median_t type_med)
    -> sptr<type_t> {
  const auto node = type_med.children().begin()->node();
  return make_sptr(ovisit(
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
          throw std::runtime_error(std::string(token_code_str(fin->type())) +
                                   " " + "unknown type (final_t)");
        }
      },
      [&ctx, &loc](const median_t &val) -> type_t {
        auto type = val.type();
        switch (type) {
        case medianc::CHAIN:
          return unresolved_t{val};
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
          throw std::runtime_error(std::string(medianc::str(type)) + " " +
                                   std::string("unknown type (median_t)"));
        }
      },
      [](const auto &val) -> type_t { std::unreachable(); }));
}


// this can be a function or a variable
//  this is because i want to be able to use type aliases to be able to declare
//  functions which kindof complicates things
auto var_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                 cursor_helper_t &cursor) -> void {
  const auto [mut_fin, type_med, val_med] =
      cursor
          .tuple_extract<tokc::VIRTUAL_EMPTY, medianc::TYPE, medianc::VALUE>();

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
      return make_sptr<type_t>(type_s::infered_t{}); // no type
    }
    return type_fn(ctx, loc,
                   type_med.value()); // resolve the literal type
  }();

  auto init_val = [&val_med, &ctx, &loc] -> sptr<expr_t> {
    if (val_med)
      return expr_fn(ctx, loc, val_med->fchild().as_median());
    return nullptr;
  }();

  *ptr = decl_s::var_decl_t{std::move(mut_val), std::move(type_val),
                            std::move(init_val)};
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
    locale = locale_t::make_child(loc);
    stmts = make_sptr(stmts_t{});
    *ptr = decl_s::scope_decl_t{locale, stmts};
  }
  const auto body = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body->children());
  // return decl_s::scope_decl_t{locale, stmts};
}
auto type_decl_fn(sptr<decl_t> ptr, context_t &ctx, sptr<locale_t> loc,
                  cursor_helper_t &cursor) -> void {
  auto type_med = cursor.extract<medianc::TYPE>();
  *ptr = decl_s::type_decl_t{type_fn(ctx, loc, type_med.value())};
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
auto decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<decl_t> {
  switch (decl_med.type()) {
  case medianc::DECL: // variables and functions
    return decl_spec_fn<var_decl_fn>(ctx, loc, decl_med);
  case medianc::SCOPE_DECL:
    return decl_spec_fn<scope_decl_fn>(ctx, loc, decl_med);
  case medianc::TYPE_DECL:
    return decl_spec_fn<type_decl_fn>(ctx, loc, decl_med);
  default:
    throw std::runtime_error("unknown declaration type");
  }
  return decl_spec_fn<type_decl_fn>(ctx, loc, decl_med);
}
auto unwrap_decl_fn(context_t &ctx, sptr<locale_t> loc, const median_t decl_med)
    -> sptr<stmt_t> {
  auto cursor = cursor_helper_t(decl_med.children());
  const auto [ids_med, body_med] =
      cursor.tuple_extract<medianc::UNWRAP_IDS, medianc::UNWRAP_BODY>();
  auto arr_ptr =
      ssptr<stmt_s::unwrap_decl_arr_t, stmt_t>(stmt_s::unwrap_decl_arr_t{});
  auto value_ptr = make_sptr<expr_t>();
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
  *value_ptr =
      expr_val_fn(ctx, loc, body_med.value().children().begin()->as_median());
  return arr_ptr.ptr();
}

auto import_fn(context_t &ctx, const median_t import_med) -> sptr<stmt_t> {
  auto cursor = cursor_helper_t{import_med.children()};
  auto strlit_node = cursor.extract<tokc::STRLIT>();

  return make_sptr<stmt_t>(stmt_s::import_t{ctx.toks.str(strlit_node.value())});
}

auto forloop_fn(context_t &ctx, sptr<locale_t> ploc, const median_t for_med)
    -> sptr<stmt_t> {
  auto cursor = cursor_helper_t{for_med.children()};
  auto [ctrl_expr_med, body_med] =
      cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

  auto locale = locale_t::make_child(ploc);
  // ctrl_expr
  auto ctrl_expr_stmts = [&] -> sptr<stmts_t> {
    auto stmts = make_sptr(stmts_t{});
    auto ch = ctrl_expr_med->children();
    // if (ch.size2() != 3)
    //   throw std::runtime_error("for loop ctrl expresion under filled");

    const auto decl_med = ch[0].as_median();
    const auto cmp_expr_med = ch[1].as_median();
    const auto it_expr_med = ch[2].as_median();

    auto decl_ptr = decl_spec_fn<var_decl_fn>(ctx, locale, decl_med);
    auto cmp_expr_ptr = expr_fn(ctx, locale, cmp_expr_med);
    auto it_expr_ptr = expr_fn(ctx, locale, it_expr_med);

    stmts->stmts.push_back(make_sptr<stmt_t>(decl_ptr));
    stmts->stmts.push_back(make_sptr<stmt_t>(cmp_expr_ptr));
    stmts->stmts.push_back(make_sptr<stmt_t>(it_expr_ptr));
    return stmts;
  }();
  // body
  auto body_stmts = [&] -> sptr<stmts_t> {
    auto stmts = make_sptr(stmts_t{});
    auto span = body_med->children();
    consume_stmts_fn(ctx, locale, stmts, span);
    return stmts;
  }();
  return make_sptr<stmt_t>(stmt_s::forloop_t{
      std::move(locale), std::move(ctrl_expr_stmts), std::move(body_stmts)});
}

auto as_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  auto type_med = med.children().begin()->as_median();
  return expr_s::as_t{{}, type_fn(ctx, loc, type_med)};
}

auto expr_operator_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<expr_elm_t> {
  auto node = med.children().begin()->node();
  return make_sptr<expr_elm_t>(ovisit(
      node,
      [&ctx, &loc](const final_t &val) -> expr_elm_t {
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
  auto locale = locale_t::make_child(loc);
  auto stmts = make_sptr(stmts_t{});
  auto cursor = cursor_helper_t{med.children()};

  auto body_med = cursor.extract<medianc::BODY>();
  consume_stmts_fn(ctx, locale, stmts, body_med->children());

  // chain part
  // chain_loop_fn(ctx, loc, cursor);

  return {std::move(locale), std::move(stmts)};
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
  auto decl_or_type = med.children().begin()->as_median();
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
    throw std::runtime_error(
        "\nvoid_t is used as a placeholder for a any callable_t at the moment\n"
        "Callable compound literals are not yet supported "
        "and the grammar around it might change");

    auto locale = locale_t::make_child(loc);
    auto stmts = make_sptr(stmts_t{});
    consume_stmts_fn(ctx, loc, stmts, cinit_med->children());
    // return ????????;
    // maybe change the structure to a more asignment oriented and not function
    // we can't really do that doe
    // this might be used on a expresion
    // this might be used as a declaration
    // so I guess this goes to a later stage to determine the type of the
    // declarations
  } else {
    list<sptr<expr_t>> exprs;
    auto ch = cinit_med->children();
    auto cursor = ch.begin();
    while (ch.contain(cursor)) {
      auto med = cursor->as_median();
      if (med.type() != medianc::EXPR)
        throw std::runtime_error(std::string(medianc::str(med.type())));

      exprs.push_back(expr_fn(ctx, loc, med));
      cursor.advance();
    }
    return expr_s::complit_t{type, exprs};
  }
}

auto if_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_elm_t {
  auto cursor = cursor_helper_t{med.children()};
  auto val = expr_s::if_t{{nullptr, make_sptr(stmts_t{})}, {}, std::nullopt};

  {
    auto if_med = cursor.extract<medianc::IF>();
    auto [ctrl_expr_med, body_med] =
        cursor_helper_t{if_med->children()}
            .tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

    val.intro.ctrl_expr = expr_fn(
        ctx, loc, ctrl_expr_med.value().children().begin()->as_median());
    consume_stmts_fn(ctx, loc, val.intro.stmts, body_med->children());
  }

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
          auto el = expr_s::if_t::else_t{make_sptr(stmts_t{})};
          consume_stmts_fn(ctx, loc, el.stmts, body_med->children());
          val.el = std::move(el);
        }
        break;
      }
      auto cursor = cursor_helper_t{elif_med->children()};
      auto [ctrl_expr_med, body_med] =
          cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();
      auto elif = expr_s::if_t::elif_t{nullptr, make_sptr(stmts_t{})};
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
  return make_sptr<expr_elm_t>(ovisit(
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
  while (ch.contain(cursor)) {
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
  return make_sptr<expr_t>(expr_val_fn(ctx, loc, med));
}

auto ret_fn(context_t &ctx, sptr<locale_t> loc, const median_t med)
    -> sptr<stmt_t> {
  auto cursor = cursor_helper_t(med.children());
  auto expr_med = cursor.extract<medianc::EXPR>();
  return make_sptr<stmt_t>(stmt_s::ret_t{expr_fn(ctx, loc, expr_med.value())});
}

auto stmt_fn(context_t &ctx, sptr<locale_t> loc, const median_t stmt_med)
    -> sptr<stmt_t> {
  const auto med = stmt_med.children().begin()->as_median();
  const auto type = med.type();
  switch (type) {
  case medianc::DECL:
    return make_sptr<stmt_t>(decl_spec_fn<var_decl_fn>(ctx, loc, med));
  case medianc::SCOPE_DECL:
    return make_sptr<stmt_t>(decl_spec_fn<scope_decl_fn>(ctx, loc, med));
  case medianc::TYPE_DECL:
    return make_sptr<stmt_t>(decl_spec_fn<type_decl_fn>(ctx, loc, med));
  case medianc::UNWRAP_DECL:
    return unwrap_decl_fn(ctx, loc, med);
  case medianc::FOR:
    return forloop_fn(ctx, loc, med);
  case medianc::IMPORT:
    return import_fn(ctx, med);
  case medianc::RETURN:
    return ret_fn(ctx, loc, med);
  case medianc::EXPR:
    return make_sptr<stmt_t>(expr_fn(ctx, loc, med));
  default:
    throw std::runtime_error(std::string(medianc::str(type)) + " " +
                             "Unknown stmt type");
  }
}

auto consume_stmts_fn(context_t &ctx, sptr<locale_t> loc, sptr<stmts_t> stmts,
                      span_t span) -> void {
  for (auto &elm : span)
    stmts->stmts.push_back(stmt_fn(ctx, loc, elm.as_median()));
}

auto entry(const token_buffer_t &toks, grammar::node_t &root_node) -> auto {
  auto ctx = context_t{toks, 0};
  auto locale = make_sptr(locale_t{});
  auto stmts = make_sptr(stmts_t{});
  auto span = root_node.as_median().children();

  consume_stmts_fn(ctx, locale, stmts, span);

  return std::tuple{locale, stmts};
}

} // namespace symbols

} // namespace semantics
