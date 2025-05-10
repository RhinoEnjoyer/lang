#include "../become.hpp"
#include "../overloaded.hpp"
#include "./parser.hpp"

#include "../str_lit.hpp"
#include "../token_str.hpp"
#include <iostream>
#include <print>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

// we need better error handling

// There is some jank about the use of ENDSTMT/; because sometimes it is checked
// by the called function but sometimes it is checked and consumed by the
// symetrical function that is called it is fine but maybe I should have it
// standardised I have the stmt<> fucntion but that one consumes the token maybe
// make that the one that just expects it and make another one that consumes it?
// or the oposite?
// idk
// It hasn't caused any problems but it is good to have in mind in case it does

// The comptime arguemnts have a problem for now I use the type function and
// when it fails I fall back to the expresion function The problem is that if it
// detects lets say a @rec it is gonna think it is a type but it might also be a
// compound lit one way is the only allow comp_lits when using the {@rec(a:s32)}
// syntax There is also a problem with ids because they might refer to a type or
// it might be a expresion so I guess we need some backtracking to go back and
// change the type of the median to a expresion and change the state so we can
// match those patterns

// compound literals and compiletime arguments???
// they do not really need them
// since

// I should have typeof and sizeof take in type_or_expr
//  this would push some work over to the semantic analyser
//  but it would maybe help a bit with clarity
//  we can use the context of the semantic analyser to get
//  some better results

/* @TODO */
/* better errors */
/* attributes syntax */
/* Consolidate functions for the @self and @pipe? maybe make it a unique start
 *   and but make it like a chain? */

namespace grammar {

// struct context_t {
//   const token_buffer_t &toks;
//   const std::map<size_t, size_t> &smap;
//   podlist_t<node_t> &nodes;
// };

// #define DISPATCH_ARGS_DECL                                                     \
//   const token_buffer_t &toks, podlist_t<node_t> &buffer, cursor_t &cursor
// #define DISPATCH_ARGS toks, buffer, cursor

#define DISPATCH_ARGS_DECL context_t &ctx, cursor_t &cursor
#define DISPATCH_ARGS ctx, cursor

#define DISPATCH_FNSIG (DISPATCH_ARGS_DECL)->void
#define DISPATCH_LAM [] DISPATCH_FNSIG

using fn_t = auto DISPATCH_FNSIG;
// using pair_t = std::pair<tokc::e,fn_t*>;
struct pair_t {
  tokc::e first;
  fn_t *second;
};

template <std::size_t N, typename T = pair_t>
using table_t_impl = std::array<pair_t, N>;

template <typename T>
concept Table = requires(T t, std::size_t i) {
  { t.begin() };
  { t.end() };
  { t.size() } -> std::convertible_to<std::size_t>;
  { t[i] };
};

template <typename T>
concept Pair = requires(T t) {
  { t.first };
  { t.second };
};

template <std::size_t N> using table_t = table_t_impl<N>;

template <Pair... T> static consteval auto table_t_make(T... args) -> auto {
  return table_t<sizeof...(T)>{args...};
}

template <std::array... args> static consteval auto table_t_make() -> auto {
  constexpr std::size_t tsize =
      (args.size() + ...); // Folding to calculate total size

  constexpr auto table = [] consteval -> auto {
    auto table = table_t<tsize>{};
    std::size_t index = 0;
    (
        [&](const auto &t) consteval {
          for (std::size_t i = 0; i < t.size(); ++i)
            table[index++] = t[i];
        }(args),
        ...);
    return table;
  }();
  static_assert(!([&] consteval -> bool {
    for (auto i = 0; i < table.size(); i++)
      for (auto j = i + 1; j < table.size(); j++)
        if (table[i].first == table[j].first)
          return true;
    return false;
  }()),"The table contains confilicting tokens");

  return table;
}

template <std::size_t len = 1>
[[clang::always_inline]] inline auto advance DISPATCH_FNSIG {
  cursor.advance(len);
}

template <Table T>
consteval auto table_t_get_fns(T t) -> std::array<fn_t *, t.size()> {
  auto N = t.size();
  std::array<fn_t *, t.size()> arr{};
  for (std::size_t i = 0; i < N; ++i) {
    arr[i] = t[i].second;
  }
  return arr;
}

template <Table T>
consteval auto table_t_get_codes(T t) -> std::array<tokc::e, t.size()> {
  auto N = t.size();
  std::array<tokc::e, t.size()> arr{};
  for (std::size_t i = 0; i < N; ++i) {
    arr[i] = t[i].first;
  }
  return arr;
}

auto push_final_impl DISPATCH_FNSIG {
  ctx.nodes.push_back(node_t::make(cursor));
  advance(DISPATCH_ARGS);
}

template <tokc::e tok = tokc::VIRTUAL_EMPTY> auto push_final DISPATCH_FNSIG {
  if constexpr (tok != tokc::VIRTUAL_EMPTY) {
    expect<tok>(DISPATCH_ARGS);
  }
  become push_final_impl(DISPATCH_ARGS);
}

template <node_t::median_t::code_t type, fn_t *fn> auto dive DISPATCH_FNSIG {
  ctx.nodes.push_back(node_t::make(type, 0, cursor));
  const auto parent_index = ctx.nodes.length() - 1;
  const auto index = ctx.nodes.length();

  fn(DISPATCH_ARGS);

  const auto length = ctx.nodes.length() - index;
  auto &med = ctx.nodes.at(parent_index).as_raw_median();
  med.last = cursor;
  med.len_ = length;
}

template <std::array type>
[[nodiscard]] inline auto is(cursor_t cursor) -> bool {
  return std::apply(
      [&](auto... elems) { return ((cursor->type_ == elems) || ...); }, type);
}
template <tokc::e... type>
[[nodiscard]] inline auto is(cursor_t cursor) -> bool {
  // return cursor->type_ == type;
  become is<std::array{type...}>(cursor);
}

template <const tokc::e... type> auto expected DISPATCH_FNSIG {
  std::cerr << "Found str: \'" << ctx.toks.str(cursor) << "\'"
            << "\n\tType: " << ctx.toks.str(cursor)
            << "\n\tRow: " << ctx.toks.row(cursor)
            << "\tCol: " << ctx.toks.col(cursor)
            << "\tLen: " << ctx.toks.len(cursor) << "\n";

  std::cerr << "Expected: ";
  ((std::cerr << token_code_str(type) << " "), ...);
  std::cerr << "\b\n\tFound: " << token_code_str(cursor->type_) << '\n';
  std::abort();
}

auto base DISPATCH_FNSIG;
auto decl DISPATCH_FNSIG;
__attribute__((visibility("default")))
auto expr(context_t &ctx, cursor_t &cursor) -> void;
__attribute__((visibility("default")))
auto type(context_t &ctx, cursor_t &cursor) -> void;
template <bool compound_ext = false> auto chain DISPATCH_FNSIG;

template <str_lit_t reason, tokc::e... type> auto expected DISPATCH_FNSIG {
  std::cerr << "Err: " << reason.value << std::endl;
  expected<type...>(DISPATCH_ARGS);
}

template <tokc::e... type>
auto expect(context_t &ctx, cursor_t &cursor) -> void {
  if (!is<type...>(cursor)) [[unlikely]]
    become expected<type...>(DISPATCH_ARGS);
}

template <tokc::e... type>
auto expect(context_t &ctx, cursor_t &&cursor) -> void {
  if (!is<type...>(cursor)) [[unlikely]]
    expected<type...>(DISPATCH_ARGS);
}

template <str_lit_t reason, tokc::e... type> auto expect DISPATCH_FNSIG {
  if (!is<type...>(cursor)) [[unlikely]]
    become expected<reason, type...>(DISPATCH_ARGS);
}

template <auto args, fn_t fallback_ = nullptr,
          std::size_t match_cursor_offset = 0>
auto path DISPATCH_FNSIG {
  // constexpr auto a = std::array<pair_t,args.size()>{args};
  // subhuman function
  constexpr auto dispatch_table = [] consteval -> auto {
    if constexpr (!std::is_constant_evaluated())
      static_assert(false, "Call table is not constructed on compile-time");

    constexpr auto arg_arr = args;

    std::array<fn_t *, tokc::length()> table = {};
    auto fallback = [] consteval -> auto {
      if constexpr (fallback_ != nullptr) {
        return fallback_;
      } else {
        return DISPATCH_LAM {
          auto filter = [&] consteval -> auto {
            auto result = std::array<tokc::e, args.size()>{};
            std::transform(
                args.begin(), args.end(), result.begin(),
                [&](const auto &pair) constexpr -> auto { return pair.first; });
            return result;
          }();

          std::cerr << "Str: \'" << ctx.toks.str(cursor) << "\'"
                    << "\n\tType: " << token_code_str(ctx.toks.type(cursor))
                    << "\n\tRow: " << ctx.toks.row(cursor)
                    << "\tCol: " << ctx.toks.col(cursor)
                    << "\tLen: " << ctx.toks.len(cursor) << "\n";
          std::cerr << "Failed to pick a path" << '\n' << "Expected: \n\t";

          for (const auto &elm : filter)
            std::cerr << token_code_str(elm) << "; ";
          std::cerr << "\n";

          std::abort();
        };
      }
    }();
    for (std::size_t i = 0; i < tokc::length(); i++)
      table[i] = fallback;

    for (const pair_t &pair : arg_arr)
      table[pair.first] = pair.second;
    return table;
  }();

  become dispatch_table[(cursor + match_cursor_offset)->type_](ctx, cursor);
}

template <fn_t *fn, fn_t *...fns> auto sequence DISPATCH_FNSIG {
  if constexpr (sizeof...(fns) == 0)
    become fn(DISPATCH_ARGS);

  fn(DISPATCH_ARGS);

  if constexpr (sizeof...(fns) > 0)
    become sequence<fns...>(DISPATCH_ARGS);
  else {
    std::unreachable();
    return;
  }
}
template <tokc::e expected_token> auto consume DISPATCH_FNSIG {
  become sequence<expect<expected_token>, advance>(DISPATCH_ARGS);
}

template <fn_t *fn, medianc::e med = medianc::SYMETRICAL>
auto symetrical_impl DISPATCH_FNSIG {
  advance(DISPATCH_ARGS);
  if (is<tokc::ENDGROUP, tokc::ENDSTMT>(cursor)) [[unlikely]] {
    advance(DISPATCH_ARGS);
    // if (is<tokc::ENDSTMT>(cursor))
    //   advance(DISPATCH_ARGS);
    return ctx.nodes.push_back(node_t::make(med, 0, cursor));
  }
  dive < med, DISPATCH_LAM {
    // size_t count = 0;
    do {
      // std::cout << count << "\n";
      // no fallback
      fn(DISPATCH_ARGS);
      expect<tokc::ENDSTMT>(DISPATCH_ARGS);
      advance(DISPATCH_ARGS);
      // count++;
    } while (!is<tokc::ENDGROUP>(cursor));
    advance(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

template <fn_t *fn, medianc::e med = medianc::SYMETRICAL>
auto symetrical DISPATCH_FNSIG {
  symetrical_impl<fn, med>(DISPATCH_ARGS);
};

template <fn_t *fn, tokc::e type, medianc::e med = medianc::symetrical(type)>
auto symetrical DISPATCH_FNSIG {
  static_assert(tokc::is_open_symetrical(type),
                "type template argument is not a symetrical");
  expect<type>(DISPATCH_ARGS);
  become symetrical_impl<fn, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::DBRACES>
auto dbraces DISPATCH_FNSIG {
  become symetrical<fn, tokc::LDBRACE, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::PARENS>
auto parens DISPATCH_FNSIG {
  become symetrical<fn, tokc::LPAREN, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::BRACES>
auto braces DISPATCH_FNSIG {
  become symetrical<fn, tokc::LBRACE, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::CBRACES>
auto cbraces DISPATCH_FNSIG {
  become symetrical<fn, tokc::LCBRACE, med>(DISPATCH_ARGS);
}

template <fn_t *fn, tokc::e type, medianc::e med>
auto symetrical_wrapper DISPATCH_FNSIG {
  static_assert(tokc::is_open_symetrical(type),
                "type template argument is not a symetrical");
  expect<type>(DISPATCH_ARGS);
  advance(DISPATCH_ARGS);

  dive < med, DISPATCH_LAM {
    fn(DISPATCH_ARGS);

    expect<"Failed to close symetrical wrapper", tokc::ENDGROUP>(DISPATCH_ARGS);
    advance(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::PARENS>
auto parens_wrapper DISPATCH_FNSIG {
  become symetrical_wrapper<fn, tokc::LPAREN, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::BRACES>
auto braces_wrapper DISPATCH_FNSIG {
  become symetrical_wrapper<fn, tokc::LBRACE, med>(DISPATCH_ARGS);
}
template <fn_t *fn, medianc::e med = medianc::CBRACES>
auto cbraces_wrapper DISPATCH_FNSIG {
  become symetrical_wrapper<fn, tokc::LCBRACE, med>(DISPATCH_ARGS);
}

template <fn_t *fn> auto advance2 DISPATCH_FNSIG {
  advance(DISPATCH_ARGS);
  fn(DISPATCH_ARGS);
}

template <medianc::e dive_code, auto fn> auto advance2 DISPATCH_FNSIG {
  become dive<dive_code, advance2<fn>>(DISPATCH_ARGS);
}

template <fn_t *fn> auto end_with_scolon DISPATCH_FNSIG {
  become sequence<fn, consume<tokc::ENDSTMT>>(DISPATCH_ARGS);
}

auto type_or_expr DISPATCH_FNSIG;
const auto &template_arg = type_or_expr;
// const auto& compound_literal = type<nullptr, 1>;

auto empty DISPATCH_FNSIG { /*Do nothing */ return; }

static auto &as =
    sequence<advance, cbraces_wrapper<end_with_scolon<type>, medianc::AS>>;

[[clang::always_inline]] inline auto attributes DISPATCH_FNSIG {
  become dbraces<chain, medianc::ATTRIBUTES>(DISPATCH_ARGS);
}

template <medianc::e wrap, fn_t *fn, fn_t *fallback = fn>
auto attribute_path DISPATCH_FNSIG {
  dive<wrap, path<table_t_make(pair_t{tokc::LDBRACE, sequence<attributes, fn>}),
                  fallback>>(DISPATCH_ARGS);
}

auto template_arg_decl DISPATCH_FNSIG;

auto template_arg_list DISPATCH_FNSIG {
  cbraces<attribute_path<medianc::ARGUMENT, template_arg_decl>,medianc::TEMPLATE_ARGUMENT_LIST>(DISPATCH_ARGS);
}

// template <bool enable_state = true, bool enable_template = true>
auto fnpath DISPATCH_FNSIG;
auto access_chain DISPATCH_FNSIG;

namespace function_types {
template<bool is_sig>
auto fnsig_template DISPATCH_FNSIG {
  constexpr auto dive_type = (is_sig) ? medianc::FN_SIG : medianc::FN_TEMPLATE;
  constexpr auto arg_fn = (is_sig) ? type : decl;
  dive < dive_type, DISPATCH_LAM {
    parens<dive<medianc::ARGUMENT, arg_fn>, medianc::FN_ARGS>(DISPATCH_ARGS);
    if (is<tokc::LPAREN>(cursor)) [[likely]]
      parens_wrapper<end_with_scolon<type>, medianc::FN_RET>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}
} // namespace function_types

auto fnpath DISPATCH_FNSIG {
  advance(DISPATCH_ARGS);
  constexpr auto fnsig_template = function_types::fnsig_template<false>;
  constexpr auto fnsig_sig = function_types::fnsig_template<true>;
  constexpr auto table = table_t_make(pair_t{tokc::COLON, fnsig_template});
  // 0 paren 1 ID 2 COLON
  path<table, fnsig_sig, 2>(DISPATCH_ARGS);
}

auto fndecl_sig DISPATCH_FNSIG {
  dive < medianc::FN_SIG, DISPATCH_LAM {
    advance(DISPATCH_ARGS);
    if (is<tokc::LBRACE>(cursor))
      symetrical<expr, medianc::FN_STATE_LIST>(DISPATCH_ARGS);

    // Compile time arguments declaration
    if (is<tokc::LCBRACE>(cursor))
      template_arg_list(DISPATCH_ARGS);

    constexpr auto arg_fn = [] consteval { return decl; }();
    if (is<tokc::LPAREN>(cursor)) [[likely]] {
      // Runtime Arguments
      parens<attribute_path<medianc::ARGUMENT,
                            path<table_t_make(pair_t{
                                     tokc::BUILTIN_SELF,
                                     dive<medianc::SELF_ARG, push_final<>>}),
                                 arg_fn>>,
             medianc::FN_ARGS>(DISPATCH_ARGS);
      if (is<tokc::LPAREN>(cursor)) [[likely]]
        // Return type
        //  parens<attribute_path<medianc::ELEMENT ,type>,
        //  medianc::FN_RET>(DISPATCH_ARGS);
        parens_wrapper<end_with_scolon<type>, medianc::FN_RET>(DISPATCH_ARGS);
    } else if (is<tokc::MINUSGREATER>(cursor)) {
      advance(DISPATCH_ARGS);
      parens_wrapper<end_with_scolon<type>, medianc::FN_RET>(DISPATCH_ARGS);
    }
  }
  > (DISPATCH_ARGS);
};

template <medianc::e wrap, fn_t *fn> auto custom_chain DISPATCH_FNSIG {
  become dive<wrap, sequence<fn, path<table_t_make(pair_t{
                                          tokc::DCOLON, advance2<chain<true>>}),
                                      empty>>>(DISPATCH_ARGS);
};

auto init_list DISPATCH_FNSIG {
  become
      advance2<custom_chain<medianc::INIT_LIST, parens<expr, medianc::BODY>>>(
          DISPATCH_ARGS);
}
auto result DISPATCH_FNSIG {
  become custom_chain<medianc::RESULT, parens<base, medianc::BODY>>(
      DISPATCH_ARGS);
}

constexpr auto chain_1_table = table_t_make(pair_t{tokc::ID, push_final});

auto template_init DISPATCH_FNSIG {
  cbraces<template_arg, medianc::TEMPLATE_INSTATIATION>(DISPATCH_ARGS);
}

namespace chain_aux {
constexpr auto fncall =
    pair_t{tokc::LPAREN, parens<expr, medianc::FUNCTION_CALL>};
constexpr auto array_access =
    pair_t{tokc::LBRACE, braces<expr, medianc::ARRAY_ACCESS>};
constexpr auto template_init = pair_t{tokc::LCBRACE, grammar::template_init};

constexpr auto inline_op = table_t_make(
    pair_t{tokc::AMPERSAND, push_final}, 
    pair_t{tokc::PERISPOMENI, push_final}
);
} // namespace chain_aux

constexpr auto chain_2_table =
    table_t_make<
    chain_aux::inline_op,
    table_t_make(
        chain_aux::fncall, 
        chain_aux::array_access,
        chain_aux::template_init)>();

[[clang::always_inline]]
inline auto chain_mid DISPATCH_FNSIG {
  do {
    path<chain_1_table>(DISPATCH_ARGS);
  AGAIN:
    if (is<tokc::DCOLON>(cursor)) [[unlikely]] {
      advance(DISPATCH_ARGS);
    } else {
      if (is<table_t_get_codes(chain_2_table)>(cursor)) {
        path<chain_2_table>(DISPATCH_ARGS);
        goto AGAIN;
      } else {
        break;
      }
    }
  } while (true);
}

template <bool compound_ext>
auto chain(context_t &ctx, cursor_t &cursor) -> void {
  static auto impl = [](context_t &ctx, cursor_t &cursor) -> void {
    if constexpr (compound_ext) {
      path<table_t_make<chain_1_table, chain_2_table>()>(ctx, cursor);
      constexpr auto codes = table_t_get_codes(chain_2_table);
      while (is<codes>(cursor)) {
        path<chain_2_table>(ctx, cursor);
      }

      if (!is<tokc::DCOLON>(cursor))
        return;
      advance(ctx, cursor);
    }
    chain_mid(DISPATCH_ARGS);
  };

  if constexpr (compound_ext) {
    impl(DISPATCH_ARGS);
  } else {
    dive<medianc::CHAIN, impl>(DISPATCH_ARGS);
  }
}
auto access_chain DISPATCH_FNSIG {
  static auto impl = [](context_t &ctx, cursor_t &cursor) -> void {
    constexpr auto table =
        table_t_make(pair_t{tokc::LCBRACE, template_arg_list});
    do {
      path<chain_1_table>(DISPATCH_ARGS);
    AGAIN:
      if (is<tokc::DCOLON>(cursor)) [[unlikely]] {
        advance(DISPATCH_ARGS);
      } else {
        if (is<table_t_get_codes(table)>(cursor)) {
          path<table>(DISPATCH_ARGS);
          goto AGAIN;
        } else {
          break;
        }
      }
    } while (true);
  };

  dive<medianc::CHAIN, impl>(DISPATCH_ARGS);
  // we just access ids, so we need to be able to do templates
}

auto collection_alias_decl DISPATCH_FNSIG {
  dive<medianc::COLLECTION_DECL,
       sequence<chain,
                path<table_t_make(pair_t{tokc::COLON, sequence<advance, type>}),
                     empty>>>(DISPATCH_ARGS);
}

auto enum_elm DISPATCH_FNSIG {
  push_final<tokc::ID>(DISPATCH_ARGS);
  consume<tokc::COLON>(DISPATCH_ARGS);
  become expr(DISPATCH_ARGS);
}

// auto comptime_arglist DISPATCH_FNSIG {
//   become cbraces<attribute_path<medianc::ARGUMENT, template_arg_decl>,
//                  medianc::TEMPLATE_ARGUMENT_LIST>(DISPATCH_ARGS);
// }

template <fn_t *elm, medianc::e med> auto aggregate_decl_pat DISPATCH_FNSIG {
  advance(DISPATCH_ARGS);
  dive < med, DISPATCH_LAM {
    // if (is<tokc::LCBRACE>(cursor))
    //   template_arg_list(DISPATCH_ARGS);
    parens<elm, medianc::BODY>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

// auto collection_fn DISPATCH_FNSIG {
//   become aggregate_decl_pat<
//       attribute_path<medianc::ELEMENT, collection_alias_decl>,
//       medianc::COLLECTION>(DISPATCH_ARGS);
// }
auto record_fn DISPATCH_FNSIG {
  // become aggregate_decl_pat<base, medianc::RECORD>(DISPATCH_ARGS);
  become aggregate_decl_pat<dive<medianc::ELEMENT, decl>, medianc::RECORD>(DISPATCH_ARGS);
}
auto union_fn DISPATCH_FNSIG {
  become aggregate_decl_pat<base, medianc::UNION>(DISPATCH_ARGS);
}
auto enum_fn DISPATCH_FNSIG {
  become aggregate_decl_pat<dive<medianc::ELEMENT, enum_elm>, medianc::ENUM>(
      DISPATCH_ARGS);
}
auto tup_fn DISPATCH_FNSIG {
  return sequence<consume<tokc::BUILTIN_TUP>,
                  parens<dive<medianc::ELEMENT, type>, medianc::TUPLE>>(
      DISPATCH_ARGS);
}

auto collection_fn DISPATCH_FNSIG {
  return sequence<consume<tokc::BUILTIN_UNION>,
                  parens<dive<medianc::ELEMENT, type>, medianc::UNION>>(
      DISPATCH_ARGS);
}

constexpr auto aggregate_table = table_t_make(
    pair_t{tokc::BUILTIN_TUP, tup_fn},
    pair_t{tokc::BUILTIN_REC, record_fn}, 
    pair_t{tokc::BUILTIN_UNION, collection_fn},
    pair_t{tokc::BUILTIN_ENUM, enum_fn});

constexpr auto integral_table = table_t_make(
    pair_t{tokc::TYPE_INT, push_final}, pair_t{tokc::TYPE_UINT, push_final},
    pair_t{tokc::TYPE_FLOAT, push_final},
    pair_t{tokc::TYPE_BOOLEAN, push_final});

constexpr auto anon_type_table =
    table_t_make<aggregate_table,
                 table_t_make(pair_t{tokc::BUILTIN_FN, fnpath})>();

static constexpr auto type_prefix_table = table_t_make(
    pair_t{tokc::PERISPOMENI, dive<medianc::PTR, advance2<type>>},
    pair_t{tokc::CARET, dive<medianc::IMMUTABLE_PTR, advance2<type>>},
    pair_t{tokc::LBRACE, symetrical<expr>} // todo //why did I comment this out
);

constexpr auto type_table =
    table_t_make<anon_type_table, integral_table,
                 table_t_make(
                              pair_t{tokc::BUILTIN_TYPEOF,
                                     advance2<parens<expr, medianc::TYPEOF>>},
                              pair_t{tokc::BUILTIN_PTR, push_final<>},
                              pair_t{tokc::BUILTIN_VOID, push_final<>})>();

auto get_index(podlist_t<node_t> &nodes) -> std::uint64_t {
  return nodes.size();
}

template <fn_t *fn> auto extend(std::uint64_t index, DISPATCH_ARGS_DECL) {
  fn(DISPATCH_ARGS);
  auto length = ctx.nodes.length() - index;
  ctx.nodes.at(index).as_raw_median().len_ = length;
};

auto type DISPATCH_FNSIG {
  static constexpr auto inner_type_table = [] consteval -> auto {
    return table_t_make<type_table, type_prefix_table,
                        table_t_make(pair_t{tokc::ID, chain<>})>();
  }();

  auto impl = [](DISPATCH_ARGS_DECL) static -> void {
    // if (is<table_t_get_codes(type_prefix_table)>(cursor)) {
    //   dive < medianc::TYPE_PREFIX, DISPATCH_LAM {
    //     // do {
    //       path<type_prefix_table>(DISPATCH_ARGS);

    //     // } while (is<table_t_get_codes(type_prefix_table)>(cursor));
    //   }
    // > (DISPATCH_ARGS);
    // }else{

    // }
    path<inner_type_table, nullptr>(DISPATCH_ARGS);
  };

  // prefix
  dive<medianc::TYPE, impl>(DISPATCH_ARGS);
}

auto while_fn DISPATCH_FNSIG;
auto for_fn DISPATCH_FNSIG;
auto switch_fn DISPATCH_FNSIG;
// auto alias_decl DISPATCH_FNSIG;
template <bool list_type = false,
          fn_t *intro = cbraces<type, medianc::COMPOUND_LITERAL_TYPE>>
auto complit_fn DISPATCH_FNSIG;

namespace if_stmt {
// if I do not use cbraces I can't know when the next set of parens will be a
// control expresion or a else stmt
//  It can be done
//                  ctrl body
// elif stmt is (...)(...)
//                   body
// and else stmt is (...)
//
// if I have ParensParens elif
// elif Parens -> else
// which means I need to look ahead to check what I have
// but because everything is stored in a array it migth be a bit more complex
// I can do what I do on the lexer
// or when lexing I can keep a table with the pair token's indexes
const auto &ctrl_expr =
    parens_wrapper<end_with_scolon<expr>, medianc::CTRL_EXPR>;
const auto &body = parens<base, medianc::BODY>;
const auto &if_branch = sequence<ctrl_expr, body>;

auto if_else_path DISPATCH_FNSIG;
constexpr auto elif_pair = pair_t{
    tokc::LPAREN, sequence<dive<medianc::IF, if_branch>, if_else_path>};
constexpr auto el_pair = pair_t{tokc::LCBRACE, dive<medianc::ELSE, body>};
constexpr auto set = table_t_make(elif_pair, el_pair);
auto if_else_path DISPATCH_FNSIG {
  if (!is<tokc::LPAREN>(cursor))
    return;
  const auto cursor_index = ctx.toks.to_index(cursor);
  const auto pair_index = ctx.smap.at(cursor_index);
  const auto pair_cursor = cursor_t(&ctx.toks.toks.at(pair_index)).advance();
  if (pair_cursor->isa(tokc::LPAREN))
    sequence<dive<medianc::IF, if_branch>, if_else_path>(DISPATCH_ARGS);
  else
    dive<medianc::ELSE, body>(DISPATCH_ARGS);
}

auto fn DISPATCH_FNSIG {
  become dive < medianc::IF_EXPR, DISPATCH_LAM {
    advance(DISPATCH_ARGS);
    dive<medianc::IF, if_branch>(DISPATCH_ARGS);

    if (!is<tokc::LPAREN>(cursor))
      return;

    if_else_path(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}
} // namespace if_stmt

auto consume DISPATCH_FNSIG { become advance(DISPATCH_ARGS); }

constexpr auto operand_expr_table = table_t_make<
    // compound_lit_table,
    table_t_make(
        pair_t{tokc::BUILTIN_FN,
               complit_fn<true,
                          dive<medianc::TYPE, fndecl_sig /* <true, false> */>>},
        pair_t{tokc::BUILTIN_REC,
               complit_fn<false, dive<medianc::TYPE, record_fn>>},
        pair_t{tokc::BUILTIN_ENUM,
               complit_fn<false, dive<medianc::TYPE, enum_fn>>},
        pair_t{tokc::BUILTIN_UNION,
               complit_fn<false, dive<medianc::TYPE, union_fn>>},
        pair_t{tokc::ID, chain},
        pair_t{tokc::LPAREN,
               custom_chain<medianc::RESULT, parens<expr, medianc::BODY>>},
        pair_t{tokc::LCBRACE,
               sequence<custom_chain<medianc::BLOCK_EXPR,
                                     cbraces<base, medianc::BODY>>>},

        // pair_t{tokc::LCBRACE, complit_fn},
        pair_t{tokc::BUILTIN_DUCKLING,
               path<table_t_make(
                        pair_t{tokc::LCBRACE, sequence<consume, complit_fn>},
                        pair_t{tokc::LPAREN, sequence<consume, init_list>}),
                    nullptr, 1>},
        pair_t{tokc::BUILTIN_SELF,
               custom_chain<medianc::SELF, consume<tokc::BUILTIN_SELF>>},
        pair_t{tokc::BUILTIN_PIPE,
               custom_chain<medianc::PIPE, consume<tokc::BUILTIN_PIPE>>},
        pair_t{tokc::INT, push_final}, pair_t{tokc::FLOAT, push_final},
        pair_t{tokc::STRLIT, push_final},
        pair_t{tokc::BUILTIN_NULL, push_final},
        pair_t{tokc::BUILTIN_IF, if_stmt::fn},
        pair_t{tokc::BUILTIN_SET, advance2<parens<expr, medianc::SET>>},
        pair_t{tokc::BUILTIN_SIZEOF,
               advance2<path<table_t_make(
                   pair_t{tokc::LPAREN, parens_wrapper<end_with_scolon<expr>,
                                                       medianc::SIZEOF>},
                   pair_t{tokc::LCBRACE, cbraces_wrapper<end_with_scolon<type>,
                                                         medianc::SIZEOF>})>>},
        pair_t{tokc::BUILTIN_WHILE, while_fn})>();

#define TOKEN_OPERATOR(CODE) pair_t{tokc::CODE, push_final},
constexpr auto symbol_operator_table = table_t_make(
#include "../token.def"
    pair_t{tokc::VIRTUAL_EMPTY, empty});

constexpr auto operator_expr_table =
    table_t_make<symbol_operator_table,
                 table_t_make(pair_t{tokc::BUILTIN_AS, as})>();

constexpr auto expr_table =
    table_t_make<operand_expr_table, operator_expr_table>();

// if I droped generic types or generic expresions I could make something
// but dropping generic types is not happening and dropping generic expresions
// would suck cock.Thus I am keeping both without changing the syntax,
// droping the responisibilty to me when writing the semantic analyser :).

// The best and most straight foward solution is to mark this as ambiguous and
// pass this to the semantic analyser.

//:( THE ONLY OPTION IS TO PASS IT THERE because we are gonna
// need to handle the single chain anywaysBU
auto type_or_expr DISPATCH_FNSIG {
  // get all the tokens in the stmt
  dive < medianc::AMBIGUOUS, DISPATCH_LAM {
    std::int32_t open_symetrical = 0;
    do {
      if (tokc::is_open_symetrical(cursor->type_)) {
        open_symetrical++;
      } else if (tokc::is_close_symetrical(cursor->type_)) {
        open_symetrical--;
      } else if (is<tokc::ENDSTMT>(cursor)) {
        if (open_symetrical == 0) {
          // preserve the ; becuase we are
          // gona use it for when we use
          // the correct function to parse
          // it and hijack the real parse
          // we might segfault without it :)
          // tree
          ctx.nodes.push_back(node_t::make(cursor));
          break;
        }
      }
      push_final(DISPATCH_ARGS);
    } while (true);
  }
  > (DISPATCH_ARGS);
}

const auto &loop_body =
    parens<path<table_t_make(pair_t{tokc::BUILTIN_BREAK, push_final}), base>,
           medianc::BODY>;

auto while_fn DISPATCH_FNSIG {
  become
      dive<medianc::WHILE,
           sequence<consume<tokc::BUILTIN_WHILE>,
                    parens_wrapper<end_with_scolon<expr>, medianc::CTRL_EXPR>,
                    loop_body>>(DISPATCH_ARGS);
}

auto for_fn DISPATCH_FNSIG {
  become dive<medianc::FOR,
              sequence<consume<tokc::BUILTIN_FOR>,
                       parens_wrapper<sequence<end_with_scolon<decl>,
                                               end_with_scolon<expr>,
                                               end_with_scolon<expr>>,
                                      medianc::CTRL_EXPR>,
                       loop_body>>(DISPATCH_ARGS);
}

auto switch_fn DISPATCH_FNSIG {
  static constexpr auto case_fn =
      dive<medianc::CASE,
           sequence<consume<tokc::BUILTIN_CASE>,
                    parens<expr, medianc::CTRL_EXPR>, parens<base>>>;
  become dive<medianc::SWITCH,
              sequence<consume<tokc::BUILTIN_SWITCH>,
                       parens<expr, medianc::CTRL_EXPR>, parens<case_fn>>>(
      DISPATCH_ARGS);
}

// TODO: there is a bug where if put a fn in the {...}
//  the result will not be correct because fns need a new locale_t and stmts_t
//  and it is not yet excluded from the type table
template <bool list_type, fn_t *intro> auto complit_fn DISPATCH_FNSIG {
  constexpr fn_t *fn = [] consteval -> fn_t * {
    if constexpr (intro != cbraces<type, medianc::COMPOUND_LITERAL_TYPE>)
      return dive<medianc::COMPOUND_LITERAL_TYPE, intro>;
    else
      return intro;
  }();

  constexpr fn_t *body_type = [] consteval -> fn_t * {
    if constexpr (!list_type) {
      return parens<expr, medianc::COMPOUND_LITERAL_INIT>;
    } else {
      return parens<base, medianc::COMPOUND_LITERAL_INIT>;
    }
  }();

  dive<medianc::COMPOUND_LITERAL,
       sequence<fn, consume<tokc::DCOLON>,
                path<table_t_make(pair_t{tokc::LCBRACE, template_init}), empty>,
                body_type,
                path<table_t_make(
                         pair_t{tokc::DCOLON, sequence<advance, chain<true>>}),
                     empty>>>(DISPATCH_ARGS);
}

auto expr DISPATCH_FNSIG {
  if (is<tokc::ENDSTMT>(cursor)) [[unlikely]] {
    expected<"Expected an expresion">(ctx, cursor);
    std::cerr << "Can't have empty expresions" << '\n';
    std::abort();
    consume(DISPATCH_ARGS);
    return;
  }
  // static constexpr auto operand_codes =
  // table_t_get_codes(operand_expr_table);
  static constexpr auto operator_codes = table_t_get_codes(operator_expr_table);

  dive < medianc::EXPR, DISPATCH_LAM {
    do {
      if (is<operator_codes>(cursor))
        dive<medianc::OPERATOR, path<operator_expr_table>>(DISPATCH_ARGS);
      else
        dive<medianc::OPERAND, path<operand_expr_table>>(DISPATCH_ARGS);
    } while (__builtin_expect((bool)(!is<tokc ::ENDSTMT>(cursor)), true));
  }
  > (DISPATCH_ARGS);
}

auto var_decl DISPATCH_FNSIG {
  dive < medianc::DECL, DISPATCH_LAM {
    push_final(DISPATCH_ARGS);

    const auto must_infer_type = [&] -> bool {
      if (is<tokc::COLONASIGN>(cursor)) [[unlikely]]
        return true;
      advance(DISPATCH_ARGS);

      if (is<tokc::ENDSTMT>(cursor)) [[unlikely]]
        expected<"Declaration needs a type ,attribute list, and/or mutability "
                 "specifier",
                 tokc::ENDSTMT>(DISPATCH_ARGS);

      if (is<tokc::BUILTIN_MUTABLE, tokc::BUILTIN_IMMUTABLE>(cursor))
          [[unlikely]] {
        ctx.nodes.push_back(node_t::make(cursor));
        advance(DISPATCH_ARGS);
      }

      // very important comment
      // this is the reason := and : = are the same
      if (is<{tokc::ASIGN, tokc::ENDSTMT}>(cursor))
        return true;

      type(DISPATCH_ARGS);
      return false;
    }();

    constexpr auto &val_fn = dive<medianc::VALUE, expr>;
    if (is<tokc::ASIGN>(cursor)) {
      advance(DISPATCH_ARGS);
      val_fn(DISPATCH_ARGS);
    } else if (must_infer_type) {
      advance(DISPATCH_ARGS);
      if (is<tokc::ENDSTMT>(cursor)) [[unlikely]]
        expected<"Type can't be infered without a value", tokc::ENDSTMT>(
            DISPATCH_ARGS);
      val_fn(DISPATCH_ARGS);
    }

    expect<tokc::ENDSTMT>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

template<tokc::e intro_tok, auto fn>
auto optional DISPATCH_FNSIG {
  if(is<intro_tok>(cursor))
    fn(DISPATCH_ARGS);
}


auto type_decl DISPATCH_FNSIG {
  dive<medianc::TYPE_DECL,
       sequence<push_final, advance<2>,
                optional<tokc::LCBRACE, template_arg_list>, type>>(
      DISPATCH_ARGS);
}

auto scope_decl DISPATCH_FNSIG {
  dive < medianc::SCOPE_DECL, DISPATCH_LAM {
    push_final(DISPATCH_ARGS);
    cursor.advance(2);
    sequence<
        // consume<tokc::ASIGN>,
        parens<base, medianc::BODY>>(ctx, cursor);
    expect<tokc::ENDSTMT>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

auto import_fn DISPATCH_FNSIG {
  become dive<medianc::IMPORT,
              advance2<sequence<expect<tokc::STRLIT>, push_final,
                                expect<tokc::ENDSTMT>>>>(DISPATCH_ARGS);
}

auto unwrap_decl DISPATCH_FNSIG {
  become dive<
      medianc::UNWRAP_DECL,
      sequence<braces<push_final<tokc::ID>, medianc::UNWRAP_IDS>,
               consume<tokc::COLONASIGN>, dive<medianc::UNWRAP_BODY, expr>>>(
      DISPATCH_ARGS);
}

auto fn_decl DISPATCH_FNSIG {
  dive < medianc::FN_DECL, DISPATCH_LAM {
    push_final(DISPATCH_ARGS);
    sequence<advance, fndecl_sig, consume<tokc::ASIGN>,
             dive<medianc::BODY, expr>>(ctx, cursor);

    expect<tokc::ENDSTMT>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

auto template_arg_decl DISPATCH_FNSIG {
  expect<tokc::ID>(DISPATCH_ARGS);
  auto cursor2 = cursor;
  ++cursor2;
  expect<tokc::COLON>(ctx, cursor2);
  constexpr auto table = table_t_make(
      pair_t{tokc::BUILTIN_TYPE,
             dive<medianc::TYPE_DECL,
                  sequence<push_final, advance<2>, expect<tokc::ENDSTMT>>>});
  path<table, var_decl, 2>(DISPATCH_ARGS);
}

auto template_stamp_decl DISPATCH_FNSIG {
  become dive<medianc::TEMPLATE_STAMP_DECL,
              sequence<push_final, advance<2>, template_arg_list, type,
                       expect<tokc::ENDSTMT>>>(DISPATCH_ARGS);
}
auto decl DISPATCH_FNSIG {
  static const auto &a =
      path<table_t_make(pair_t{tokc::BUILTIN_SCOPE, scope_decl},
                        pair_t{tokc::BUILTIN_TYPE, type_decl},
                        // pair_t{tokc::BUILTIN_TEMPLATE, template_stamp_decl},
                        pair_t{tokc::BUILTIN_FN, fn_decl}
                        ),
           var_decl, 2>;
  become a(DISPATCH_ARGS);
}

constexpr auto base_decls = table_t_make(
    pair_t{tokc::ID, path<table_t_make(pair_t{tokc::COLON, decl},
                                       pair_t{tokc::COLONASIGN, var_decl}),expr, 1>},
    pair_t{tokc::LBRACE, unwrap_decl});

constexpr auto base_return = table_t_make(
    pair_t{tokc::BUILTIN_BECOME,
           advance2<medianc::BECOME, sequence<expr, expect<tokc::ENDSTMT>>>},
    pair_t{tokc::BUILTIN_RETURN,
           advance2<medianc::RETURN, sequence<expr, expect<tokc::ENDSTMT>>>});
constexpr auto base_loop = table_t_make(pair_t{tokc::BUILTIN_FOR, for_fn});

auto break_fn DISPATCH_FNSIG {
  dive<medianc::BREAK,
       sequence<consume<tokc::BUILTIN_BREAK>,
                path<table_t_make(pair_t{tokc::ENDSTMT, empty}), expr>,
                expect<tokc::ENDSTMT>>>(DISPATCH_ARGS);
}
constexpr auto base_misc =
    table_t_make(pair_t{tokc::BUILTIN_BREAK, break_fn},
                 pair_t{tokc::BUILTIN_UNREACHABLE, push_final},
                 pair_t{tokc::BUILTIN_IMPORT, import_fn});

constexpr auto base_table =
    table_t_make<base_decls, base_return, base_loop, base_misc>();

template <auto table = base_table> auto base_call DISPATCH_FNSIG {
  become path<table, expr>(DISPATCH_ARGS);
}

auto base DISPATCH_FNSIG {
  become attribute_path<medianc::STMT,
                        base_call<table_t_make<base_decls, base_loop>()>,
                        base_call>(DISPATCH_ARGS);
}

__attribute__((visibility("default"))) auto
entry(const token_buffer_t &toks, const std::map<size_t, size_t> &smap,
      cursor_t cursor, const cursor_t end) -> parser_out {
  auto nodes = podlist_t<node_t>::create(64);
  auto ctx = context_t{toks, smap, nodes};
  symetrical<base, medianc::FILE>(ctx, cursor);
  nodes.shrink_to_fit();
  return nodes;
}

inline auto traverse_impl(const token_buffer_t &toks, auto &cursor, auto end,
                          std::int32_t depth) -> void {
  constexpr const auto depth_mult = 3;

#define space_str std::string(depth *depth_mult, ' ')
#define print std::cout << space_str << depth << "|"
  while (cursor != end) {
    // People say std::visit is slow and it has heap allocations?
    std::visit(overloaded{[&](node_t::final_t &val) {
                            {
                              print << "Final: " << token_code_str(val->type_)
                                    << " \'" << toks.str(val)
                                    << "\'" /*<< " pos: {" << toks.row(val) <<
                                               ", " << toks.col(val)<< "}" */
                                    << '\n';
                            }
                            cursor.advance();
                          },
                          [&](node_t::median_t &val) {
                            {
                              print << "Median: " << medianc::str(val.type_)
                                    << " length: "
                                    << val.len_ /* << " pos: "
               << " {" << toks.row(val.first) << ", "<< toks.col(val.first) <<
               "}..."
               << "{" << toks.row(val.last) << ", "<< toks.col(val.last) << "}"
             */ << "\n";
                            }
                            cursor.advance();
                            traverse_impl(toks, cursor, cursor + val.len_,
                                          depth + 1);
                          },
                          [&](node_t::err_t &val) {
                            {
                              print << "Error, it is joever" << '\n';
                              std::abort();
                            }
                          }},
               cursor->node_);
  }
#undef space_str
#undef print
}

// Needs to be remade for now it only prints

__attribute__((visibility("default"))) auto traverse(const token_buffer_t &buf,
                                                     podlist_t<node_t> &buffer)
    -> void {
  auto cursor = buffer.begin();
  return traverse_impl(buf, cursor, buffer.end(), 0);
}
#undef DISPATCH_LAM
#undef DISPATCH_ARGS_DECL
#undef DISPATCH_ARGS
#undef DISPATCH_FNSIG
} // namespace grammar
