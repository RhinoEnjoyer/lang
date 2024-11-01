#include "../become.hpp"
#include "../token.hpp"
#include "../token_str.hpp"
#include <cstdlib>
#include <cstring>

#include <array>
#include <iostream>
#include <utility>
#include <variant>


namespace parser {

using cursor_t = vec<token_t>::c_it;

struct node_t {
  using final_t = cursor_t;

  struct median_t {
    using code_t = std::int32_t;

    code_t type_;
    std::int32_t len_;
  };
  struct err_t {
    std::int32_t index_;
  };

  using node_var_t = std::variant<final_t, median_t, err_t>;
  node_var_t node;

  [[nodiscard]] auto as_median() -> median_t & {
    return std::get<median_t>(node);
  }
  [[nodiscard]] auto as_final() -> final_t & { return std::get<final_t>(node); }
  [[nodiscard]] auto as_err() -> err_t & { return std::get<err_t>(node); }

  [[nodiscard]] auto as_median() const -> const median_t & {
    return std::get<median_t>(node);
  }
  [[nodiscard]] auto as_final() const -> const final_t & {
    return std::get<final_t>(node);
  }
  [[nodiscard]] auto as_err() const -> const err_t & {
    return std::get<err_t>(node);
  }

  [[nodiscard]] auto is_median() const -> bool {
    return std::holds_alternative<median_t>(node);
  }
  [[nodiscard]] auto is_final() const -> bool {
    return std::holds_alternative<final_t>(node);
  }
  [[nodiscard]] auto is_err() const -> bool {
    return std::holds_alternative<err_t>(node);
  }

  [[nodiscard]] static auto make(cursor_t cursor) -> node_t {
    return node_t{final_t(cursor)};
  }
  [[nodiscard]] static auto make(median_t::code_t type,
                                 std::int32_t len) -> node_t {
    return node_t{median_t{type, len}};
  }
};

#define DISPATCH_ARGS_DECL                                                     \
  const token_buffer_t& toks, vec<node_t> &buffer, cursor_t &cursor/*, const cursor_t end*/
#define DISPATCH_ARGS toks,buffer, cursor /*, end*/
#define DISPATCH_FNSIG (DISPATCH_ARGS_DECL)->void
#define DISPATCH_LAM [] DISPATCH_FNSIG

using fn_t = auto DISPATCH_FNSIG;
using pair_t = std::pair<tokc::e,fn_t*>;

template <std::size_t N>
using set_t = std::array<pair_t, N>;

template <pair_t... args> consteval auto make_set() -> auto {
  return set_t<sizeof...(args)>{args...};
}

auto push_final(DISPATCH_ARGS_DECL) {
  buffer.push_back(node_t::make(cursor));
  cursor.advance();
}

template <node_t::median_t::code_t type, auto FN>
auto dive(DISPATCH_ARGS_DECL) -> void {
  buffer.push_back(node_t::make(type, 0));
  const auto parent_index = buffer.length() - 1;
  const auto index = buffer.length();

  FN(DISPATCH_ARGS);

  const auto length = buffer.length() - index;
  buffer.at(parent_index).as_median().len_ = length;
}

template <typename... T> using tup = std::tuple<T...>;

template <const tokc::e... type>
[[nodiscard]] inline auto is(auto cursor) -> bool {
  return ((cursor->type_ == type) || ...);
}

template <const tokc::e... type> auto expected(DISPATCH_ARGS_DECL) -> void {
  std::cerr << "Str: \'" << toks.str(cursor) << "\'"
            << "\n\tType: " << toks.str(cursor)
            << "\n\tRow: " << toks.row(cursor) << "\tCol: " << toks.col(cursor)
            << "\tLen: " << toks.len(cursor) << "\n";

  std::cerr << "Expected: ";
  ((std::cerr << token_code_str(type) << " "), ...);
  std::cerr << "\b\n\tFound: " << token_code_str(cursor->type_) << '\n';
  std::abort();
}

template <const tokc::e... type> auto expect(DISPATCH_ARGS_DECL) -> void {
  if (!is<type...>(cursor)) [[unlikely]]
    BECOME expected<type...>(DISPATCH_ARGS);
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
          constexpr auto m = args;
          auto filter = [&] consteval -> auto {
            auto result = std::array<tokc::e, args.size()>{};
            std::transform(
                m.begin(), m.end(), result.begin(),
                [&](const auto &pair) constexpr -> auto { return pair.first; });
            return result;
          }();

          std::cerr << "Str: \'" << toks.str(cursor) << "\'"
                       << "\n\tType: " << token_code_str(toks.type(cursor))
                       << "\n\tRow: " << toks.row(cursor)
                       << "\tCol: " << toks.col(cursor)
                       << "\tLen: " << toks.len(cursor) << "\n";
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

  BECOME dispatch_table[(cursor + match_cursor_offset)->type_](toks, buffer,cursor);
}
// template <auto args, std::size_t match_cursor_offset = 0>
//  auto path DISPATCH_FNSIG {
//    BECOME path<nullptr,args,match_cursor_offset>(DISPATCH_ARGS);
//  }

auto base DISPATCH_FNSIG;

template <fn_t *fn> auto symetrical_impl DISPATCH_FNSIG {
  dive < 10, DISPATCH_LAM {
    do {
      // no fallback
      fn(DISPATCH_ARGS);
      expect<tokc::e::ENDSTMT>(DISPATCH_ARGS);
      cursor.advance();
    } while (!is<tokc::e::ENDGROUP>(cursor));
    cursor.advance();
  }
  > (DISPATCH_ARGS);
}

template <fn_t *fn> auto symetrical DISPATCH_FNSIG {
  cursor.advance();
  if (is<tokc::ENDGROUP>(cursor)) [[unlikely]] {
    cursor.advance();
    return buffer.push_back(node_t::make(10, 0));
  }
  symetrical_impl<fn>(DISPATCH_ARGS);
};

// template <fn_t *fn, tokc::e type, tokc::e group>
// auto symetrical DISPATCH_FNSIG {
//   cursor.advance();
//   if (is<tokc::ENDGROUP>(cursor)) [[unlikely]] {
//     cursor.advance();
//     return buffer.push_back(node_t::make(group, 0));
//   }
//   BECOME symetrical_impl<fn>(DISPATCH_ARGS);
// };

template <fn_t* fn, tokc::e type>
auto symetrical DISPATCH_FNSIG {
  static_assert(tokc::is_open_symetrical(type), "type template argument is not a symetrical");
  expect<type>(DISPATCH_ARGS);
  cursor.advance();
    if (is<tokc::ENDGROUP>(cursor)) [[unlikely]] {
      cursor.advance();
      return buffer.push_back(node_t::make(10, 0));
    }

  BECOME symetrical_impl<fn>(DISPATCH_ARGS);
}

template<auto fn> auto advance2 DISPATCH_FNSIG{
  cursor.advance();
  fn(DISPATCH_ARGS);
}

template<int dive_code,auto fn> auto advance2 DISPATCH_FNSIG{
  BECOME dive<dive_code, advance2<fn>>(DISPATCH_ARGS);
}

template <int dive_code, fn_t *fn = base, tokc::e open_code = tokc::e::LPAREN>
auto advance2symetrical DISPATCH_FNSIG {
  BECOME advance2<dive_code, symetrical<fn, open_code>>(DISPATCH_ARGS);
}

auto decl DISPATCH_FNSIG;
auto expr DISPATCH_FNSIG;

template<fn_t* fn = nullptr, int compound_state = -1>
auto type DISPATCH_FNSIG;
const auto& compound_literal = type<nullptr, 1>;
const auto& comptime_arg = type<expr, 0>;


auto empty DISPATCH_FNSIG {}
static auto placeholder = DISPATCH_LAM {
  std::cerr << "TODO function" << '\n';
  std::abort();
};

auto& as = advance2symetrical<98,type>;

const auto &fn_sig = dive<20, DISPATCH_LAM {
  cursor.advance();

  if(is<tokc::e::LBRACE>(cursor)) [[unlikely]]
    symetrical<expr>(DISPATCH_ARGS);

  if(is<tokc::e::LCBRACE>(cursor)) [[unlikely]]
    symetrical<decl>(DISPATCH_ARGS);

  // expect<tokc::LPAREN>(DISPATCH_ARGS);
  symetrical<decl, tokc::LPAREN>(DISPATCH_ARGS);

  expect<tokc::LPAREN>(DISPATCH_ARGS);
  symetrical<type, tokc::LPAREN>(DISPATCH_ARGS);
}>;


//@(expr;expr;...)
const auto &duck = advance2<dive<9909923, symetrical<expr, tokc::e::LPAREN>>>;

const auto& compound =  dive < 760, DISPATCH_LAM {
    if(is<tokc::e::BUILTIN_DUCKLING>(cursor)){
      duck(DISPATCH_ARGS);
      if(!is<tokc::e::DCOLON>(cursor))
        return;
      cursor.advance();
    }

    do {
      path < make_set<pair_t{tokc::ID, push_final}>(),DISPATCH_LAM {std::cerr << "Invalid compound element" << '\n';std::abort();} >
            (DISPATCH_ARGS);
                 // pair_t{tokc::e::LPAREN, symetrical<base>},
                 // pair_t{tokc::e::LBRACE, symetrical<expr>},
                 // pair_t{tokc::e::LCBRACE, symetrical<type<expr>>}

      // if (!is<tokc::e::DCOLON>(cursor)) [[unlikely]]{
      //   break;
      // }
      // cursor.advance();
    AGAIN:
      if (is<tokc::e::DCOLON>(cursor)) [[unlikely]] {
        cursor.advance();
      } else {
        if (is<tokc::e::LPAREN, tokc::e::LBRACE, tokc::e::LCBRACE>(cursor)) {
          path<make_set<pair_t{tokc::e::LPAREN, symetrical<base>},
                        pair_t{tokc::e::LBRACE, symetrical<expr>},
                        pair_t{tokc::e::LCBRACE, symetrical<type<expr>>}>()>(
              DISPATCH_ARGS);
          goto AGAIN;
          // if (!is<tokc::e::DCOLON>(cursor)) [[unlikely]]
          //   break;
          // cursor.advance();
        } else {
          break;
        }
      }
    } while (true);
}
>;


constexpr auto merge_tables = [](auto a, auto b) constexpr -> auto {
  auto table = std::array<pair_t, a.size() + b.size()>{};
  std::uint64_t index = 0;
  for (std::uint64_t i = 0; i < a.size(); i++) {
    table[index] = a[i];
    index++;
  }
  for (std::uint64_t i = 0; i < b.size(); i++) {
    table[index] = b[i];
    index++;
  }
  return table;

};

constexpr auto aggregate_table = make_set<
  pair_t{tokc::e::BUILTIN_SCOPE, dive<30,push_final>}, 
  pair_t{tokc::e::BUILTIN_FN, dive<30,fn_sig>}, 
  pair_t{tokc::e::BUILTIN_REC, dive<30,advance2symetrical<665,decl>>},
  pair_t{tokc::e::BUILTIN_UNION, dive<30,advance2symetrical<666,decl>>},
  pair_t{tokc::e::BUILTIN_ENUM, dive<30,advance2symetrical<666,expr>>},
  pair_t{tokc::e::BUILTIN_VECTOR, dive<30,advance2symetrical<669,type>>},
  pair_t{tokc::e::BUILTIN_TYPEOF, dive<30,advance2symetrical<667,expr>>}
>();

constexpr auto type_table = merge_tables(aggregate_table, make_set<pair_t{tokc::e::ID, dive<99099, compound>}>());

constexpr auto type_codes = [] consteval -> auto {
  auto table = std::array<tokc::e, type_table.size()>{tokc::e::ERROR};
  std::transform(type_table.begin(), type_table.end(), table.begin(),
                 [](const auto &pair) { return pair.first; });
  return table;
}();
constexpr auto type_fn = [] consteval -> auto {
  auto table = std::array<fn_t*, type_table.size()>{nullptr};
  std::transform(type_table.begin(), type_table.end(), table.begin(),
                 [](const auto &pair) { return pair.second; });
  return table;
}();

auto get_index(vec<node_t>& buffer) -> std::uint64_t{
  return buffer.size();
}


template<fn_t* fn>
auto extend(std::uint64_t index,DISPATCH_ARGS_DECL){
  fn(DISPATCH_ARGS);
  auto length = buffer.length() - index;
  buffer.at(index).as_median().len_ = length;
};


//compound_state 
// = -1 only type
// = 0  type and compound
// = 1 only compound

template <fn_t *const fallback, const int is_compound> auto type DISPATCH_FNSIG {
  // constexpr int compound_state = 1;
  constexpr auto dive_code = [] consteval -> int{
    if constexpr (fallback != nullptr)
      return 99099;
    return 30;
  }();

  auto impl = []<auto args>(DISPATCH_ARGS_DECL) static -> void{
    std::size_t memento = buffer.size();
   
    if constexpr (dive_code == 99099) {
      path<args,dive<99099,fallback>>(DISPATCH_ARGS);
    }else {
      path<args,nullptr>(DISPATCH_ARGS);
    }

    const auto comp_lit = [&] {
      cursor.advance();
      buffer.at(memento).as_median().type_ = 2100219;

      std::size_t memento2 = buffer.size();
      dive<99099099, compound>(DISPATCH_ARGS);

      buffer.at(memento).as_median().len_ +=
          buffer.at(memento2).as_median().len_ + 1;
    };

    if constexpr (is_compound >= 0){
      if constexpr (is_compound == 1)
        expect<tokc::e::DCOLON>(DISPATCH_ARGS);
      if constexpr (is_compound == 0)
        if (!is<tokc::e::DCOLON>(cursor))
          return;
      comp_lit();
    }
  };

  impl.template operator()<type_table>(DISPATCH_ARGS);
}

template<fn_t* fn, fn_t*... fns>
auto sequence DISPATCH_FNSIG{
  fn(DISPATCH_ARGS);

  if constexpr (sizeof...(fns) > 0)
    BECOME sequence<fns...>(DISPATCH_ARGS);
  else
    return;
}


namespace if_stmt{
static constexpr auto& if_ctrl_stmt = dive<20202,sequence<symetrical<expr>, symetrical<base,tokc::e::LCBRACE>>>;
auto if_else_path DISPATCH_FNSIG{
  static constexpr auto elif_pair =
      pair_t{tokc::e::LPAREN,sequence<if_ctrl_stmt,if_else_path>};

  static constexpr auto el_pair = pair_t{tokc::e::LCBRACE, symetrical<base>};
  static constexpr auto set = make_set<elif_pair, el_pair>();
  BECOME path<set,DISPATCH_LAM{}>(DISPATCH_ARGS);
}

auto fn DISPATCH_FNSIG {
  BECOME dive<5000,DISPATCH_LAM{
  cursor.advance();
  if_ctrl_stmt(DISPATCH_ARGS);

  if(!is<tokc::e::LPAREN, tokc::e::LCBRACE>(cursor))
    return;

  if_else_path(DISPATCH_ARGS);
  }>(DISPATCH_ARGS);
}
} // namespace if_stmt

#define TOKEN_SYMBOL_SEQUENCE(SPELLING, CODE) pair_t{tokc::e::CODE, push_final},
constexpr auto expr_table = make_set<
    #include "../token.def"
    pair_t{tokc::e::ID, compound}, 
    pair_t{tokc::e::INT, push_final},
    pair_t{tokc::e::FLOAT, push_final},
    pair_t{tokc::e::LPAREN, symetrical<base>},
    pair_t{tokc::e::BUILTIN_IF, if_stmt::fn},
    pair_t{tokc::e::BUILTIN_PIPE, push_final},
    pair_t{tokc::e::BUILTIN_SET, advance2symetrical<12941, expr>},
    pair_t{tokc::e::BUILTIN_SIZEOF, advance2symetrical<129491, type>},
    pair_t{tokc::e::BUILTIN_DUCKLING, compound},
    pair_t{tokc::e::BUILTIN_AS, as},
    pair_t{tokc::e::BUILTIN_UNION, compound_literal},
    pair_t{tokc::e::BUILTIN_REC, compound_literal},
    pair_t{tokc::e::BUILTIN_VECTOR, compound_literal},
    pair_t{tokc::e::BUILTIN_FN, compound_literal},
    pair_t{tokc::e::BUILTIN_TYPEOF, compound_literal}>();

auto expr DISPATCH_FNSIG {


  if (is<tokc::e::ENDSTMT>(cursor)) [[unlikely]]{
    std::cerr << "Can't have empty expresions" << '\n';
    std::abort();
  }
  dive < 40, DISPATCH_LAM {
    do {
      path<expr_table>(DISPATCH_ARGS);
    } while (LLVM_LIKELY(!is<tokc::e::ENDSTMT>(cursor)));
  }
  > (DISPATCH_ARGS);
}

auto alias_decl DISPATCH_FNSIG {
  dive < 214241, DISPATCH_LAM {
    push_final(DISPATCH_ARGS);
    cursor.advance(2);

    expect<tokc::e::ASIGN>(DISPATCH_ARGS);
    cursor.advance();

    type(DISPATCH_ARGS);
    expect<tokc::e::ENDSTMT>(DISPATCH_ARGS);
  }> (DISPATCH_ARGS);
}

auto var_decl DISPATCH_FNSIG {
  dive < 50, DISPATCH_LAM {
    push_final(DISPATCH_ARGS);

    const auto must_infer_type = [&] -> bool {
      if (is<tokc::e::COLONASIGN>(cursor)) [[unlikely]]
        return true;
      cursor.advance();

      if (is<tokc::e::ENDSTMT>(cursor)) [[unlikely]] 
        expected<tokc::ENDSTMT>(DISPATCH_ARGS);
      if (is<tokc::e::BUILTIN_MUTABLE, tokc::e::BUILTIN_IMMUTABLE>(cursor))
          [[unlikely]] {
        buffer.push_back(node_t::make(cursor));
        cursor.advance();
      }

      //Attributes
      if(is<tokc::e::BUILTIN_DUCKLING>(cursor)){
        advance2symetrical<10>(DISPATCH_ARGS);
      }
        

      if (is<tokc::e::ASIGN, tokc::e::ENDSTMT>(cursor)) [[unlikely]] {
        return true;
      }

      type(DISPATCH_ARGS);

      return false;
    }();

    if (is<tokc::e::ASIGN>(cursor)) {
      cursor.advance();
      expr(DISPATCH_ARGS);
    } else if (must_infer_type) {
      cursor.advance();
      if (is<tokc::e::ENDSTMT>(cursor)) {
        std::cerr << "Type can't be infered without a value" << '\n';
        std::abort();
      }
      expr(DISPATCH_ARGS);
    }

    expect<tokc::e::ENDSTMT>(DISPATCH_ARGS);
  }
  > (DISPATCH_ARGS);
}

auto decl DISPATCH_FNSIG {
  BECOME path<make_set<pair_t{tokc::e::BUILTIN_ALIAS, alias_decl}>(),var_decl,2>(DISPATCH_ARGS);
}

auto id DISPATCH_FNSIG {
  BECOME path<make_set<pair_t{tokc::e::COLON, decl},
                       pair_t{tokc::e::COLONASIGN, var_decl}>(),
              expr, 1>(DISPATCH_ARGS);
}

auto base DISPATCH_FNSIG {
  BECOME path<make_set<{tokc::e::ID, id}, {tokc::e::LPAREN, symetrical<base>},
                       {tokc::e::BUILTIN_RETURN, advance2<dive<99, expr>>}>(),
              expr>(DISPATCH_ARGS);
}

auto entry(const token_buffer_t& toks,cursor_t cursor, const cursor_t end) -> podlist_t<node_t> {
  auto buffer = podlist_t<node_t>::create(64);
  cursor.advance();
  base(DISPATCH_ARGS);
  return buffer;
}

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

auto traverse_impl(token_buffer_t &buf, vec<node_t>::it &cursor,
                   vec<node_t>::it end, int depth) -> void {
  constexpr const auto depth_mult = 3;

#define space_str std::string(depth *depth_mult, ' ')
#define print std::cout << space_str << depth << "|"

  while (cursor != end) {
    // People say std::visit is slow
    std::visit(
        overloaded{[&](node_t::final_t &val) {
                     {
                       print << "Final: " << token_code_str(val->type_) << " \'"
                             << buf.str(val) << "\'" << '\n';
                     }
                     cursor.advance();
                   },
                   [&](node_t::median_t &val) {
                     {
                       print << "Median: " << val.type_
                             << " length: " << val.len_ << '\n';
                     }
                     cursor.advance();
                     traverse_impl(buf, cursor, cursor + val.len_, depth + 1);
                   },
                   [&](node_t::err_t &val) {
                     {
                       print << "Error, it is joever" << '\n';
                       std::abort();
                     }
                   }},
        cursor->node);
  }
#undef space_str
#undef print
}

auto traverse(token_buffer_t &buf, vec<node_t> &buffer, token_buffer_t &toks) {
  auto cursor = buffer.begin();
  return traverse_impl(buf, cursor, buffer.end(), 0);
}
#undef DISPATCH_LAM
#undef DISPATCH_ARGS_DECL
#undef DISPATCH_ARGS
#undef DISPATCH_FNSIG
} // namespace parser
