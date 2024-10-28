#include "../become.hpp"
#include "../token.hpp"
#include "../token_str.hpp"
#include <cstdlib>
#include <cstring>

#include <array>
#include <iostream>
#include <utility>
#include <variant>


/*
  So far so good 
  using variadric arguments might have been a good idea at first 
  but it turned out kindof limiting

  It should be best to replace all the varidaric templates with 
  a more concrete type like a array of something, it will require some tweeking
  but it should improve clarity, reduce copy pasting and keeping track of multiple path configurations
  π.χ {tokc::e::BUILTIN_VECTOR,vector}....(on another function) {tokc::e::BUILTIN_VECTOR,advance2<...>(...)}
  and will set a foundation for a more consistent methodology
*/


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

// template <const auto type>
// [[nodiscard]] auto is(cursor_t cursor){
//   for(const auto& elm: type){
//     if(elm == cursor->type_)
//       return true;
//   }
//   return false;
// }

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

template <std::size_t match_cursor_offset, auto fallback_, std::pair<tokc::e, fn_t *>... args>
 auto path DISPATCH_FNSIG {
  // subhuman function
  constexpr auto dispatch_table = [] consteval -> auto {
    if constexpr (!std::is_constant_evaluated())
      static_assert(false, "Call table is not constructed on compile-time");

    constexpr auto arg_arr =
        std::array<std::pair<tokc::e, fn_t *>, sizeof...(args)>{args...};

    std::array<fn_t *, tokc::length()> table = {};
    auto fallback = [] consteval -> auto {
      if constexpr (fallback_ != nullptr) {
        return fallback_;
      } else {
        return DISPATCH_LAM {
          constexpr auto m =
              std::array<std::pair<tokc::e, fn_t *>, sizeof...(args)>{args...};

          auto filter = [&] consteval -> auto {
            auto result = std::array<tokc::e, sizeof...(args)>{};
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

    for (const std::pair<tokc::e, fn_t *> &pair : arg_arr)
      table[pair.first] = pair.second;
    return table;
  }();

  BECOME dispatch_table[(cursor + match_cursor_offset)->type_](toks, buffer,cursor);
}

template <auto fallback_, pair_t... args>
auto path DISPATCH_FNSIG {
  BECOME path<0, fallback_, args...>(DISPATCH_ARGS);
}

template <std::size_t match_cursor_offset, std::pair<tokc::e, fn_t *>... args>
auto path DISPATCH_FNSIG {
  BECOME path<match_cursor_offset, nullptr, args...>(DISPATCH_ARGS);
}
template <std::pair<tokc::e, fn_t *>... args> auto path DISPATCH_FNSIG {
  BECOME path<0, nullptr, args...>(DISPATCH_ARGS);
}

auto base DISPATCH_FNSIG;

template <fn_t* fn, tokc::e type,
          tokc::e group = tokc::symetrical_group(type)>
auto symetrical DISPATCH_FNSIG {
  static_assert(tokc::is_open_symetrical(type), "type template argument is not a symetrical");

  cursor.advance();

  if (is<tokc::ENDGROUP>(cursor)) [[unlikely]] {
    cursor.advance();
    return buffer.push_back(node_t::make(group, 0));
  }

  dive < 10, DISPATCH_LAM {
    do {
      //no fallback
      fn(DISPATCH_ARGS);
      expect<tokc::e::ENDSTMT>(DISPATCH_ARGS);
      cursor.advance();
    } while (!is<tokc::e::ENDGROUP>(cursor));
    cursor.advance();
  }
  > (DISPATCH_ARGS);
};

template <tokc::e type, tokc::e group = tokc::symetrical_group(type)>
auto symetrical DISPATCH_FNSIG {
  BECOME symetrical<base, type, group>(DISPATCH_ARGS);
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
    symetrical<expr, tokc::LBRACE>(DISPATCH_ARGS);

  if(is<tokc::e::LCBRACE>(cursor)) [[unlikely]]
    symetrical<decl, tokc::LCBRACE>(DISPATCH_ARGS);

  expect<tokc::LPAREN>(DISPATCH_ARGS);
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
      path <DISPATCH_LAM {std::cerr << "Invalid compound element" << '\n';std::abort();},
      {tokc::ID, DISPATCH_LAM{buffer.push_back(node_t::make(cursor));cursor.advance();}},
      {tokc::e::LPAREN,  symetrical<tokc::LPAREN>},
      {tokc::e::LBRACE,  symetrical<expr,tokc::LBRACE>},
      {tokc::e::LCBRACE, symetrical<type<expr>,tokc::LCBRACE>}
      > (DISPATCH_ARGS);

    if (!is<tokc::e::DCOLON>(cursor)) [[unlikely]] 
      break;
    cursor.advance();
  }while (true);
}
>;

auto push_final(DISPATCH_ARGS_DECL) {
  buffer.push_back(node_t::make(cursor));
  cursor.advance();
}


constexpr auto combine_tables = [](auto a, auto b) constexpr -> auto {
  auto table = std::array<std::pair<tokc::e,fn_t*>, a.size() + b.size()>{};
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



struct table_entry_t{
  tokc::e type;
  fn_t* fn;
  const char* fn_name;

  consteval static auto make(const tokc::e type, fn_t *fn,
                             const char *fn_str) -> table_entry_t {
    return {type, fn, fn_str};
  };
};

#define table_entry_make(TYPE,FN)\
  table_entry_t::make(TYPE,FN, #FN)





constexpr auto aggregate_table = std::array{
  pair_t{tokc::e::BUILTIN_SCOPE, dive<30,push_final>}, 
  pair_t{tokc::e::BUILTIN_FN, dive<30,fn_sig>}, 

  pair_t{tokc::e::BUILTIN_REC, dive<30,advance2symetrical<665,decl>>},
  pair_t{tokc::e::BUILTIN_UNION, dive<30,advance2symetrical<666,decl>>},
  pair_t{tokc::e::BUILTIN_ENUM, dive<30,advance2symetrical<666,expr>>},
  pair_t{tokc::e::BUILTIN_VECTOR, dive<30,advance2symetrical<669,expr>>},
  pair_t{tokc::e::BUILTIN_TYPEOF, dive<30,advance2symetrical<667,base>>}
};
constexpr auto type_table = combine_tables(aggregate_table, std::array{pair_t{tokc::e::ID, dive<99099, compound>}});
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

  auto impl = []<pair_t... args>(DISPATCH_ARGS_DECL) static -> void{
    std::size_t memento = buffer.size();
   
    if constexpr (dive_code == 99099) {
      path<dive<99099,fallback>, args...>(DISPATCH_ARGS);
    }else {
      path<nullptr,args...>(DISPATCH_ARGS);
    }
    
    if constexpr (is_compound >= 0){
      if constexpr (is_compound == 1)
        expect<tokc::e::DCOLON>(DISPATCH_ARGS);
      
      if(is<tokc::e::DCOLON>(cursor)){
        cursor.advance();
        buffer.at(memento).as_median().type_ = 2100219;

        std::size_t memento2 = buffer.size();
        dive<99099099, compound>(DISPATCH_ARGS);

        buffer.at(memento).as_median().len_ += buffer.at(memento2).as_median().len_ + 1;
      }
    }
  };

  /* This ain't the best way to do it */
  impl.template operator()<
       {tokc::e::ID, dive<99099,compound>},
       {tokc::e::BUILTIN_FN, dive<30,fn_sig>}, 
       {tokc::e::BUILTIN_REC, dive<30,advance2symetrical<665,decl>>},
       {tokc::e::BUILTIN_UNION, dive<30,advance2symetrical<666,decl>>},
       {tokc::e::BUILTIN_ENUM, placeholder},
       {tokc::e::BUILTIN_SCOPE, dive<30,push_final>}, 
       {tokc::e::BUILTIN_VECTOR, dive<30,advance2symetrical<669,expr>>},
       {tokc::e::BUILTIN_TYPEOF, dive<30,advance2symetrical<667,base>>}    
    >(DISPATCH_ARGS);
}

/*
  Need to do precedence
  How?
    No idea
*/
auto expr DISPATCH_FNSIG {

  #define TOKEN_SYMBOL_SEQUENCE(SPELLING,CODE)\
    {tokc::e::CODE,push_final},

  if (is<tokc::e::ENDSTMT>(cursor)) [[unlikely]]{
    std::cerr << "Can't have empty expresions" << '\n';
    std::abort();
  }
  dive < 40, DISPATCH_LAM {
    do {
      path<{tokc::e::ID, compound},

           #include "../token.def"
           {tokc::e::INT, push_final},
           {tokc::e::FLOAT, push_final},

           {tokc::e::LPAREN, symetrical<tokc::e::LPAREN>},
           {tokc::e::BUILTIN_DUCKLING, compound},
           {tokc::e::BUILTIN_AS, as},

           {tokc::e::BUILTIN_UNION, compound_literal},
           {tokc::e::BUILTIN_REC, compound_literal},
           {tokc::e::BUILTIN_VECTOR, compound_literal},
           {tokc::e::BUILTIN_TYPEOF, compound_literal},
           {tokc::e::BUILTIN_FN, compound_literal}
           >(DISPATCH_ARGS);
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
      if (is<tokc::e::COLONASIGN>(cursor)) [[unlikely]] {
        return true;
      }
      cursor.advance();

      if (is<tokc::e::ENDSTMT>(cursor)) [[unlikely]] {
        std::cerr
            << "Can't have a declaration without a type and without a value"
            << '\n';
        std::abort();
      }

      if (is<tokc::e::BUILTIN_MUTABLE, tokc::e::BUILTIN_IMMUTABLE>(cursor))
          [[unlikely]] {
        buffer.push_back(node_t::make(cursor));
        cursor.advance();
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
  BECOME path<2,var_decl, {tokc::e::BUILTIN_ALIAS, alias_decl}>(DISPATCH_ARGS);
}
const auto &id =
    path<1, expr, {tokc::e::COLON, decl}, {tokc::e::COLONASIGN, var_decl}>;

auto base DISPATCH_FNSIG {
  return path<expr,
              {tokc::e::ID, id},
              {tokc::e::LPAREN, symetrical<base,tokc::e::LPAREN>},
              {tokc::e::BUILTIN_RETURN, DISPATCH_LAM{cursor.advance();dive<99,expr>(DISPATCH_ARGS);}}
              >(DISPATCH_ARGS);
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
}

auto traverse(token_buffer_t &buf, vec<node_t> &buffer, token_buffer_t &toks) {
  auto cursor = buffer.begin();
  return traverse_impl(buf, cursor, buffer.end(), 0);
}
#undef space_str
#undef print
#undef DISPATCH_LAM
#undef DISPATCH_ARGS_DECL
#undef DISPATCH_ARGS
#undef DISPATCH_FNSIG
} // namespace parser
