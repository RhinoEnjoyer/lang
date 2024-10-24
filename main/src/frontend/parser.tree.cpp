#include "../token.hpp"
#include "../token_str.hpp"
// #include "../unreachable.hpp"
#include <llvm/Support/Compiler.h>
#include <llvm/Support/raw_ostream.h>
#include <cstdlib>
#include <cstring>

#include <array>
#include <iostream>
#include <iterator>
#include <optional>
#include <source_location>
#include <utility>
#include <variant>

namespace parser {

using cursor_t = vec<token_t>::c_it;

struct node_t {
  using final_t = cursor_t;
  struct err_t {
    std::size_t index;
  };

  struct median_t {
    using code_t = int;

    code_t code;
    podlist_t<node_t> ch;
  };

  std::variant<final_t, median_t, err_t> node;

  [[nodiscard]]
  static auto make(cursor_t cursor) -> node_t {
    return node_t(cursor_t{cursor});
  }
  [[nodiscard]]
  static auto make(median_t::code_t type, podlist_t<node_t>& ch) -> node_t {
    return node_t(median_t{type, std::move(ch)});
  }
  [[nodiscard]]
  static auto make(median_t::code_t type, podlist_t<node_t>&& ch) -> node_t {
    return node_t(median_t{type, std::move(ch)});
  }
  [[nodiscard]]
  static auto err(std::size_t errcode) -> node_t {
    return node_t{err_t{errcode}};
  }

  node_t(node_t&& other) noexcept = default;
  node_t& operator=(node_t&& other) noexcept = default;

 private:
  template <typename... Args>
  explicit node_t(Args&&... args) : node(std::forward<Args>(args)...) {}

  node_t(const node_t&) = delete;
  node_t& operator=(const node_t&) = delete;
};

const auto& dst = std::distance<vec<token_t>::c_it>;
using res_t = std::optional<node_t>;

template <typename... T>
using tup = std::tuple<T...>;

template <const tokc::e... type>
static auto is(cursor_t&& cursor) -> bool {
  return ((cursor->type_ == type) || ...);
}
template <const tokc::e... type>
static auto is(cursor_t& cursor) -> bool {
  return ((cursor->type_ == type) || ...);
}

template <const tokc::e... type>
static auto expect(cursor_t cursor, const cursor_t end) -> void {
  if (is<type...>(cursor, end))
    return;

  llvm::errs() << "Expected: ";
  ((llvm::errs() << token_code_str(type) << " "), ...);
  llvm::errs() << "\b\n\tFound: " << token_code_str(cursor->type_) << '\n';
  std::abort();
}

using fn_t = auto(cursor_t&, const cursor_t) -> node_t;

// This function takes in a variadric list of pairs
//   the pair is a token type that is the start of a pattern
//   and a fucntion pointer to the function that will be dispatched
//
//  It builds a dispatch table in compile time that will be used to jump to
//  the apopriate function

[[clang::preserve_all, clang::noinline]] auto backtrack(cursor_t& cursor,
                                                        const cursor_t end)
    -> cursor_t {
  auto c = cursor;
  while (c->type_ != tokc::e::BEGINSYMETRICAL && c->type_ != tokc::e::ENDSTMT &&
         c->type_ != tokc::e::ENDGROUP)
    --c;
  if (c != cursor)
    ++c;
  return c;
}

template <std::pair<tokc::e, fn_t*>... args>
static auto path(cursor_t& cursor, const cursor_t end) -> node_t {
  // subhuman function
  constexpr auto dispatch_table = [] consteval -> auto {
    if constexpr (!std::is_constant_evaluated())
      static_assert(false, "Call table is not constructed on compile-time");

    constexpr auto arg_arr =
        std::array<std::pair<tokc::e, fn_t*>, sizeof...(args)>{args...};

    std::array<fn_t*, tokc::length()> table = {};
    auto fallback = [](cursor_t& cursor, const cursor_t end) -> node_t {
      constexpr auto m =
          std::array<std::pair<tokc::e, fn_t*>, sizeof...(args)>{args...};

      constexpr auto filter = [&] consteval -> auto {
        auto result = std::array<tokc::e, sizeof...(args)>{};
        std::transform(
            m.begin(), m.end(), result.begin(),
            [&](const auto& pair) constexpr -> auto { return pair.first; });
        return result;
      }();

      llvm::errs() << "Failed to pick a path" << '\n';
      for (auto c = backtrack(cursor, end); c.base() <= cursor.base(); ++c)
        llvm::errs() << token_code_str(c->type_) << ' ';

      llvm::errs() << "\n" << "Expected: \n\t";

      for (const auto& elm : filter)
        llvm::errs() << token_code_str(elm) << "; ";

      llvm::errs() << "\nFound:\n\t" << token_code_str(cursor->type_) << ";\n";
      std::abort();
    };
    for (std::size_t i = 0; i < tokc::length(); i++)
      table[i] = fallback;

    for (const std::pair<tokc::e, fn_t*>& pair : arg_arr)
      table[pair.first] = pair.second;
    return table;
  }();
  // std::cout << "CALL" << std::endl;
  // std::cout << token_code_str(cursor->type_) << std::endl;

  [[clang::musttail]] return dispatch_table[cursor->type_](cursor, end);
}

auto base(cursor_t& cursor, const cursor_t end) -> node_t;

template <const tokc::e tok,
          const tokc::e group = tokc::open2group_symetrical(tok)>
[[nodiscard]] auto symetrical(cursor_t& cursor, const cursor_t end) -> node_t {
  static_assert(tokc::is_open_symetrical(tok),
                "symetrical function must be used with a symetrical token");

  ++cursor;
  if (is<tokc::ENDGROUP>(cursor))
    return node_t::make(group, vec<node_t>::make());

  auto ch = vec<node_t>::create(1);
  do {
    ch.push_back(base(cursor, end));

    // this is here to make debuging easier
    {
      if (!is<tokc::e::ENDSTMT>(cursor)) {
        for (auto c = backtrack(cursor, end); c.base() <= cursor.base(); ++c)
          llvm::errs() << token_code_str(c->type_) << ' ';
        llvm::errs() << "Mistep" << "\n";
        std::abort();
      }
    }
    ++cursor;
  } while (!is<tokc::e::ENDGROUP>(cursor));
  ++cursor;
  // Made one group;
  return node_t::make(group, ch);
}

static auto fn_sig(cursor_t& cursor, const cursor_t end) -> node_t {
  ++cursor;
  auto args = [&] { return symetrical<tokc::LPAREN>(cursor, end); }();
  auto ret_type = [&] { return symetrical<tokc::LPAREN>(cursor, end); }();
  return node_t::make(tokc::BUILTIN_FN, vec<node_t>::make(args, ret_type));
}

static auto type(cursor_t& cursor, const cursor_t end) -> node_t {
  // std::cout << "Type" << std::endl;
  return node_t::make(
      tokc::BUILTIN_TYPE,
      vec<node_t>::make(
          path<{tokc::e::BUILTIN_FN, fn_sig},
               {tokc::e::ID,
                [](cursor_t& cursor, const cursor_t end) -> node_t {
                  ++cursor;
                  return node_t::make(cursor - 1);
                }}>(cursor, end)));
}

static auto expr(cursor_t& cursor, const cursor_t end) -> node_t {
  if (is<tokc::e::ENDSTMT>(cursor)) {
    llvm::outs() << "Expresion can't be empty" << '\n';
    std::abort();
  }

  do {
    ++cursor;
  } while ((LLVM_LIKELY(!is<tokc::e::ENDSTMT>(cursor))));
  return node_t::make(cursor - 1);
}
namespace stmt {

auto decl(cursor_t& cursor, const cursor_t end) -> node_t {
  auto decl_name = std::optional(node_t::make(cursor));
  ++cursor;

  auto [decl_mut, decl_type] = [&] -> tup<res_t, res_t> {
    if (LLVM_UNLIKELY(!is<tokc::e::COLON>(cursor)))
      return std::tuple{std::nullopt, std::nullopt};
    ++cursor;

    auto m = [&] -> res_t {
      if (!is<tokc::e::BUILTIN_MUTABLE, tokc::e::BUILTIN_IMMUTABLE>(cursor))
        return std::nullopt;

      ++cursor;
      return node_t::make(cursor);
    }();

    auto t = [&] -> res_t {
      if (is<tokc::e::ASIGN, tokc::e::ENDSTMT>(cursor))
        return std::nullopt;
      return type(cursor, end);
    }();

    if (!m && !t) {
      llvm::errs() << "This path needs to be false" << '\n';
      std::abort();
    }
    return {std::move(m), std::move(t)};
  }();

  auto decl_value = [&] -> res_t {
    if (is<tokc::e::ASIGN>(cursor)) {
      /* We should have a value we do not care if we have a type */
      ++cursor;
      return expr(cursor, end);
    } else if (!decl_type) {
      /* We do not have a type and we do not have a value */
      llvm::outs() << std::source_location::current().line() << '\n';
      std::abort();
    } else if (is<tokc::e::ENDSTMT>(cursor)) {
      /* We have a type but not a value */
      return std::nullopt;
    } else {
      llvm::errs() << "IDK" << '\n';
      std::abort();
    }
  }();
  auto n = node_t::make(10, vec<node_t>::make_from_opt(decl_name, decl_mut,
                                                       decl_type, decl_value));

  return n;
}

}  // namespace stmt
auto id(cursor_t& cursor, const cursor_t end) -> node_t {
  if (is<tokc::e::COLON, tokc::e::COLONASIGN>(cursor + 1))
    return stmt::decl(cursor, end);
  return expr(cursor, end);
}

auto base(cursor_t& cursor, const cursor_t end) -> node_t {
  return path<{tokc::e::LPAREN, symetrical<tokc::e::LPAREN>},
              {tokc::e::ID, id}>(cursor, end);
}

template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

static constexpr auto DEPTH_MULT = 3;
auto traverse(node_t& node, const int depth = 0) -> void {
  std::visit(overloaded{[=](node_t::final_t& val) {
                          std::cout << std::string(depth * DEPTH_MULT, ' ')
                                    << "Final: " << token_code_str(val->type_)
                                    << std::endl;
                        },
                        [=](node_t::median_t& val) {
                          std::cout << std::string(depth * DEPTH_MULT, ' ')
                                    << "Median, length: " << val.ch.size()
                                    << std::endl;
                          for (auto& elm : val.ch) {
                            traverse(elm, depth + 1);
                          }
                        },
                        [=](node_t::err_t& val) {
                          std::cout << std::string(depth * DEPTH_MULT, ' ')
                                    << "Found an Error" << std::endl;
                        }},
             node.node);
}

[[nodiscard]]
auto entry(cursor_t begin, cursor_t end) -> node_t {
  auto cursor = begin + 1;
  return base(cursor, end);
}
}  // namespace parser
