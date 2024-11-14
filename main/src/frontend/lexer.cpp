#include "../become.hpp"
#include "./lexer.hpp"
#include <array>

namespace lexer {

#define DISPATCH_RETURN int32_t
#define DISPATCH_ARGS_DECL                                                     \
  lexer_t &lexer, const llvm::StringRef src, const pos_t bpos, pos_t pos
#define DISPATCH_ARGS lexer, src, bpos, pos

#define asign2table(table, index, val) table[index] = val
constexpr auto one_char_tok_lut = [] constexpr {
  std::array<tokc::e, 256> table = {tokc::e::ERROR};
#define TOKEN_ONE_CHAR(spelling, code)                                         \
  asign2table(table, spelling[0], tokc::e::code);
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  asign2table(table, spelling[0], tokc::e::code);
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  asign2table(table, spelling[0], tokc::e::code);
#include "../token.def"
  return table;
}();

using dispatch_fn_t = auto(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN;
using dispatch_table_t = std::array<dispatch_fn_t *, 256>;

constexpr auto symbol_sequence_bytes = [] constexpr {
  std::array<bool, 256> table = {false};
#define TOKEN_SYMBOL_SEQUENCE(spelling, code)                                  \
  for (std::uint32_t i = 0; i < sizeof(spelling); ++i)                         \
    asign2table(table, spelling[i], true);
#include "../token.def"

  return table;
}();

constexpr auto id_start_byte_lut = [] constexpr {
  std::array<bool, 256> table = {false};
  for (char c = 'A'; c <= 'Z'; ++c)
    table[c] = true;
  for (char c = 'a'; c <= 'z'; ++c)
    table[c] = true;
  table['_'] = true;
  return table;
}();

static constexpr auto id_byte_lut = [] constexpr {
  std::array<bool, 256> table = id_start_byte_lut;
  for (char c = '0'; c <= '9'; ++c)
    table[c] = true;
  return table;
}();

static constexpr auto symetrical_table = [] constexpr {
  std::array<tokc::e, 256> table = {tokc::e::ERROR};
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  asign2table(table, (spelling)[0], tokc::e::closing_code);
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  asign2table(table, (spelling)[0], tokc::e::opening_code);
#include "../token.def"
  return table;
}();

static constexpr auto parallel_table = [] constexpr {
  std::array<tokc::e, 256> table = {tokc::e::ERROR};
#define TOKEN_PARALLEL_SEQUENCE(spelling, code, sgroup_code)                   \
  asign2table(table, spelling[0], tokc::e::code);

#include "../token.def"
  return table;
};
static constexpr auto parallel_table_bool = [] constexpr {
  std::array<bool, 256> table = {false};
#define TOKEN_PARALLEL_SEQUENCE(spelling, code, sgroup_code)                   \
  asign2table(table, spelling[0], true);

#include "../token.def"
  return table;
};

static constexpr auto symetrical_table_bool = [] constexpr {
  std::array<bool, 256> table = {false};
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  asign2table(table, (spelling)[0], true);
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  asign2table(table, (spelling)[0], true);
#include "../token.def"
  return table;
}();

static constexpr auto whitespace_table = [] constexpr {
  std::array<bool, 256> table = {false};
  table[' '] = true;
  table['\n'] = true;
  table['\t'] = true;
  return table;
}();

[[clang::always_inline]] inline auto
scan_for_word(llvm::StringRef src, pos_t pos) -> llvm::StringRef {
  const std::size_t size = src.size();
  while (pos < static_cast<pos_t>(size) &&
         id_byte_lut[static_cast<unsigned char>(src[pos])])
    ++pos;
  return src.substr(0, pos);
}

[[clang::always_inline]] inline auto
scan_for_number(llvm::StringRef src, pos_t pos) -> llvm::StringRef {
  const std::size_t size = src.size();
  while (pos < static_cast<pos_t>(size) && '0' <= src[pos] && src[pos] <= '9')
    ++pos;
  return src.substr(0, pos);
}

[[clang::always_inline]] inline auto
scan_for_symbol(llvm::StringRef src, pos_t pos) -> llvm::StringRef {
  const std::size_t size = src.size();
  while (pos < static_cast<pos_t>(size) && symbol_sequence_bytes[src[pos]])
    ++pos;
  return src.substr(0, pos);
}

[[clang::always_inline]] inline auto skip_hwhitespace(llvm::StringRef src,
                                                      pos_t &pos) -> void {
  while (LLVM_LIKELY(pos < static_cast<pos_t>(src.size())) &&
         LLVM_UNLIKELY(src[pos] == ' ' || src[pos] == '\t'))
    ++pos;
}
namespace lex {
[[clang::always_inline]] inline auto hwhitespace(llvm::StringRef src,
                                                 pos_t &pos) -> void {
  skip_hwhitespace(src, pos);
}
[[clang::always_inline]] inline auto
vwhitespace(lexer_t &lexer, llvm::StringRef src, pos_t &pos) -> void {
  while (pos < static_cast<pos_t>(src.size()) && (src[pos] == '\n')) {
    ++lexer.line_;
    ++pos;
  }
}
[[clang::always_inline]] auto word(llvm::StringRef src,
                                   pos_t &pos) -> llvm::StringRef {
  // CCHECK(id_start_byte_lut[src[pos]]);

  const auto idtext = scan_for_word(src.substr(pos), 0);
  // Failed to create token

  pos += idtext.size();
  return idtext;
}
[[clang::always_inline]] auto number(llvm::StringRef src,
                                     pos_t &pos) -> llvm::StringRef {
  // CCHECK(id_start_byte_lut[src[pos]]);

  const auto idtext = scan_for_number(src.substr(pos), 0);

  pos += idtext.size();
  return idtext;
}
[[clang::always_inline]] auto symbol(llvm::StringRef src,
                                     pos_t &pos) -> llvm::StringRef {
  // CCHECK(id_start_byte_lut[src[pos]]);
  const auto symbol_text = scan_for_symbol(src.substr(pos), 0);

  pos += symbol_text.size();
  return symbol_text;
}
} // namespace lex

// struct strview {
//   char *begin;
//   std::size_t len = 0;
//   constexpr strview(char *str, std::size_t l) : begin(str) {}

//   operator llvm::StringRef() { return llvm::StringRef(begin, len); }
//   operator std::string_view() { return std::string_view(begin, len); }
// };

namespace dispatch {
auto next(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN;

[[clang::noinline]]
auto err(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  llvm::errs() << "Error: Unexpected char " << src[pos] << " at index " << pos
               << '\n';
  std::exit(EXIT_FAILURE);
}

[[clang::noinline, clang::preserve_all]]
auto err(const std::string_view s, DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  llvm::errs() << "Error: " << s << "\n\tPos:" << pos << '\n';

  std::exit(EXIT_FAILURE);
}
auto symbol(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN;

auto comment(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  if (pos + 1 < src.size() && src[pos + 1] == '/') {
    pos += 2;
    while (pos < src.size() && src[pos] != '\n')
      ++pos;
    ++pos;
  } else {
    return symbol(DISPATCH_ARGS);
  }
  become next(DISPATCH_ARGS);
}

// acording to perf my biggest bottleneck is this one
auto id(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  const auto text = lex::word(src, pos);

  if (LLVM_UNLIKELY(text.size() == 0))
    return err("ID needs to have charactes in it", DISPATCH_ARGS);

  lexer.push(tokc::e::ID, bpos, pos);
  become next(DISPATCH_ARGS);
}

auto number(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  auto text = lex::number(src, pos);
  if (LLVM_UNLIKELY(text.size() == 0))
    return err("Number needs to have numbers in it", DISPATCH_ARGS);

  tokc::e type = tokc::e::INT;
  // bad for perfomance
  // maybe use a smaller dispatch table??
  bool dot = false;
AGAIN:
  if (LLVM_LIKELY(pos < src.size())) {
    if (src[pos] == '.') { /* float */
      ++pos;
      type = tokc::e::FLOAT;
      lex::number(src, pos);
      if (LLVM_UNLIKELY(src[pos] == '.') || dot)
        return err("A float number can only have one dot", DISPATCH_ARGS);
      dot = true;
      goto AGAIN;
    } else if (src[pos] == '\'') { /* int */
      if (!dot)
        type = tokc::e::INT;
      // do {
      ++pos;
      lex::number(src, pos);
      goto AGAIN;
      // } while (LLVM_UNLIKELY(src[pos] == '\''));
    }
  }

  // same goes for this one
  if (LLVM_LIKELY(pos < src.size() &&
                  LLVM_UNLIKELY(src[pos] == 'e'))) { /* exponent */
    ++pos;
    if (LLVM_LIKELY(src[pos] == '+' || src[pos] == '-'))
      ++pos;
    else
      err("Unknown exponent sign", DISPATCH_ARGS);

    const auto exp = lex::number(src, pos);
    if (LLVM_UNLIKELY(exp.size() == 0))
      err("Need to have a valid Exponent", DISPATCH_ARGS);
  }

  // and this one
  if (pos < src.size() &&
      LLVM_UNLIKELY(!whitespace_table[src[pos]] &&
                    !symbol_sequence_bytes[src[pos]] && src[pos] != ';' &&
                    !symetrical_table_bool[src[pos]]))
    err("Invalid following symbol after a number ", DISPATCH_ARGS);

  lexer.push(type, bpos, pos);
  become next(DISPATCH_ARGS);
}
// automaticaly build tables that lead to a function that leads to tables
// 
// Get all the possible symbols
// sort them by length
// match usinging that list
auto symbol(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  auto text = lex::symbol(src, pos);
  if (LLVM_UNLIKELY(text.size() == 0))
    return err("Symbol string needs to have symbols in it", DISPATCH_ARGS);

  auto r = llvm::StringSwitch<tokc::e>(text)
#define TOKEN_SYMBOL_SEQUENCE(spelling, code) .Case(spelling, tokc::e::code)
#include "../token.def"
               .Default(tokc::e::ERROR);

  if (LLVM_UNLIKELY(r == tokc::e::ERROR))
    err("Unknown symbol " + text.str(), DISPATCH_ARGS);

  lexer.push(r, bpos, pos);

  become next(DISPATCH_ARGS);
}

template <auto &fn> auto onechar(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  const tokc::e t = one_char_tok_lut[src[pos]];
  // CCHECK(LLVM_UNLIKELY(t == tokc::e::ERROR));
  fn(t, DISPATCH_ARGS);

  lexer.push(t, bpos, pos + 1);
  ++pos;

  become next(DISPATCH_ARGS);
}
// static auto onechar(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
//   auto t = one_char_tok_lut[src[pos]];
//   // CCHECK(LLVM_UNLIKELY(t == tokc::e::ERROR));
//   // create token
//   // --lexer.depth_;
//   lexer.push(t, bpos, pos + 1);
//   // ++lexer.depth_;
//   ++pos;

//   become next(DISPATCH_ARGS);
// }

namespace symetrical {

auto open(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  static constexpr auto fn = [](tokc::e r,
                                DISPATCH_ARGS_DECL) constexpr -> void {
    lexer.openstack_.push_back(symetrical_table[src[pos]]);
    // ++lexer.depth_;
  };
  become onechar<fn>(DISPATCH_ARGS);
}

// add a recovery token so we can continiue lexing
auto close(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  static constexpr auto fn = [](tokc::e r,
                                DISPATCH_ARGS_DECL) constexpr -> void {
    if (LLVM_UNLIKELY(lexer.openstack_.empty()))
      err("Openstack is empty", DISPATCH_ARGS);
    else if (LLVM_UNLIKELY(lexer.openstack_.back() != r))
      err("Wrong closing symetrical token", DISPATCH_ARGS);
    // --lexer.depth_;
    lexer.openstack_.pop_back();
  };

  {
    // auto t = one_char_tok_lut[src[pos]];
    fn(one_char_tok_lut[src[pos]], DISPATCH_ARGS);

    if (LLVM_UNLIKELY(
            !tokc::is_open_symetrical(lexer.buffer_.toks.back().type_) &&
            lexer.buffer_.toks.back().type_ != tokc::ENDSTMT))
      lexer.push(tokc::ENDSTMT, bpos, pos + 1);

    lexer.push(tokc::ENDGROUP, bpos, pos + 1);
    // ++lexer.depth_;
    ++pos;
    { become next(DISPATCH_ARGS); }
  }

  // become onechar<fn>(DISPATCH_ARGS);
}
} // namespace symetrical

auto scolon(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  {
    // auto t = one_char_tok_lut[src[pos]];
    lexer.push(tokc::ENDSTMT, bpos, pos + 1);
    // ++lexer.depth_;
    ++pos;
    { become next(DISPATCH_ARGS); }
  }

  // become onechar<fn>(DISPATCH_ARGS);
}

auto builtin(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  ++pos;
  auto text = lex::word(src, pos);
  // for some reason this gave us big perfomance boost
  // instead of using text directly matching the same thing just withough the @
  auto r = llvm::StringSwitch<tokc::e>(
               llvm::StringRef(text.begin() - 1, text.size() + 1))
#define TOKEN_BUILTIN_KEYWORD(spelling, code) .Case(spelling, tokc::e::code)
#include "../token.def"
               .Default(tokc::e::ERROR);

  if (LLVM_UNLIKELY(r == tokc::e::ERROR))
    err("Unknown builtin type: " + text.str(), DISPATCH_ARGS);
  lexer.push(r, bpos, pos);

  // push token here?
  become next(DISPATCH_ARGS);
}

auto hwhitespace(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  lex::hwhitespace(src, pos);
  become next(DISPATCH_ARGS);
}
auto vwhitespace(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  lex::vwhitespace(lexer, src, pos);
  become next(DISPATCH_ARGS);
}
auto strlit(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  // err(__PRETTY_FUNCTION__,DISPATCH_ARGS);
  ++pos;
  while (pos < src.size() && src[pos] != '\"')
    ++pos;
  ++pos;
  lexer.push(tokc::e::STRLIT, bpos, pos);

  become next(DISPATCH_ARGS);
}

constexpr auto dispatch_table = [] constexpr {
  dispatch_table_t table = {};
  for (int i = 0; i < 256; ++i) {
    if (id_start_byte_lut[i] == true)
      asign2table(table, i, dispatch::id);
    else
      asign2table(table, i, dispatch::err);
  }

  for (auto i = '0'; i <= '9'; ++i)
    asign2table(table, i, dispatch::number);

  asign2table(table, ' ', dispatch::hwhitespace);
  asign2table(table, '\t', dispatch::hwhitespace);
  asign2table(table, '\n', dispatch::vwhitespace);

  asign2table(table, '@', dispatch::builtin);
  asign2table(table, ';', dispatch::scolon);

#define TOKEN_SYMBOL_SEQUENCE(spelling, code)                                  \
  asign2table(table, (spelling)[0], dispatch::symbol);

// #define TOKEN_ONE_CHAR(spelling, code)                                         \
//   asign2table(table, (spelling)[0], onechar);
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  asign2table(table, (spelling)[0], symetrical::open);
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  asign2table(table, (spelling)[0], symetrical::close);
#include "../token.def"
  asign2table(table, '/', comment);

  asign2table(table, '\"', strlit);

  return table;
}();

[[clang::always_inline]] inline auto
next(DISPATCH_ARGS_DECL) -> DISPATCH_RETURN {
  if (LLVM_UNLIKELY(!(pos < static_cast<pos_t>(src.size()))))
    return 0;

  become dispatch_table[static_cast<unsigned char>(src[pos])](lexer, src, pos,
                                                              pos);
}
} // namespace dispatch

auto entry(const src_buffer_t *src) -> token_buffer_t  {
  lexer_t lexer;
  lexer.buffer_.toks = vec<token_t>::create(64);
  lexer.buffer_.locs = vec<srcloc_t>::create(64 * 4);
  lexer.buffer_.src = src;

  lexer.push(tokc::e::BEGINSYMETRICAL, 0, 0);

  dispatch::next(lexer, src->buffer(), 0, 0);

  if (!tokc::is_open_symetrical(lexer.buffer_.toks.back().type_) &&
      lexer.buffer_.toks.back().type_ != tokc::e::ENDSTMT)
    lexer.push(tokc::e::ENDSTMT, 0, 0);
  lexer.push(tokc::e::ENDGROUP, 0, 0);

  if (lexer.openstack_.size() > 0) {
    llvm::errs() << "Failed to close a symetrical" << '\n';
    std::exit(EXIT_FAILURE);
  }

  lexer.buffer_.toks.shrink_to_fit();
  lexer.buffer_.locs.shrink_to_fit();

  return std::move(lexer.buffer_);
  // return 0;
}

} // namespace lexer

#undef DISPATCH_RETURN
#undef DISPATCH_ARGS_DECL
#undef DISPATCH_ARGS
