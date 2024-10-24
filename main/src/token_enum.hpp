#pragma once

#include <cstdint>
namespace tokc {

enum e : std::int32_t {
#define TOKEN_VIRTUAL(code) code,
#define TOKEN_SYMBOL_SEQUENCE(spelling, code) code,
#define TOKEN_ONE_CHAR(spelling, code) code,
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  code, sgroup_code,
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  code,
#define TOKEN_PARALLEL_SEQUENCE(spelling, code, sgroup_code) code, sgroup_code,
#define TOKEN_BUILTIN_KEYWORD(spelling, code) code,
#include "./token.def"

  LENGTH,
};

inline constexpr auto length() -> std::int64_t { return e::LENGTH; }
inline constexpr auto is_symetrical(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  case tokc::e::code:                                                          \
    return tokc::e::code;
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::code;

#include "./token.def"
  default:
    return false;
  }
}
inline constexpr auto is_open_symetrical(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  case tokc::e::code:                                                          \
    return tokc::e::code;

#include "./token.def"
  default:
    return false;
  }
}
inline constexpr auto open2close_symetrical(const tokc::e tok) -> tokc::e {
  switch (tok) {
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  case tokc::e::code:                                                          \
    return tokc::e::closing_code;

#include "./token.def"
  default:
    return tokc::e::ERROR;
  }
}
inline constexpr auto open2group_symetrical(const tokc::e tok) -> tokc::e {
  switch (tok) {
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  case tokc::e::code:                                                          \
    return tokc::e::sgroup_code;

#include "./token.def"
  default:
    return tokc::e::ERROR;
  }
}
inline constexpr auto close2group_symetrical(const tokc::e tok) -> tokc::e {
  switch (tok) {
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::sgroup_code;

#include "./token.def"
  default:
    return tokc::e::ERROR;
  }
}
inline constexpr auto close2open_symetrical(const tokc::e tok) -> tokc::e {
  switch (tok) {
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::opening_code;

#include "./token.def"
  default:
    return tokc::e::ERROR;
  }
}

inline constexpr auto symetrical_group(const tokc::e tok) -> tokc::e {
  switch (tok) {
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::sgroup_code;
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::sgroup_code;

#include "./token.def"
  default:
    return tokc::e::ERROR;
  }
}
inline constexpr auto is_close_symetrical(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                          \
    return tokc::e::code;

#include "./token.def"
  default:
    return false;
  }
}

inline constexpr auto is_builtin(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_BUILTIN_KEYWORD(spelling, code)                                  \
  case tokc::e::code:                                                          \
    return tokc::e::code;

#include "./token.def"
  default:
    return false;
  }
}

inline constexpr auto is_operator(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_OPERATOR(code)                                                   \
  case tokc::e::code:                                                          \
    return tokc::e::code;
#include "./token.def"
  default:
    return false;
  }
}

inline constexpr auto is_operand(const tokc::e tok) -> bool {
  switch (tok) {
#define TOKEN_OPERAND(code)                                                    \
  case tokc::e::code:                                                          \
    return tokc::e::code;
#include "./token.def"
  default:
    return false;
  }
}

} // namespace tokc