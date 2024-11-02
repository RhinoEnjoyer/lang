#pragma once
#include "./token_enum.hpp"
#include <string>
#include <string_view>

constexpr inline std::string_view token_code_str(const tokc::e tok) {
  switch (tok) {
#define TOKEN_VIRTUAL(code)                                                    \
  case tokc::e::code:                                                             \
    return #code;
#define TOKEN_SYMBOL_SEQUENCE(spelling, code)                                  \
  case tokc::e::code:                                                             \
    return #code;
#define TOKEN_ONE_CHAR(spelling, code)                                         \
  case tokc::e::code:                                                             \
    return #code;
#define TOKEN_SYMETRICAL_OPEN_SEQUENCE(spelling, code, closing_code,           \
                                       sgroup_code)                            \
  case tokc::e::code:                                                             \
    return #code;
#define TOKEN_SYMETRICAL_CLOSE_SEQUENCE(spelling, code, opening_code,          \
                                        sgroup_code)                           \
  case tokc::e::code:                                                             \
    return #code;

#define TOKEN_PARALLEL_SEQUENCE(spelling, code, sgroup_code)\
  case tokc::e::code:                                                             \
    return #code;\
  case tokc::e::sgroup_code:                                                             \
    return #sgroup_code;

#define TOKEN_BUILTIN_KEYWORD(spelling, code)                                  \
  case tokc::e::code:                                                             \
    return #code;
#include "./token.def"
  default:
    return "ERROR";
  }
}
