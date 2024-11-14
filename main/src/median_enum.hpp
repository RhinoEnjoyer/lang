#pragma once

#include "token_enum.hpp"
#include <cstdint>
namespace medianc {
enum e : std::int32_t {
#define MEDIAN_CODE(code) code,
#include "median.def"
  last
};

constexpr inline const char *to_str(e val) {
  switch (val) {
#define MEDIAN_CODE(code)                                                      \
  case code:                                                                   \
    return #code;
#include "median.def"
  default:
    return "Unknown";
  }
}


constexpr inline e symetrical(tokc::e val) {
  switch(val){
    case tokc::LPAREN: return e::PARENS;
    case tokc::LBRACE: return e::BRACES;
    case tokc::LCBRACE: return e::CBRACES;

    case tokc::RPAREN: return e::PARENS;
    case tokc::RBRACE: return e::BRACES;
    case tokc::RCBRACE: return e::CBRACES;
    default: return e::last;
  }
}


} // namespace medianc
