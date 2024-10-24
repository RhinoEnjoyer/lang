#ifndef LEXER_HPP
#define LEXER_HPP

#include "../node.hpp"
#include <fstream>
#include <tuple>

namespace frontend{namespace lexer{

using lexer_return = std::tuple<snode, std::vector<loct>>;
lexer_return lex(std::ifstream &src_file);

}}
#endif
