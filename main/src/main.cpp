#include <llvm/Support/VirtualFileSystem.h>
#include <print>
#include <unistd.h>

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <utility>

#include "./frontend/lexer.hpp"
#include "./frontend/parser.hpp"
// #include "./frontend/semantics.hpp"
#include "./source_buffer.hpp"
#include "mesure.hpp"
#include "./frontend/semantics4.hpp"
#include "token.hpp"

#include <fmt/format.h>
#include <fmt/printf.h>




auto print_token_buffer(token_buffer_t& buffer){
  constexpr int TYPE_WIDTH = 15;
  constexpr int STRING_WIDTH = 20;
  constexpr int ROW_WIDTH = 10;
  constexpr int LENGTH_WIDTH = 10;
  constexpr int COLUMN_WIDTH = 10;
  // Print header
  fmt::print("{:<{}} | {:<{}} | {:<{}} | {:<{}} | {:<{}}\n", "TYPE", TYPE_WIDTH,
             "STRING", STRING_WIDTH, "ROW", ROW_WIDTH, "COLUMN", COLUMN_WIDTH,
             "LENGTH", LENGTH_WIDTH);
  fmt::print("{:-<{}}-+-{:-<{}}-+-{:-<{}}-+-{:-<{}}-+-{:-<{}}\n", "",
             TYPE_WIDTH, "", STRING_WIDTH, "", ROW_WIDTH, "", COLUMN_WIDTH, "",
             LENGTH_WIDTH);

  // Loop through tokens and print rows
  for (auto i = buffer.toks.cbegin(); i != buffer.toks.cend(); ++i) {
    fmt::print("{:<{}} | {:<{}} | {:<{}} | {:<{}} | {:<{}}\n",
               token_code_str(i->type_), TYPE_WIDTH, buffer.str(i),
               STRING_WIDTH, buffer.row(i), ROW_WIDTH, buffer.col(i),
               COLUMN_WIDTH, buffer.len(i), LENGTH_WIDTH);
    }

}


int main() {
  auto lam = [](auto fs, const std::string filepath) {
    if (!fs->exists(filepath))
      std::cerr << "File: " << filepath << " doens't exist" << '\n';

    std::cout << "Loading File: " << filepath << '\n';
    auto src = [&] -> auto {
      auto opt = src_buffer_t::make(fs.get(), filepath);
      if (!opt) {
        std::cout << "Error: " << opt.getError().message() << '\n';
        std::abort();
      }
      return std::move(opt.get());
    }();
    std::cout << '\t' << src.length() << " bytes" << "\n\n";

    auto [lex_output, lex_time] = mesure([&] { return lexer::entry(&src); });

    constexpr double togb = 1024 * 1024 * 1024;
    // static constexpr double togb = 1000 * 1000 * 1000;
    auto &buffer = lex_output;
    print_token_buffer(buffer);

    std::cout << "Lexer: " << lex_time << "\n"
              << "\tToken count: " << lex_output.toks.size() << '\n'
              << "\tBuffer size in bytes: " << lex_output.total_size_in_bytes()
              << '\n'

              << "\tProccess Speed: "
              << (static_cast<double>(src.buffer().size()) / togb) /
                     lex_time.count()
              << " GBytes/Sec" << '\n'

              << "\tGeneration Speed: "
              << (static_cast<double>(lex_output.toks.size_in_bytes() +
                                      lex_output.locs.size_in_bytes()) /
                  togb) /
                     lex_time.count()
              << " GBytes/Sec" << '\n'
              << "\n\n";

    // std::exit(EXIT_SUCCESS);
    /* Parser
      Tree like structure as a result
      Symetricals are guranteed to work
      Scolons are ENDSTMT
      Closing Symetricals are ENDGROUP
    */

    //heaptracker shows that alot of allocations are done by std::variant or std::visit or at least they are realated to it?

    auto [parser_output, parser_time] = mesure([&] {
      return parser::entry(lex_output, lex_output.toks.cbegin(),
                           lex_output.toks.cend());
    });

    parser::traverse(lex_output, parser_output);
    std::cout << "\n"
              << "Parser: " << parser_time << "\n"
              << "\tLength: " << parser_output.length() << "\n"
              << std::endl;

    auto [table, semantics_time] = mesure([&] {
      return semantics::symbols::entry(lex_output, parser_output);
    });


    // semantics::visit(table);
    std::println();
    std::println("Semantics symbols pass: {}", semantics_time);
    for (auto &elm : table->table)
      semantics::symbols::visit(elm.second);

    parser_output.release();
    lex_output.locs.release();
    lex_output.toks.release();
  };

  std::cout.imbue(std::locale("en_US.UTF-8"));
  auto fs = llvm::vfs::getRealFileSystem();
  lam(fs, "../main.foo");
  // lam(fs, "../main2.foo");

  return 0;
}
