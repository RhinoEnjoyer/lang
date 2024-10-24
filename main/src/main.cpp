#include <llvm/Support/VirtualFileSystem.h>
#include <unistd.h>

#include <cmath>
#include <cstdlib>
#include <iostream>

#include "./frontend/lexer.cpp"
#include "./frontend/parser.cpp"
#include "./source_buffer.hpp"
#include "mesure.hpp"
#include "token_str.hpp"

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

    auto [buffer, lex_time] = mesure([&] { return lexer::lex_(&src); });
    buffer.toks.shrink_to_fit();
    buffer.locs.shrink_to_fit();

    // constexpr double togb = 1024 * 1024 * 1024;
    static constexpr double togb = 1000 * 1000 * 1000;

    std::cout << "Lexer: " << lex_time.count() << "sec" << "\n"
              << "\tToken count: " << buffer.toks.size() << '\n'
              << "\tBuffer size in bytes: " << buffer.total_size_in_bytes()
              << '\n'

              << "\tProccess Speed: "
              << (static_cast<double>(src.buffer().size()) / togb) /
                     lex_time.count()
              << " GBytes/Sec" << '\n'

              << "\tGeneration Speed: "
              << (static_cast<double>(buffer.toks.size_in_bytes() +
                                      buffer.locs.size_in_bytes()) /
                  togb) /
                     lex_time.count()
              << " GBytes/Sec" << '\n'
              << "\n\n";

    // for (auto i = buffer.toks.cbegin(); i != buffer.toks.cend(); ++i) {
    //   std::cout << "token:" << token_code_str(i->type_) <<
    //   "\n\t"
    //             << "str:\"" << buffer.str(i) << "\"" << " pos:" << '{'
    //             // << "depth:" << buffer.token_depth(i) << ' '
    //             << "row:" << buffer.row(i) << ' '
    //             << "col:" << buffer.col(i) << ' '
    //             << "len:" << buffer.len(i) << '}' << '\n';
    // }

    /* Parser
      Tree like structure as a result
      Symetricals are guranteed to work
      Scolons are ENDSTMT
      Closing Symetricals are ENDGROUP
    */

    auto [parser_tree, parser_time] = mesure([&] {
      return parser::entry(buffer, buffer.toks.cbegin(), buffer.toks.cend());
    });
    std::cout << "Parser: " << parser_time << "\n"
              << "\tNode count: " << parser_tree.length() << "\n"
              << std::endl;
    parser::traverse(buffer,parser_tree,buffer);


    // auto stack = vec<token_t>::make(buffer.toks.size());
    // stack.release();

    parser_tree.release();
    buffer.locs.release();
    buffer.toks.release();
  };

  std::cout.imbue(std::locale("en_US.UTF-8"));
  auto fs = llvm::vfs::getRealFileSystem();
  lam(fs, "../example0.txt");

  return 0;
}
