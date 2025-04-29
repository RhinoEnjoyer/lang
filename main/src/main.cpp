#include "./frontend/lexer.hpp"
#include "./frontend/parser.hpp"
#include "./frontend/semantics.hpp"
#include "./source_buffer.hpp"
#include "mesure.hpp"
#include "nicknames.hpp"
#include "token.hpp"
#include <boost/program_options.hpp>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <print>
#include <string_view>
#include <utility>

void print_tokens(const token_buffer_t &buffer) {
  std::cout << std::left << std::setw(8) << "Index" << std::setw(8) << "Type"
            << std::setw(8) << "Row" << std::setw(8) << "Col"
            << std::setw(8) << "Len" << "Str" << "\n";
  std::cout << std::string(50, '-') << "\n";

  for (auto it = buffer.toks.cbegin(); it != buffer.toks.cend(); ++it) {
    std::size_t index = buffer.to_index(it);
    std::cout << std::setw(8) << index << std::setw(8) << static_cast<int>(it->type())
              << std::setw(8) << buffer.row(it) << std::setw(8) << buffer.col(it)
              << std::setw(8) << buffer.len(it) << buffer.str(it) << "\n";
  }
}

int main(int argc, char *argv[]) {
  namespace prog_opts = boost::program_options;
  prog_opts::options_description desc("Allowed options");
  desc.add_options()("file,f", prog_opts::value<std::string>(),
                     "Specify input file");

  prog_opts::variables_map vm;
  prog_opts::store(prog_opts::parse_command_line(argc, argv, desc), vm);
  prog_opts::notify(vm);

  std::string input_file;
  if (vm.count("file"))
    input_file = vm["file"].as<std::string>();
  else {
    desc.print(std::cerr);
    throw std::runtime_error("Need a file");
  }

  auto lam = [](const std::string filepath) {
    // sleep(10);
    auto src = [&filepath]() -> src_buffer_t {
        namespace fs = std::filesystem;
        if (!fs::exists(filepath)) {
            std::cerr << "[[ERROR]] File \"" << filepath << "\" doesn't exist\n";
            std::exit(EXIT_FAILURE);
        }
        std::cout << "Loading File: " << filepath << '\n';
        try {
            return src_buffer_t::make(filepath);
        } catch (const std::exception& ex) {
            std::cerr << "Error: " << ex.what() << '\n';
            std::abort();
        }
    }();
    std::println("\t{}bytes\n", src.length());

    auto [lex_out, lex_time] = mesure([&] { return lexer::entry(&src); });
    auto &[lex_output, lex_symetrical_map] = lex_out;
    // print_tokens(lex_output);

    auto [grammar_output, grammar_time] = mesure([&] {
      return grammar::entry(lex_output, lex_symetrical_map,
                            lex_output.toks.cbegin(), lex_output.toks.cend());
    });

    grammar::traverse(lex_output, grammar_output);

    auto symbol_pool = allocator_t::make();
    auto [symbols_result, symbols_time] = mesure([&] {
      return semantics::symbols::entry(symbol_pool, lex_output, lex_symetrical_map,
                                       *grammar_output.begin());
    });
    auto &[file_locale, file_stmts] = symbols_result;
    semantics::print(file_stmts);
    std::cout << "\n"
              << "Lexer: " << lex_time << "\n"
              << "\tToken buffer byte count: " << lex_output.toks.size()*sizeof(token_t)*sizeof(srcloc_t) << "\n" //The compiler is tripping here ->
              << "Grammar: " << grammar_time << '\n'
              << "\tNode count: " << grammar_output.length()*sizeof(grammar::node_t) << '\n'
              << "Symbols: " << symbols_time << '\n'
              << "\tPool size:" << symbol_pool.allocated_size()
              << "\n";

    symbol_pool.release();
    grammar_output.release();
    lex_output.locs.release();
    lex_output.toks.release();
  };

  std::cout.imbue(std::locale("en_US.UTF-8"));
  // after careful consideration llvm::vfs is kinda big and should go
  //  my way to determine this was sleep analysis :)
  lam(input_file);
  // lam("../main2.foo");

  return 0;
}

