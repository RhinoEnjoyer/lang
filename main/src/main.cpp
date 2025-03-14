#include <cstdlib>
#include <iostream>
#include <llvm/Support/VirtualFileSystem.h>
#include <print>
#include <utility>

#include "./frontend/lexer.hpp"
#include "./frontend/parser.hpp"
#include "./frontend/semantics.hpp"
#include "./source_buffer.hpp"
#include "frontend/semantics_header.hpp"
#include "mesure.hpp"
#include "nicknames.hpp"
#include "token.hpp"

// #include <fmt/format.h>
// #include <fmt/printf.h>

#include <boost/program_options.hpp>
#include <iostream>
#include <string_view>

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
    auto src = [&filepath] -> auto {
      // auto file = std::filesystem::path{filepath};
      // if(!std::filesystem::exists(file))
      //   std::println(std::cerr, "\'{}\' doesn't exist", filepath);

      auto fs = llvm::vfs::getRealFileSystem();
      if (!fs->exists(filepath)) {
        std::println(std::cerr, "[[ERROR]] File \"{}\" doens't exist",
                     filepath);
        std::exit(EXIT_FAILURE);
      }
      std::cout << "Loading File: " << filepath << '\n';
      return [&] -> auto {
        auto opt = src_buffer_t::make(fs.get(), filepath);
        if (!opt) {
          std::cout << "Error: " << opt.getError().message() << '\n';
          std::abort();
        }
        return std::move(opt.get());
      }();
    }();

    std::println("\t{}bytes\n", src.length());

    auto [lex_out, lex_time] = mesure([&] { return lexer::entry(&src); });
    auto &[lex_output, lex_symetrical_map] = lex_out;

    auto [grammar_output, grammar_time] = mesure([&] {
      return grammar::entry(lex_output, lex_symetrical_map,
                            lex_output.toks.cbegin(), lex_output.toks.cend());
    });
    grammar::traverse(lex_output, grammar_output);

    auto symbol_pool = allocator_t::make();
    auto [symbols_result, symbols_time] = mesure([&] {
      return semantics::symbols::entry(symbol_pool, lex_output,
                                       *grammar_output.begin());
    });
    auto &[locale, stmts] = symbols_result;

    // semantics::resolve::entry(lex_output, locale, stmts);

    // TEST
    // {
    //   std::println("Running the test");
    //   auto lookup =
    //       semantics::locale_t::expect_lookup<semantics::decl_s::scope_decl_t>(
    //           locale, "core");

    //   if (!lookup.found)
    //     throw std::runtime_error("Did not find core");

    //   auto &scope = lookup.symbol.get();
    //   {
    //     auto lookup =
    //         semantics::locale_t::expect_lookup<semantics::decl_s::type_decl_t>(
    //             scope.loc, "allocator_t");
    //     if (!lookup.found)
    //       std::abort();
    //     std::println("core::allocator_t exits");
    //   }
    // }
    // // TEST

    std::cout << "\n"
              << "Lexer: " << lex_time << "\n"
              << "\tToken count: " << lex_output.toks.size()*sizeof(token_t)*sizeof(srcloc_t) << "\n"
              << "Grammar: " << grammar_time << '\n'
              << "\tNode count: " << grammar_output.length()*sizeof(grammar::node_t) << '\n'
              << "Symbols: " << symbols_time << '\n'
              << "\tPool size:" << symbol_pool.allocated_size() << "\n";

    symbol_pool.release();
    grammar_output.release();
    lex_output.locs.release();
    lex_output.toks.release();
  };

  std::cout.imbue(std::locale("en_US.UTF-8"));
  // after careful consideration llvm::vfs is kinda big and should go
  //  my to determine was sleep analysis :)
  lam(input_file);
  // lam("../main2.foo");

  return 0;
}
