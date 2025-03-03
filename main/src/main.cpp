#include <boost/program_options/options_description.hpp>
#include <llvm/Support/VirtualFileSystem.h>
#include <print>
#include <unistd.h>

#include <iostream>
#include <utility>

#include "./frontend/lexer.hpp"
#include "./frontend/parser.hpp"
#include "./frontend/semantics.hpp"
#include "./source_buffer.hpp"
#include "mesure.hpp"
// #include "./frontend/semantics4.hpp"
#include "token.hpp"

// #include <fmt/format.h>
// #include <fmt/printf.h>

#include <iostream>
#include <string_view>
#include <variant>

#include <boost/program_options.hpp>

// Helper to combine multiple lambdas for std::visit.
// template<class... Ts>
// struct overloaded : Ts... {
//   using Ts::operator()...;
// };
// template<class... Ts>
// overloaded(Ts...) -> overloaded<Ts...>;

// Simple indent helper.
void print_indent(int indent) {
  const auto space = std::string(3 * indent, ' ');
  std::print("{}", space);
}

// Forward declarations for visit functions.
void visit(const semantics::decl_t &node, int indent = 0);
void visit(const semantics::type_t &node, int indent = 0);
void visit(const semantics::expr_t &node, int indent = 0);
void visit(const semantics::stmts_t &node, int indent = 0);
void visit(const semantics::stmt_t &node, int indent = 0);

//
// Visit function for declarations
//
void visit(const semantics::decl_t &node, int indent) {
  print_indent(indent);
  std::println("decl_t {}", node.name);

  // Here we use std::visit on the underlying variant (decl_s::var) that decl_t
  // inherits.
  std::visit(overloaded{[&](const std::monostate &) {
                          print_indent(indent + 1);
                          std::cout << "monostate\n";
                        },
                        [&](const semantics::unresolved_t &) {
                          print_indent(indent + 1);
                          std::cout << "unresolved_t\n";
                        },
                        [&](const semantics::decl_s::unwrap_decl_elm_t &elm) {
                          print_indent(indent + 1);
                          std::cout << "unwrap_decl_elm_t (index " << elm.index
                                    << ")\n";
                          if (elm.init_val)
                            visit(*elm.init_val, indent + 2);
                        },
                        [&](const semantics::decl_s::var_decl_t &var_decl) {
                          print_indent(indent + 1);
                          std::cout << "var_decl_t\n";
                          if (var_decl.type)
                            visit(*var_decl.type, indent + 2);
                          if (var_decl.expr)
                            visit(*var_decl.expr, indent + 2);
                        },
                        [&](const semantics::decl_s::scope_decl_t &scope_decl) {
                          print_indent(indent + 1);
                          std::cout << "scope_decl_t\n";
                          // Optionally, if you want to print details of the
                          // locale:
                          if (scope_decl.loc) {
                            print_indent(indent + 2);
                            std::cout << "locale_t (child)\n";
                          }
                          if (scope_decl.stmts)
                            visit(*scope_decl.stmts, indent + 2);
                        },
                        [&](const semantics::decl_s::type_decl_t &type_decl) {
                          print_indent(indent + 1);
                          std::cout << "type_decl_t\n";
                          if (type_decl.type)
                            visit(*type_decl.type, indent + 2);
                        }},
             static_cast<const semantics::decl_s::var &>(node));
}

// Visit function for types
void visit(const semantics::type_t &node, int indent) {
  print_indent(indent);
  std::cout << "type_t\n";
  std::visit(
      overloaded{
          [&](const std::monostate &) {
            print_indent(indent + 1);
            std::cout << "monostate\n";
          },
          [&](const semantics::type_s::infered_t &) {
            print_indent(indent + 1);
            std::cout << "infered_t\n";
          },
          [&](const semantics::type_s::primitive_t &primitive) {
            print_indent(indent + 1);
            std::cout << "primitive_t\n";
            std::visit(
                overloaded{[&](const semantics::type_s::void_t &) {
                             print_indent(indent + 2);
                             std::cout << "void_t\n";
                           },
                           [&](const semantics::type_s::indirection_t &indir) {
                             print_indent(indent + 2);
                             std::cout << "indirection_t\n";
                             std::visit(
                                 overloaded{
                                     [&](const semantics::type_s::ref_t &) {
                                       print_indent(indent + 3);
                                       std::cout << "ref_t\n";
                                     },
                                     [&](const semantics::type_s::ptr_t &) {
                                       print_indent(indent + 3);
                                       std::cout << "ptr_t\n";
                                     },
                                     [&](const semantics::type_s::optr_t &) {
                                       print_indent(indent + 3);
                                       std::cout << "optr_t\n";
                                     }},
                                 indir);
                           },
                           [&](const auto &num) {
                             print_indent(indent + 2);
                             std::cout << typeid(num).name() << "\n";
                           }},
                primitive);
          },
          [&](const semantics::type_s::aggregate_t &agg) {
            print_indent(indent + 1);
            std::cout << "aggregate_t\n";
            std::visit(overloaded{[&](const semantics::type_s::rec_t &rec) {
                                    print_indent(indent + 2);
                                    std::cout << "rec_t\n";
                                    if (rec.loc) {
                                      print_indent(indent + 3);
                                      std::cout << "locale_t (child)\n";
                                    }
                                    if (rec.members)
                                      visit(*rec.members, indent + 3);
                                  },
                                  [&](const semantics::type_s::tup_t &tup) {
                                    print_indent(indent + 2);
                                    std::cout << "tup_t\n";
                                    for (auto &t : tup.types)
                                      if (t)
                                        visit(*t, indent + 3);
                                  }},
                       agg);
          },
          [&](const semantics::type_s::callable_t &callable) {
            print_indent(indent + 1);
            std::cout << "callable_t\n";
            std::visit(
                overloaded{[&](const semantics::type_s::fn_t &fn) {
                             print_indent(indent + 2);
                             std::cout << "fn_t\n";
                             // if (fn.body)
                             //   visit(*fn.body, indent + 2);
                           },
                           [&](const semantics::type_s::closure_t &closure) {
                             print_indent(indent + 2);
                             std::cout << "closure_t\n";
                             // if (closure.body)
                             //   visit(*closure.body, indent + 2);
                           }},
                callable);
          },
          [&](const semantics::unresolved_t &) {
            print_indent(indent + 1);
            std::cout << "unresolved_t\n";
          }},
      static_cast<const semantics::type_s::var &>(node));
}

// Visit function for expressions
void visit(const semantics::expr_t &node, int indent) {
  print_indent(indent);
  std::println("expr_t");
 //  std::visit(overloaded{[&](const std::monostate &) {
 //                          print_indent(indent + 1);
 //                          std::cout << "monostate\n";
 //                        },
 //                        [&](const semantics::unresolved_t &) {
 //                          print_indent(indent + 1);
 //                          std::cout << "unresolved_t\n";
 //                        },
 //                        [&](const auto &) {
 //                          print_indent(indent + 1);
 //                          std::cout << "unknown expresion\n";
 //                        }},
 //             static_cast<const semantics::expr_s::var &>(node));
}

//
// Visit function for statement lists
//
void visit(const semantics::stmts_t &stmts, int indent) {
  print_indent(indent);
  std::cout << "stmts_t\n";
  for (auto &stmt : stmts.stmts)
    if (stmt)
      visit(*stmt, indent + 1);
}

//
// Visit function for statements
//
void visit(const semantics::stmt_t &stmt, int indent) {
  print_indent(indent);
  std::cout << "stmt_t\n";
  std::visit(overloaded{[&](const std::monostate &) {
                          print_indent(indent + 1);
                          std::cout << "monostate\n";
                        },
                        [&](const semantics::stmt_s::unwrap_decl_arr_t &arr) {
                          print_indent(indent + 1);
                          std::cout << "unwrap_decl_arr_t\n";
                          // Visit each child declaration in the unwrap array.
                          for (auto &child : arr.wraps)
                            if (child)
                              visit(*child, indent + 2);
                        },
                        [&](const semantics::stmt_s::forloop_t &forloop) {
                          print_indent(indent + 1);
                          std::cout << "forloop_t\n";
                          if (forloop.loc) {
                            print_indent(indent + 2);
                            std::cout << "locale_t (child)\n";
                          }
                          if (forloop.ctrl_expr_stmts)
                            visit(*forloop.ctrl_expr_stmts, indent + 2);
                          if (forloop.body_stmts)
                            visit(*forloop.body_stmts, indent + 2);
                        },
                        [&](const semantics::stmt_s::import_t &imp) {
                          print_indent(indent + 1);
                          std::cout << "import_t: " << imp.file << "\n";
                        },
                        // When the statement is a declaration or an expression
                        // stored in a smart pointer:
                        [&](const sptr<semantics::decl_t> &decl_ptr) {
                          if (decl_ptr) {
                            visit(*decl_ptr, indent + 1);
                          }
                        },
                        [&](const semantics::stmt_s::ret_t &ret_stmt) {
                          std::println("ret_t");
                          if (ret_stmt.expr)
                            visit(ret_stmt.expr, indent + 1);
                        },
                        [&](const sptr<semantics::expr_t> &expr_ptr) {
                          if (expr_ptr)
                            visit(*expr_ptr, indent + 1);
                        }},
             static_cast<const semantics::stmt_s::var &>(stmt));
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
    auto src = [&filepath] -> auto {
      // auto file = std::filesystem::path{filepath};
      // if(!std::filesystem::exists(file))
      //   std::println(std::cerr, "\'{}\' doesn't exist", filepath);

      auto fs = llvm::vfs::getRealFileSystem();
      if (!fs->exists(filepath))
        std::cerr << "File: " << filepath << " doens't exist" << '\n';
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

    constexpr double togb = 1024 * 1024 * 1024;
    // static constexpr double togb = 1000 * 1000 * 1000;
    auto &buffer = lex_output;
    // print_token_buffer(buffer);
    // std::cout << "Lexer: " << lex_time << "\n"
    //           << "\tToken count: " << lex_output.toks.size() << '\n'
    //           << "\tByte size: " << lex_output.total_size_in_bytes() << '\n'

    //           << "\tProccess Speed: "
    //           << (static_cast<double>(src.buffer().size()) / togb) /
    //                  lex_time.count()
    //           << " GBytes/Sec" << '\n'

    //           << "\tGeneration Speed: "
    //           << (static_cast<double>(lex_output.toks.size_in_bytes() +
    //                                   lex_output.locs.size_in_bytes()) /
    //               togb) /
    //                  lex_time.count()
    //           << " GBytes/Sec" << '\n'
    //           << "\n\n";

    auto [grammar_output, grammar_time] = mesure([&] {
      return grammar::entry(lex_output, lex_symetrical_map,
                            lex_output.toks.cbegin(), lex_output.toks.cend());
    });
    grammar::traverse(lex_output, grammar_output);

    auto [semantics_result, semantics_time] = mesure([&] {
      return semantics::symbols::entry(lex_output, *grammar_output.begin());
    });
    auto &[locale, stmts] = semantics_result;

    // visit(*stmts);

    // TEST
    {
      std::println("Running the test");
      auto lookup =
          semantics::locale_t::expect_lookup<semantics::decl_s::scope_decl_t>(
              locale, "core");
      if (!lookup.found) {
        throw std::runtime_error("Did not find core");
      }
      auto &scope = lookup.symbol.get();
      {
        auto lookup =
            semantics::locale_t::expect_lookup<semantics::decl_s::type_decl_t>(
                scope.loc, "allocator_t");
        if (!lookup.found)
          std::abort();
        std::println("core::allocator_t exits");
      }
    }
    // TEST

    std::cout << "\nLexer: " << lex_time << "\n"
              << "\tToken count: " << lex_output.toks.size() << "\n";
    std::cout << "\n"
              << "Grammar: " << grammar_time << '\n'
              << "\tLength: " << grammar_output.length() << '\n'
              << "\tByte size: " << grammar_output.size_in_bytes() << '\n'
              << std::endl;
    std::println("Semantics symbols pass: {}", semantics_time);
    std::println("STMT count: {}", stmts->stmts.size());

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
