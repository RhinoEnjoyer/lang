#include <functional>
#include <llvm/Support/VirtualFileSystem.h>
#include <map>
#include <optional>
#include <span>
#include <string_view>
#include <type_traits>
#include <unistd.h>

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <utility>
#include <variant>

#include "./frontend/lexer.hpp"
#include "./frontend/parser.hpp"
#include "./source_buffer.hpp"
#include "median_enum.hpp"
#include "mesure.hpp"
#include "token.hpp"
#include "become.hpp"
#include "token_str.hpp"

// ////////////
//   struct parser2{

//   struct ctx_t{
//     token_buffer_t& toks;
//   } ctx;
  
    
//   using cursor_t = podlist_t<parser::node_t>::c_it;

//   struct id_t{
//     std::string_view name;
//     id_t(std::string_view n): name(n) {}
//   };

//   struct scope_access_list_t{
//     podlist_t<id_t> list;  
//   };

//   struct symbol_t{
//     scope_access_list_t scope;
//     id_t name;
//   };

//   using type_var = std::variant<std::monostate, symbol_t>;
//   struct type_t: public type_var {
//     using type_var::variant;
//   };

//   struct operand_t {};

//   struct as_t {
//     type_t type;
//   };


//   struct plus_t{};
//   struct minus_t{};
//   struct lop_t{};
//   struct rop_t{};

//   using operator_var  = std::variant<std::monostate, lop_t, rop_t>;
//   struct operator_t: operator_var {
//     using operator_var::variant;
//   };

//   using expr_elm_var = std::variant<std::monostate, operand_t, operator_t>;
//   struct expr_t {
//     podlist_t<expr_elm_var> list;
//   };

//   struct attribute_t {};



//   struct decl_t {
//     podlist_t<attribute_t> attributes;
//     std::string_view name;
//     type_t type;
//   };

//   using val_var = std::variant<std::monostate, expr_t>;
//   struct val_t: public val_var {
//     using val_var::variant;
//   };
//   struct vardecl_t {
//     decl_t info;
//     val_t value;
//   };

//   struct import_t {std::string_view file;};

//   using stmt_var = std::variant<std::monostate,expr_t, vardecl_t, import_t>;
//   struct stmt_t: public stmt_var{
//     using stmt_var::variant;
//   };


//   auto type(cursor_t& cursor, const cursor_t end) -> type_t{
//     auto med = cursor->as_median();
//     cursor.advance();
//     if(med.type_ == medianc::CHAIN){
//       auto scopes = scope_access_list_t{};
//       if (med.len_ > 1)
//         for (int i = 0; i < med.len_ - 1; i++) {
//           scopes.list.push_back(ctx.toks.str(cursor->as_final()));
//           cursor.advance();
//           //a type can be considered also a scope
//           //or it might be collection
//           //but also scopes
//           //they might have compiletime arguments
//           if(cursor->is_median()){
//             std::cerr << "We can have cbraces as part of the syntax to "
//                          "indicate that a scope/namespace"
//                          " has a compile time feature but for now I don't "
//                          "support it and we do pure scopes on the syntax level"
//                          "\n";
//               std::abort();
//           }
//         }
//       return symbol_t{std::move(scopes), ctx.toks.str(cursor->as_final())};
//     }

//     return std::monostate{};
//   }
//   static auto call(auto fn, auto* t, cursor_t& cursor) -> auto{
//     auto med = cursor->as_median();
//     cursor++;
//     return fn(t, cursor, cursor + med.len_);
//   }
//   auto vardecl(cursor_t& cursor, const cursor_t end) -> vardecl_t{

//     auto attribs = podlist_t<attribute_t>{};
//     auto& n = cursor->as_final();
//     cursor++;
//     auto t = call(std::mem_fn(&parser2::type),this,cursor);
//     auto val = [&] -> val_t{
//       if(cursor == end)
//         return std::monostate{};
//       else
//         // return call(std::mem_fn(&parser2::exp));
//     }();

//     return vardecl_t{{std::move(attribs), ctx.toks.str(n), std::move(t)}, val};
//   }
//   auto stmt(cursor_t& cursor, const cursor_t end) -> stmt_t{
//     cursor++;
//     if (cursor->is_median()) [[likely]] {
//       auto& med = cursor->as_median();
//       if (med.type_ == medianc::DECL) 
//         return call(std::mem_fn(&parser2::vardecl), this, cursor);
//       return std::monostate{};
//     }  

//     std::unreachable();
//     return {};
//   }
//   };
// ////////////


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


    auto [buffer, lex_time] = mesure([&] { return lexer::entry(&src); });
    buffer.toks.shrink_to_fit();
    buffer.locs.shrink_to_fit();

    // constexpr double togb = 1024 * 1024 * 1024;
    static constexpr double togb = 1000 * 1000 * 1000;

    // for (auto i = buffer.toks.cbegin(); i != buffer.toks.cend(); ++i) {
    //   std::cout << "token:" << token_code_str(i->type_) <<
    //   "\n\t"
    //             << "str:\"" << buffer.str(i) << "\"" << " pos:" << '{'
    //             // << "depth:" << buffer.token_depth(i) << ' '
    //             << "row:" << buffer.row(i) << ' '
    //             << "col:" << buffer.col(i) << ' '
    //             << "len:" << buffer.len(i) << '}' << '\n';
    // }
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

    // std::exit(EXIT_SUCCESS);
    /* Parser
      Tree like structure as a result
      Symetricals are guranteed to work
      Scolons are ENDSTMT
      Closing Symetricals are ENDGROUP
    */

    //heaptracker shows that alot of allocations are done by std::variant or std::visit or at least they are realated to it?

    auto [parser_tree, parser_time] = mesure([&] {
      return parser::entry(buffer, buffer.toks.cbegin(), buffer.toks.cend());
    });

    // parser::traverse(buffer, parser_tree);
    std::cout << "\n"
              << "Parser token tree: " << parser_time << "\n"
              << "\tLength: " << parser_tree.length() << "\n"
              << std::endl;


    
  
    parser_tree.release();
    buffer.locs.release();
    buffer.toks.release();
  };

  std::cout.imbue(std::locale("en_US.UTF-8"));
  auto fs = llvm::vfs::getRealFileSystem();
  lam(fs, "../main2.foo");

  return 0;
}
