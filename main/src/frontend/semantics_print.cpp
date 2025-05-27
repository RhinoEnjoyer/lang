#include <iostream>
#include <print>
#include "./semantics.hpp"

namespace semantics{


#define instr std::string(indent * 3, ' ')
#define inprint(next)                                                          \
  std::print(std::cerr,"{}", instr);                                                     \
  next
#define printvar                                                               \
  std::println(std::cerr,"type:{}", boost::core::demangled_name(typeid(val)))
#define deflam                                                                 \
  [&](auto &val) {                                                             \
    inprint({});                                                               \
    std::cout << "Unknown type: " << boost::core::demangled_name(typeid(val))  \
              << std::endl;                                                    \
  }
__attribute__((visibility("default")))
void print(sptr<type_s::fnsig_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr,"{}", (void *)val.get_ptr()));

  if (val->ret_type)
    print(val->ret_type, indent + 1);
  if (val->template_args) {
    for (auto &elm : val->template_args->args) {
      ovisit(elm, [&](auto &val) { print(val.ptr(), indent + 1); });
    }
  }
}
__attribute__((visibility("default")))
void print(sptr<decl_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr, "{}", (void *)val.get_ptr()));
  inprint(std::println(std::cerr, "name: {}, id: {}", val->name, val->dindex));

  ovisit(
      val.get_val(),
      [&indent](decl_s::var_decl_t &val) {
        inprint(printvar);
        inprint(std::println(std::cerr, "mutability: {}", (val.mut)? "mutable": "immutable")) ;
        if (val.type)
          print(val.type, indent + 1);
        if (val.expr)
          print(val.expr, indent + 1);
      },
      [&indent](decl_s::field_t &val) {
        inprint(printvar);
        inprint(std::println("index:{}", val.index));
        print(val.decl.ptr(), indent + 1);
      },
      [&indent](decl_s::type_decl_t &val) {
        inprint(printvar);
        if (val.type)
          print(val.type, indent + 1);
      },
      [&indent](decl_s::template_stamp_decl_t &val) {
        inprint(printvar);
        inprint(std::println(std::cerr,"template module"));
        for (auto &elm : val.mod.targs->args) {
          ovisit(elm, [&](auto &val) { print(val.ptr(), indent + 1); });
        }
        if (val.type)
          print(val.type, indent + 1);
      },
      [&indent](decl_s::scope_decl_t &val) {
        inprint(printvar);
        print(val.frame.stmts, indent + 1);
      },
      [&indent](decl_s::fn_decl_t &val) {
        inprint(printvar);
        if (val.sig.ptr())
          print(val.sig.ptr(), indent + 1);
        if (val.body)
          print(val.body, indent + 1);
      },
      [&indent](decl_s::unwrap_decl_elm_t &val) {
        inprint(printvar);
        inprint(std::println(std::cerr,"index: {}", val.index));
      },
      [&indent](decl_s::template_var_input_t &val) { inprint(printvar); },
      [&indent](decl_s::template_type_input_t &val) { inprint(printvar); },
      [&indent](unresolved_t &val) { inprint(printvar); }, deflam);
}

__attribute__((visibility("default")))
void print(sptr<type_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr,"{}", (void *)val.get_ptr()));
  ovisit(
      val.get_val(),
      [&indent](type_s::type_ref_t &val) {
        inprint(printvar);
        inprint(std::println(std::cerr,"ref:{}", (void *)val.ref.get_ptr()));
      },
      [&indent](type_s::primitive_t &val) {
        inprint(printvar);
        ovisit(
            val,
            [&indent](type_s::numeric_t &val) {
              inprint(printvar);
              ovisit(val, [&indent](auto &val) {
                inprint(printvar);
                inprint(std::println(std::cerr,"size:{}", val.size));
              });
            },
            deflam);
      },
      [&indent](type_s::aggregate_t &val) {
        inprint(printvar);
        ovisit(
            val,
            [&indent](type_s::rec_t &val) {
              inprint(printvar);
              for (auto &elm : val.fields)
                print(elm.ptr(), indent + 1);
              for (auto &elm : val.idecls)
                print(elm, indent + 1);
            },
            [&indent](type_s::tup_t &val) {
              inprint(printvar);
              for (auto &elm : val.types)
                print(elm, indent + 1);
            });
      },
      [&indent](type_s::indirection_t &val) {
        inprint(printvar);
        ovisit(
            val,
            [&indent](auto &val) {
              inprint(printvar);
              print(val.type, indent + 1);
            },
            [&indent](type_s::optr_t &val) { inprint(printvar); });
      },
      deflam);
}

void print(expr_s::block_t &val, const size_t indent) {
  print(val.frame.stmts, indent);
}

void print(expr_s::chain_t& val, const size_t indent){
  auto visit_lam = [](expr_s::postfix_t &val, const size_t indent) {
    ovisit(
        val,
        [&indent](expr_s::post::var_access_t &val) {
          inprint(printvar);
          // print(val.val.ptr(), indent + 1);
        },
        [&indent](expr_s::post::fn_access_t &val) {
          inprint(printvar);
          // print(val.val.ptr(), indent + 1);
        },
        [&indent](expr_s::post::field_access_t &val) {
          inprint(printvar);
          // print(val.val.ptr(), indent + 1);
        },
        [&indent](auto &val) { inprint(printvar); });
  };
  for (auto &elm : val.chain) {
    // inprint(std::println("{{"));
    visit_lam(elm, indent + 1);
    // inprint(std::println("}}"));
  }
}
__attribute__((visibility("default")))
void print(sptr<expr_elm_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr,"{}", (void *)val.get_ptr()));
  print(val->type, indent+1);
  ovisit(
      val.get_val(),
      [&indent](expr_s::operator_t &val) {
        inprint(printvar);
        auto& meta = val.meta();
        inprint(std::println(std::cerr,"operator:{}", str(meta.op)));
        if (meta.has_payload) {
          //for now it is only for as
          auto& as_payload = val.get_as_payload();
          inprint(std::println(std::cerr,"as_payload"));
          print(as_payload.type, indent + 1);
        }
        ovisit(
            val, 
            [&](expr_s::bop_t &val) { 
              inprint(std::println("BOP")); 
              inprint(std::println("lhs{{")); 
              print(val.lhs, indent + 1);
              inprint(std::println("}}")); 
              inprint(std::println("rhs{{")); 
              print(val.rhs, indent + 1);
              inprint(std::println("}}")); 
            },
            [&](expr_s::uop_t &val) {
              inprint(std::println("UOPT"));
              inprint(std::println("the_one_side{{")); 
              print(val.operand, indent + 1);
              inprint(std::println("}}")); 
            },
            [](auto) {});
        
      },
      [&indent](expr_s::operand_t &val) {
        inprint(printvar);
        ovisit(
            val, 
            [indent](expr_s::block_t &val) { inprint(printvar); print(val, indent + 1); },
            [indent](expr_s::chain_t& val) { inprint(printvar); print(val, indent + 1); },
            [indent](auto &val) { inprint(printvar); });
      },
      [&indent](expr_s::subexpr_t &val) {
        inprint(std::println("Subexpr"));
        inprint(std::println("prev: {}", (void *)val.expr.get_ptr()));
        print(val.expr, indent + 1);
      },
      [&indent](auto &val) { inprint(printvar); });
}

__attribute__((visibility("default")))
void print(sptr<expr_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr, "{}", (void *)val.get_ptr()));
  print(val->type, indent+1);
    inprint(std::println("{{"));
    print(val->val, indent + 1);
    inprint(std::println("}}"));
}
__attribute__((visibility("default")))
void print(sptr<stmt_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr,"{}", (void *)val.get_ptr()));
  ovisit(
      val.get_val(),
      [&indent](stmt_s::import_t &val) {
        inprint(printvar);
        inprint(std::println(std::cerr,"{}", val.file));
      },
      [&indent](stmt_s::ret_t &val) {
        inprint(printvar);
        print(val.expr, indent + 1);
      },
      [&indent](stmt_s::forloop_t &val) { inprint(printvar); },
      [&indent](stmt_s::unwrap_decl_arr_t &val) {
        inprint(printvar);
        for (auto &elm : val.wraps) {
          print(elm, indent + 1);
        }
      },
      [&indent](stmt_s::unreachable_t &val) { inprint(printvar); },
      [&indent](sptr<decl_t> &val) { print(val, indent + 1); },
      [&indent](sptr<expr_t> &val) { print(val, indent + 1); }, deflam);
}
__attribute__((visibility("default")))
void print(sptr<stmts_t> &val, const size_t indent) {
  inprint(printvar);
  inprint(std::println(std::cerr,"{}", (void *)val.get_ptr()));
  for (auto &elm : val->stmts)
    print(elm, indent + 1);
}

#undef instr
#undef inprint
#undef printvar
#undef deflam

}
