#pragma once

#include "parser.hpp"
#include <memory>
#include <string_view>
#include <map>
#include <utility>
#include <variant>

namespace semantics {

template <typename T> using opt = std::optional<T>;
template <typename T> using sptr = std::shared_ptr<T>;

struct parser_it_t {
  podlist_t<parser::node_t>::it node;
  parser_it_t(podlist_t<parser::node_t>::it it) : node(it) {}
};

struct named_t {
  std::string_view name;
  named_t(std::string_view view) : name(view) {}
};

struct locale_t;
struct vardecl_t;

struct scopedecl_t : named_t, parser_it_t {
  sptr<semantics::locale_t> symbols;
};

struct block_t : parser_it_t {
  sptr<semantics::locale_t> symbols;
};

struct rec_t {
  std::vector<sptr<vardecl_t>> members;
};

struct lit_type_t {
  struct integral_t {};
  struct strlit_t {};
};

struct type_t {
  size_t id; // this is needed for uniqueness
  std::variant<sptr<rec_t>, sptr<lit_type_t>> var;
};

struct type_ref_t {
  std::variant<sptr<type_t>, sptr<type_ref_t>> ref;
};

struct value_t {};

struct lit_val_t {
  struct float_t {
    std::string_view str;
  };
  struct sint_t {
    std::string_view str;
  };
  struct uint_t {
    std::string_view str;
  };
  struct str_t {
    std::string_view str;
  };
  struct comp_t {
    sptr<type_t> type;
    std::vector<sptr<value_t>> init_list;
  };
};


struct vardecl_t : named_t, parser_it_t {};
struct typedecl_t : named_t, parser_it_t {};

struct locale_t {
  sptr<locale_t> parent;
  using var = std::variant<scopedecl_t, vardecl_t, typedecl_t>;

  using lookup_t = opt<std::reference_wrapper<var>>;

  std::unordered_map<std::string_view, var> table;

  auto insert(std::string_view name, locale_t::var val) -> auto {
    return this->table.try_emplace(name, std::move(val));
  }

  template <bool is_local>
  auto lookup(const std::string_view name) -> lookup_t {
    auto it = table.find(name);

    if (it != table.end())
      return it->second;

    if constexpr (is_local) {
      if (parent) [[likely]]
        return parent->lookup<is_local>(name);
    }
    return std::nullopt;
  }
};

struct locale_state_t {
  sptr<semantics::locale_t> global;
  sptr<semantics::locale_t> local;

  static auto make() -> locale_state_t {
    auto g = std::make_shared<semantics::locale_t>(locale_t{nullptr, {}});
    return {g, g};
  }

  auto emerge() -> bool {
    if (local->parent) {
      local = local->parent;
      return true;
    }
    return false;
  }

  auto insert(std::string_view name, locale_t::var val) -> auto {
    return local->insert(name, std::move(val));
  }

  auto dive(parser_it_t node, std::string_view name) -> auto {
    auto new_scope = std::make_shared<locale_t>(locale_t{local, {}});
    auto res = insert(name, scopedecl_t{name, node, new_scope});

    if (!res.second)
      return res;

    local = new_scope;
    return res;
  }
};

struct ctx_t {
  podlist_t<parser::node_t> &nodes;
  token_buffer_t &toks;
  locale_state_t loc;
};

using cursor_t = podlist_t<parser::node_t>::iterator;

auto var_decl(ctx_t &ctx, cursor_t cursor) {
  cursor++;

  auto n = cursor;
  cursor++;

  auto t = [&] -> opt<std::pair<cursor_t, size_t>> {
    if (cursor->is_median() && cursor->as_median()->type_ == medianc::TYPE)
      return std::pair{cursor, cursor->as_median().children().size()};
    return std::nullopt;
  }();

  if (t.has_value()) {
    auto tv = t.value();
    tv.first++;
    if (tv.first->is_final()) {
      auto tf = tv.first->as_final();
      switch (tf->type_) {
      case tokc::TYPE_FLOAT:
      case tokc::TYPE_INT:
      case tokc::TYPE_UINT:
        break;
      default:
        std::cerr << "Variable has unsuported type" << std::endl;
        std::abort();
      }
    } else if (tv.first->is_median()) {
      auto tm = tv.first->as_median();
      auto code = tm->type_;
      switch (code) {
      case medianc::CHAIN:
        // [&] {
        //   auto l = ctx.loc.local;
        //   auto ch = tm.children();
        //   for (size_t i = 0; i < ch.size(); i++) {
        //     auto &elm = *(ch.begin() + i);
        //     if (elm.is_final()) {
        //       auto final = elm.as_final();
        //       auto name = ctx.toks.str(final.get());
        //       auto result = l->lookup<true>(name);
        //       if (result.has_value()) {
        //         auto &val = result->get();
        //         if (!std::holds_alternative<scopedecl_t>(val)) {
        //           if (i != ch.size() - 1 &&
        //               !std::holds_alternative<typedecl_t>(val)) {
        //             std::cerr << "Invalid chain ending" << std::endl;
        //             std::abort();
        //           }
        //         } else {
        //           auto &s = std::get<scopedecl_t>(val);
        //           l = s.symbols;
        //         }
        //       } else {
        //         std::cerr << "Unknown symbol" << std::endl;
        //         std::abort();
        //       }
        //     } else {
        //       std::cerr << "Unsupported" << std::endl;
        //       std::abort();
        //     }
        //   }
        // }();
        // break;
      default:
        std::cerr << "Variable has unsuported type " << medianc::to_str(code)
                  << std::endl;
        std::abort();
      }
    }
  }

  if (t.has_value())
    cursor.advance(t->second + 1);

  auto v = [&] -> opt<std::pair<cursor_t, size_t>> {
    if (cursor->is_median() && cursor->as_median()->type_ == medianc::VALUE)
      return std::pair{cursor, cursor->as_median().children().size()};
    return std::nullopt;
  }();

  auto symbol = semantics::vardecl_t{ctx.toks.str(n->as_final()), n};
  auto [res, inserted] =
      ctx.loc.insert(ctx.toks.str(n->as_final()), symbol);
  if (!inserted) {
    std::cout << "failed to insert variable symbol" << std::endl;
    std::abort();
  }
}

auto type_decl(ctx_t &ctx, cursor_t cursor) {
  cursor++;
  auto n = cursor;
  cursor++;
  auto t = cursor;
  auto symbol = semantics::typedecl_t{ctx.toks.str(n->as_final()), n};
  auto [res, inserted] =
      ctx.loc.insert(ctx.toks.str(n->as_final()), symbol);
  if (!inserted) {
    std::cout << "Failed to insert type symbol" << std::endl;
    std::abort();
  }
}

auto stmt(ctx_t &ctx, cursor_t cursor) -> void;
auto scope_decl(ctx_t &ctx, cursor_t cursor) {
  cursor++;
  auto n = cursor;
  auto name = ctx.toks.str(n->as_final());

  auto check = ctx.loc.dive({n}, ctx.toks.str(n->as_final()));
  if (!check.second) {
    auto [othername, var] = *check.first;

    if (!std::holds_alternative<scopedecl_t>(var)) {
      std::cerr << "Failed to insert scope symbol" << std::endl;
      std::abort();
    }
    if (othername != name) {
      std::cerr << "Failed to insert scope symbol" << std::endl;
      std::abort();
    }
    ctx.loc.local = std::get<scopedecl_t>(check.first->second).symbols;
  }
  cursor++;
  // body
  auto b = cursor;
  auto span = b->as_median().children();
  auto cursor2 = span.begin();
  auto end = span.end();
  size_t c = 0;
  do {
    // std::cout << c << std::endl;
    auto med = cursor2->as_median();
    stmt(ctx, cursor2);
    cursor2 += med.children().size() + 1;
    c++;
  } while (cursor2 < end);
  ctx.loc.emerge();
}

auto stmt(ctx_t &ctx, cursor_t cursor) -> void {
  cursor.advance();
  auto med = cursor->as_median();

  switch (med->type_) {
  case medianc::DECL:
    var_decl(ctx, cursor);
    break;
  case medianc::SCOPE_DECL:
    scope_decl(ctx, cursor);
    break;
  case medianc::TYPE_DECL:
    type_decl(ctx, cursor);
    break;
  default:
    std::unreachable();
  }
}

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

auto dump_symbols(ctx_t &ctx, locale_t &locale,
                  const size_t depth = 0) -> void {
#define space_str std::string(depth * 6, ' ')
#define print std::cout << space_str << depth << "|"

  for (auto &elm : locale.table) {
    std::visit(overloaded{[&](vardecl_t &val) {
                            print << "Variable "
                                  << ctx.toks.str(val.node->as_final())
                                  << std::endl;
                          },
                          [&](typedecl_t &val) {
                            print << "Type "
                                  << ctx.toks.str(val.node->as_final())
                                  << std::endl;
                          },
                          [&](scopedecl_t &val) {
                            print << "Scope "
                                  << ctx.toks.str(val.node->as_final())
                                  << std::endl;
                            dump_symbols(ctx, *val.symbols.get(), depth + 1);
                          }},
               elm.second);
  }
#undef space_str
#undef print
}
auto entry(podlist_t<parser::node_t> &nodes, token_buffer_t &toks) {
  auto ctx = ctx_t{nodes, toks, locale_state_t::make()};
  auto span = nodes.begin()->as_median().children();

  auto cursor = span.begin();
  auto end = span.end();
  size_t c = 0;
  do {
    // std::cout << c << std::endl;
    // if(cursor->is_median()){
    auto med = cursor->as_median();
    stmt(ctx, cursor);
    cursor += med.children().size() + 1;
    // }
    // else{
    //   std::cerr << "Failed to find a median" << std::endl;
    //   std::abort();
    // }
    c++;
  } while (cursor < end);

  dump_symbols(ctx, *ctx.loc.local);
}

} // namespace semantics
