// #pragma once

// One way to not need to foward declare
//  try to lookup
//  if symbol doesn't exist add it to a queue of symbols that need to be
//  resolved retry to resolve the symbol

#include "../overloaded.hpp"
#include "parser.hpp"
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include "../nicknames.hpp"
#include <print>


namespace semantics {
// THE LSP AND THE MACROS ARE FUCKING THE SYMETRICAL MATCHING IN THE CODE
// SO WHEN I WANT TO GO THE TOP OF THIS FUNCTION I GET CANCER
// I AM NOT SURE WHY

struct type_t;
struct decl_t;
struct expr_t;

// template <typename T, typename BASE> using ssptr = specialized_shared_ptr<T, BASE>;

// somehow someway
// break the static type system
// just this once
//  to have a more customised locale
//  so for example,stmt_list can take whatever
//  but a fnsig_t can't take whatever it can only take vardecls
//  BUT the locale should be able to point to whatever other locale it needs to
//  no matter the restrictions

namespace experimental {

struct locale_inh {
  using var = sptr<decl_t>;
  using lookup_t = opt<std::pair<locale_inh *, var>>;
  using entry_t = std::pair<std::string_view, var>;
  using insert_t = opt<entry_t>;

  sptr<locale_inh> parent;

  virtual auto insert(const std::string_view name,locale_inh::var v) -> insert_t = 0;
  virtual auto ancestor_lookup(const std::string_view name) -> lookup_t = 0;
  virtual auto local_lookup(const std::string_view name) -> lookup_t = 0;
};

struct map_locale_t : public locale_inh {
  std::map<std::string_view, var> table;
  insert_t insert(std::string_view name, locale_inh::var val) override {
    auto res = this->table.try_emplace(name, val);
    if (!res.second)
      return std::nullopt;

    std::string_view res_name = res.first->first;
    var res_ptr = res.first->second;
    return std::pair{res_name, res_ptr};
  }

  template <bool is_local_search = false>
  lookup_t lookup(const std::string_view name) {
    auto it = table.find(name);
    if (it != table.end())
      return std::pair{this, it->second};

    if constexpr (!is_local_search)
      if (parent)
        return parent->ancestor_lookup(name);
    return std::nullopt;
  }

  lookup_t ancestor_lookup(const std::string_view name) override {
    return lookup<false>(name);
  }
  lookup_t local_lookup(const std::string_view name) override {
    return lookup<true>(name);
  }
};

struct linear_locale_t : public locale_inh {
  using index_t = size_t;
  std::vector<std::pair<std::string_view, var>> buffer;
  std::map<std::string_view, index_t> lookup_map;

  template <bool is_local> lookup_t lookup(const std::string_view name) {
    // Search in the local lookup map
    auto it = lookup_map.find(name);
    if (it != lookup_map.end())
      return std::pair{this, buffer[it->second].second};

    // If not found and is_local is false, check the parent
    if constexpr (!is_local) {
      if (parent)
        return parent->ancestor_lookup(name);
    }

    return std::nullopt;
  }

  insert_t insert(const std::string_view name, locale_inh::var val) override {
    if (lookup_map.find(name) != lookup_map.end())
      return std::nullopt;

    index_t index = buffer.size();
    buffer.emplace_back(name, val);
    lookup_map[name] = index;

    return std::pair{name, val};
  }

  lookup_t ancestor_lookup(const std::string_view name) override {
    return lookup<false>(name);
  }
  lookup_t local_lookup(const std::string_view name) override {
    return lookup<true>(name);
  }
};
} // namespace experimental

struct locale_t {
  sptr<locale_t> parent_;

  using var = sptr<decl_t>;
  using lookup_t = opt<std::pair<locale_t *, var>>;

  std::map<std::string_view, var> table;

  locale_t(sptr<locale_t> p) : parent_(p) {}
  locale_t() : parent_(nullptr) {}

  auto insert(std::string_view name, locale_t::var val) -> auto {
    return this->table.try_emplace(name, val);
  }

  bool operator==(const locale_t &rhs) const {
    auto &lhs = *this;

    // Check if parent pointers are equal or if parents themselves are equal
    if (lhs.parent_ != rhs.parent_) {
      if (!lhs.parent_ || !rhs.parent_ || !(*lhs.parent_ == *rhs.parent_))
        return false;
    }

    // Compare tables for equality
    if (lhs.table != rhs.table)
      return false;

    return true;
  }
  template <bool is_local_search = false>
  auto lookup(const std::string_view name) -> lookup_t {
    auto it = table.find(name);

    if (it != table.end())
      return std::pair{this, it->second};

    if constexpr (!is_local_search) {
      if (parent_)
        return parent_->lookup<is_local_search>(name);
    }
    return std::nullopt;
  }
};

struct number_inh {
  size_t size;

  number_inh() {}
  number_inh(std::string_view str)
      : size(std::stoull(std::string(str.substr(1)))) {}
};

struct float_t : public number_inh {
  float_t(std::string_view str) {
    this->size = std::stoull(std::string(str.substr(1)));
    switch (this->size) {
    case 32:
    case 64:
    case 80:
    case 128:
      break;
    default:
      std::cerr << str << " Unsupported float size: " << this->size << '\n';
      std::abort();
    }
  }
}; // f<NUMBER>

struct sint_t : public number_inh {}; // s<NUMBER>
struct uint_t : public number_inh {}; // u<NUMBER>

using number_t = var<float_t, sint_t, uint_t>;
struct boolean_t{};

struct ref_t {
  sptr<type_t> type;
};

struct ptr_t {
  sptr<type_t> type;
};

struct opaque_ptr_t {};

using primitive_t = var<opaque_ptr_t, ptr_t, ref_t, boolean_t, number_t>;

struct rec_t {
  sptr<locale_t> env;
  rec_t(sptr<locale_t> loc) : env(loc) {}
  // WIP
  bool operator==(const rec_t &other) const {
    return env->table == other.env->table;
  }
};

struct fnsig_t {
  opt<sptr<locale_t>> env;
  opt<sptr<type_t>> ret;

  fnsig_t(opt<sptr<locale_t>> loc, opt<sptr<type_t>> r)
      : env(std::move(loc)), ret(std::move(r)) {}

  bool operator==(const fnsig_t &other) const {
    if (*env != *other.env)
      return false;
    if (ret != other.ret)
      return false;
    return true;
  }
};
using type_var = var<rec_t, fnsig_t, primitive_t>;

struct type_t : public type_var {
  // type_var data;

  bool operator==(const type_t &other) const {
    if (this == &other)
      return true;
    return *this == other;
  }
  bool operator==(const sptr<type_t> &other) const {
    if (this == other.get())
      return true;
    return *this == *other;
  }
};

struct var_t {
  opt<sptr<type_t>> type;
  opt<sptr<expr_t>> val;
};

struct scope_t {
  sptr<locale_t> scope;
  bool operator==(const scope_t &other) const { return scope == other.scope; }
};

struct typedecl_t {
  sptr<type_t> type;
  bool operator==(const sptr<type_t> &other) const { return (*type == *other); }
};

struct fn_t {
  ssptr<fnsig_t, type_t> sig;
  opt<sptr<expr_t>> body; // do this when you are done with expresions
};

struct self_ptr {
  sptr<type_t> type;
};

using decl_var = var<var_t, scope_t, typedecl_t, fn_t, self_ptr>;
struct decl_t : public decl_var {
  using decl_var::operator=;
  std::string_view name;
};

} // namespace semantics
namespace semantics {
struct locale_state_t {
  sptr<semantics::locale_t> global_scope;
  sptr<semantics::locale_t> local_scope;

  sptr<type_t> type_ptr;
  auto get_type_ptr() { return type_ptr; }

  static auto make() -> locale_state_t {
    auto root = std::make_shared<semantics::locale_t>();
    return {root, root, nullptr};
  }

  template <bool local_search = false>
  auto lookup(std::string_view name) -> auto {
    return local_scope->lookup<local_search>(name);
  }

  auto insert(const std::string_view name,
              const semantics::locale_t::var val) -> auto {
    return local_scope->insert(name, val);
  }

  // makes a locale with that locale as it's parent
  auto make_locale() -> sptr<semantics::locale_t> {
    return std::make_shared<locale_t>(local_scope);
  }

  auto emerge() { local_scope = local_scope->parent_; }

  template <typename FNT, typename... T>
  auto dive(sptr<semantics::locale_t> &locale, FNT fn, T... args) {
    local_scope = locale;
    fn(args...);
    this->emerge();
  }
};

struct context_t {
  parser::node_buffer_t &nodes;
  token_buffer_t &toks;
  locale_state_t &locale_state;
};

using span_t = std::span<parser::node_t>;
using cursor_t = span_t::iterator;
[[clang::preserve_all]] auto handle_error(const std::string &message) -> void {
  std::cerr << message << std::endl;
  std::abort(); // Use EXIT_FAILURE for clarity
};

using list_fn = void (*)(context_t &ctx, cursor_t cursor);
template <list_fn fn> 
void list(context_t &ctx, span_t span) {
  auto cursor = span.begin();
  auto end = span.end();

  do {
    auto med = cursor->as_median();
    fn(ctx, cursor);
    cursor += med.children().size() + 1;
  } while (cursor < end);
}

auto var_decl(context_t &ctx, span_t ch) -> void;


auto fn_args(context_t &ctx, cursor_t cursor) -> void {
  cursor++;
  auto med = cursor->as_median();
  auto ch = med.children();
  switch (med.type()) {
  case medianc::DECL:
    return var_decl(ctx, ch);
  case medianc::SELF_ARG:
    return [&]{
      auto type_ptr = ctx.locale_state.get_type_ptr();
      if(!type_ptr)
        handle_error("@self arguemnt declaration can't be used outside of a record");
      ctx.locale_state.insert("@self", std::make_shared<decl_t>(self_ptr{type_ptr}));
    }();
  // case medianc::SCOPE_DECL:
  //   return scope_decl(ctx, ch);
  // case medianc::TYPE_DECL:
  //   return type_decl(ctx, ch);
  // case medianc::FN_DECL:
  //   return fn_decl(ctx, ch);
  default:
    handle_error(
        "Median is not a declaration in function arguments or a @self pointer");
  }
}
auto resolve_type(context_t &ctx, span_t ch) -> sptr<type_t>;

auto fn_sig(context_t &ctx, cursor_t cursor) {
  return [&] -> sptr<type_t> {
    auto med = cursor->as_median();
    auto new_locale = ctx.locale_state.make_locale();

    auto ch = med.children();
    if (ch.size() == 0)
      return std::make_shared<type_t>(fnsig_t{new_locale, std::nullopt});
    auto &args = *ch.begin();

    ctx.locale_state.dive(new_locale, list<fn_args>, ctx, args.as_median().children());

    auto ret = [&] -> opt<sptr<type_t>> {
      auto ret_parens = (ch.begin() + args.as_median().children().size() + 1);
      if (ch.end() <= ret_parens) {
        return std::nullopt;
      }
      auto med = ret_parens->as_median();
      if (med.children().size() == 0)
        return std::nullopt;
      // we have 1 type, this could change if we made it so
      // multiple types returns a @rec with those types but we
      // will see about that
      return resolve_type(ctx, med.children().front().as_median().children());
    }();
    // return something here otherwise illigal instruction or just segfault
    return std::make_shared<type_t>(fnsig_t(new_locale, ret));
  }();
}

auto stmt(context_t &ctx, cursor_t cursor) -> void;
auto resolve_type(context_t &ctx, span_t ch) -> sptr<type_t> {
  return [&] -> sptr<type_t> {
    auto cursor = ch.begin();
    auto final_visitor = [&](parser::node_t::final_t &val) -> sptr<type_t> {
      switch (val->type_) {
      case tokc::BUILTIN_PTR:
        return std::make_shared<type_t>(opaque_ptr_t());
      case tokc::TYPE_FLOAT:
        return std::make_shared<type_t>(float_t(ctx.toks.str(val)));
      case tokc::TYPE_INT:
        return std::make_shared<type_t>(sint_t(ctx.toks.str(val)));
      case tokc::TYPE_UINT:
        return std::make_shared<type_t>(uint_t(ctx.toks.str(val)));
      case tokc::TYPE_BOOLEAN:
        return std::make_shared<type_t>(boolean_t());
      default:
        std::unreachable();
      }
    };
    auto median_visitor = [&](parser::node_t::median_t &val) -> sptr<type_t> {
      auto ch = cursor->as_median().children();
      switch (val.type_) {
      case medianc::PTR:
        return std::make_shared<type_t>(ptr_t{resolve_type(
            ctx, ch.front()
                     .as_median()
                     .children())}); // if we didn't get the front we would
                                     // have a TYPE and we need the children
                                     // of that type not the TYPE it self
      case medianc::REF:
        return std::make_shared<type_t>(ref_t{
            resolve_type(ctx, ch.front().as_median().children())}); // same here
      case medianc::TYPEOF:
        return [&] -> sptr<type_t>{
          handle_error("Typeof is still a TODO");
          std::unreachable();
        }();
      case medianc::CHAIN:
        // this doesn't throw correctly
        return [&] -> sptr<type_t> {
          // I should find a way to do this without raw pointers
          auto locale = ctx.locale_state.local_scope.get();
          sptr<decl_t> val = nullptr;
          for (size_t i = 0; i < ch.size(); i++) {
            auto &elm = *(ch.begin() + i);

            auto fin = elm.as_checked<parser::node_t::final_t>();
            if (!fin->isa(tokc::ID))
              handle_error("Expected ID in type chain");

            std::string_view link = ctx.toks.str(fin);
            auto res = locale->lookup(link);
            if (!res.has_value())
              handle_error("Undefined reference: " + std::string(link));
            locale = res->first;
            val = res->second;

            if (i == ch.size() - 1 &&
                std::holds_alternative<typedecl_t>(*val)) {
              return std::get<typedecl_t>(*val).type;
            } else if (std::holds_alternative<scope_t>(*val)) {
              auto vptr = std::get<scope_t>(*val);
              auto new_locale = vptr.scope.get();
              // can this even be null?
              // if we get random segfaults around here I will change it
              // back
              //  if (!new_locale || (new_locale &&
              //  new_locale->table.size() == 0))
              if ((new_locale && new_locale->table.size() == 0))
                handle_error("Empty scope");
              locale = vptr.scope.get();
              continue;
            } else {
              handle_error("Symbol is not a type: " + std::string(link));
            }
          }

          std::unreachable();
        }();
      case medianc::RECORD:
        return [&] -> sptr<type_t> {
          cursor++;
          // auto rec = rec_t(ctx.locale_state.make_locale());
          auto ptr = ssptr<rec_t, type_t>(
              std::make_shared<type_t>(rec_t{ctx.locale_state.make_locale()}));

          // give state
          ctx.locale_state.type_ptr = ptr.ptr();

          auto med = cursor->as_median();
          auto span = med.children();
          ctx.locale_state.dive(ptr.get().env, std::function([&] {
                                  list<stmt>(ctx, span);
                                  return 0;
                                }));
          // reset it's state
          ctx.locale_state.type_ptr = nullptr;
          return ptr.val;
        }();
        break;
      case medianc::FN_SIG:
        return fn_sig(ctx, cursor);
        break;
      default:
        handle_error("Unsupported type");
        std::abort();
      }
    };

    auto err_visitor = [&](parser::node_t::err_t &val) -> sptr<type_t> {
      std::unreachable();
    };
    return std::visit(overloaded{final_visitor, median_visitor, err_visitor},
                      cursor->node);
  }();
}

auto var_decl(context_t &ctx, span_t ch) -> void {
  auto cursor = ch.begin();
  auto end = ch.end();
  auto name = ctx.toks.str(cursor->as_final());

  auto insert = ctx.locale_state.insert(name, std::make_shared<decl_t>());
  if (!insert.second)
    handle_error("Failed to insert type decl");
  auto ptr = insert.first->second;

  cursor++;
  auto type = [&] -> std::pair<opt<sptr<type_t>>, size_t> {
    if (ch.end() <= cursor)
      return {std::nullopt, 0};
    auto node = cursor;
    if (!node->is_median())
      return {std::nullopt, 0};

    auto med = node->as_median();
    if (med.type() != medianc::TYPE)
      return {std::nullopt, 0};
    auto ch = med.children();
    return std::pair{resolve_type(ctx, ch), ch.size()};
  }();
  cursor += type.second;

  auto val = opt<sptr<expr_t>>(std::nullopt);

  *ptr = var_t{type.first.value(), val};
}

auto type_decl(context_t &ctx, span_t ch) {
  // smth smth recursion smth smth infinite loop smth smth big bad
  auto cursor = ch.begin();
  const auto name = ctx.toks.str(cursor->as_final());
  auto insert = ctx.locale_state.insert(name, std::make_shared<decl_t>());
  if (!insert.second)
    handle_error("Failed to insert type decl");
  auto ptr = insert.first->second;

  cursor++;

  // a type ref can only be only alias directly
  //  I think it is better this way
  auto type = [&] -> sptr<type_t> {
    auto med = cursor->as_median();
    auto ch = med.children();
    return resolve_type(ctx, ch);
  }();

  *ptr = typedecl_t{type};
}

auto scope_decl(context_t &ctx, span_t ch) {
  auto cursor = ch.begin();
  const auto name = ctx.toks.str(cursor->as_final());
  auto lookup = ctx.locale_state.lookup<true>(name);

  sptr<locale_t> new_locale;
  if (lookup.has_value()) {
    auto decl = lookup->second;
    if (std::holds_alternative<scope_t>(*decl)) {
      auto vptr = std::get<scope_t>(*decl);
      new_locale = vptr.scope;
    } else {
      handle_error(
          "Symbol already exists: I AM SUPPOSED TO PRINT THE SYMBOL NAME HERE");
    }
  } else {
    new_locale = ctx.locale_state.make_locale();
    auto res = ctx.locale_state.insert(
        name, std::make_shared<decl_t>(scope_t{new_locale}));
    if (!res.second) {
      handle_error("Failed to insert scope: I AM SUPPOSED TO PRINT THE SYMBOL "
                   "NAME HERE");
    }
  }

  // Scope can be empty
  if (ch.size() != 2) {
    cursor += 2;
    ctx.locale_state.dive(new_locale, list<stmt>, ctx,
                          std::span{cursor, ch.end()});
  }
  // throw std::runtime_error(std::string(__func__ )+ " Not Yet Homie");
}

auto fn_decl(context_t &ctx, span_t ch) {
  auto cursor = ch.begin();
  const auto name = ctx.toks.str(cursor->as_final());
  auto insert = ctx.locale_state.insert(name, std::make_shared<decl_t>());
  if (!insert.second)
    handle_error("Failed to insert fn decl");

  cursor++;
  auto type_ch = cursor->as_median().children();
  auto type = fn_sig(ctx, cursor);

  cursor += type_ch.size();

  *insert.first->second = decl_t(fn_t{type, std::nullopt});
}

auto stmt(context_t &ctx, cursor_t cursor) -> void {
  cursor++;
  auto med = cursor->as_median();
  auto ch = med.children();
  switch (med.type()) {
  case medianc::DECL:
    return var_decl(ctx, ch);
  case medianc::SCOPE_DECL:
    return scope_decl(ctx, ch);
  case medianc::TYPE_DECL:
    return type_decl(ctx, ch);
  case medianc::FN_DECL:
    return fn_decl(ctx, ch);
  default:
    std::unreachable();
  }
}

auto visit(const sptr<locale_t> &loc, const size_t depth = 0) -> void;

#define dprint std::cout << std::string(depth * 3, ' ')
template<bool recursion = true>
void type_visit(type_t &val, const size_t depth) {
  std::visit(
      overloaded{[&](const fnsig_t &val) -> void {
                   dprint << "type fnsig ";
                   if (val.ret.has_value())
                     type_visit(*val.ret.value(), depth + 1);
                   if (val.env.has_value()) {
                     std::cout << '\n';
                     visit(val.env.value(), depth + 1);
                   }
                 },
                 [&](const rec_t &val) -> void {
                   dprint << "type record \n";
                   if constexpr (recursion)
                     visit(val.env, depth + 1);
                 },
                 [&](const primitive_t &val) -> void {
                   return std::visit(overloaded{
                       [&](const ref_t &val) -> void {
                         dprint << "type ref \n";
                         type_visit(*val.type, depth + 1);
                       },
                       [&](const ptr_t &val) -> void {
                         dprint << "type ptr \n";
                         type_visit(*val.type, depth + 1);
                       },
                       [&](const opaque_ptr_t &val) -> void {
                         dprint << "type opaque ptr";
                       },
                       [&](const boolean_t &val) -> void {
                         dprint << "boolean \n";
                       },
                       [&](const number_t &val) -> void {
                         std::visit(
                             overloaded{[&](const float_t &val) -> void {
                                          dprint << "type float: " << val.size << "bit";
                                        },
                                        [&](const sint_t &val) -> void {
                                          dprint << "type sint: " << val.size << "bit";
                                        },
                                        [&](const uint_t &val) -> void {
                                          dprint << "type uint: " << val.size << "bit";
                                        }},
                             val);
                         // std::cout << "integral " << val.bit_size << "bit";
                       }},val);
                 }},
      val);
}

void visit(const sptr<locale_t> &loc, const size_t depth) {
  for (auto &elm : loc->table) {
    auto ptr = elm.second;
    dprint << elm.first << " ";
    std::visit(overloaded{[&](const var_t &val) {
                            std::cout << "var decl\n";
                            if (val.type.has_value())
                              type_visit(*val.type.value(), depth + 1);
                            std::cout << '\n';
                          },
                          [&](const typedecl_t &val) {
                            std::cout << "type decl\n";
                            type_visit(*val.type, depth + 1);
                            std::cout << "\n";
                          },
                          [&](const scope_t &val) {
                            std::cout << "scope decl\n";
                            visit(val.scope, depth + 1);
                          },
                          [&](const fn_t &val) {
                            std::cout << "fn decl\n";
                            type_visit(*val.sig.ptr(), depth + 1);
                          },
                          [&](const self_ptr &val) {
                            std::cout << "self pointer\n";
                            type_visit<false>(*val.type, depth + 1);
                          }},
               *elm.second);
  }
}

auto entry(podlist_t<parser::node_t> &nodes,
           token_buffer_t &toks) -> sptr<locale_t> {

  auto loc = locale_state_t::make();
  auto ctx = context_t{nodes, toks, loc};
  auto span = nodes.begin()->as_median().children();

  list<stmt>(ctx, span);

  // visit(loc.local_scope);
  return std::move(loc.global_scope);
}

} // namespace semantics
