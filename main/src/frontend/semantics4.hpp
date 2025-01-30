#include "../nicknames.hpp"
#include "./parser.hpp"
#include <any>
#include <memory>
#include <print>
#include <ostream>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>

#include "../table.hpp"

namespace semantics {
struct decl_t;
struct type_t;
struct expr_t;

using node_t = parser::node_t::external_node;
using span_t = parser::node_t::median_proxy_t<false>::span_t;
using cursor_t = span_t::iterator;
using median_t = parser::node_t::median_proxy_t<false>;
using final_t = parser::node_t::final_t;
using err_t = parser::node_t::err_t;

struct locale_t;

namespace decl_s {
struct type_decl_t {
  sptr<type_t> type;
};

struct scope_decl_t {
  sptr<semantics::locale_t> loc;
};

struct var_decl_t {
  sptr<type_t> type;
  sptr<expr_t> init_val;
};

// this could be used but it would work better with inheritance for decl_index
// and stuff ??
// using var = svar<median_t, var_decl_t, type_decl_t,
// scope_decl_t>;
//
using var = var<var_decl_t, type_decl_t, scope_decl_t, median_t>;
} // namespace decl_s

struct decl_t : public decl_s::var {
  using decl_s::var::operator=;
  decl_t() : decl_s::var(), decl_index(-1) {}

  decl_t(decl_s::var val, std::string_view n, size_t i)
      : decl_s::var(val), name(n), decl_index(i) {}

  std::string_view name;
  size_t decl_index;
};

namespace type_s {
// this might be the struct that I should change evrything to a inheritance
// based locale_t
//  where I can one with a map and one with a vector
struct record_t {
  //  the order of a record matters so I should have it use a vector?
  //  allow the locale to give a list of pointers based on the decl_index?
  //  use std::set?
  sptr<locale_t> loc;
};
struct unresoved_t {
  median_t med;
};
struct bitsize_t {
  size_t size;
  bitsize_t(size_t s) : size(s) {}
  bitsize_t(std::string_view str)
      : size(std::stoull(std::string(str.substr(1)))) {}

  auto tobyte() -> size_t { return size / 8; }
};
struct float_t  :public bitsize_t {
  //could make it bettr but it works and I do not think I will ever change it so it is fine
  float_t(const size_t len) : bitsize_t(len) {
    if (size != 16 && size != 32 && size != 64 && size != 80 && size != 128) {
      throw std::invalid_argument(
          "Invalid bit size for float_t. Allowed: 16, 32, 64, 80, 128.");
    }
  }
  float_t(std::string_view str) : bitsize_t(str) {
    if (size != 16 && size != 32 && size != 64 && size != 80 && size != 128) {
      throw std::invalid_argument(
          "Invalid bit size for float_t. Allowed: 16, 32, 64, 80, 128.");
    }
  }
};
struct sint_t   :public bitsize_t {using bitsize_t::bitsize_t;};
struct uint_t   :public bitsize_t {using bitsize_t::bitsize_t;};
struct boolean_t:public bitsize_t {using bitsize_t::bitsize_t;};
using numeric_t = var<float_t, sint_t, uint_t, boolean_t>;

struct optr_t {};
struct ptr_t {
  sptr<type_t> type;
};
struct ref_t {
  sptr<type_t> type;
};
using indirection_t = var<optr_t, ptr_t, ref_t>;

struct tup_t {
  list<sptr<type_t>> types;
};

using var =
    var<unresoved_t, record_t, tup_t, indirection_t, numeric_t>;
}; // namespace type_s

struct type_t : public type_s::var {};

struct locale_t {
  // this is basicaly our prespective
  // since we do lookups from the prespective of the symbol
  using val_t = sptr<decl_t>;
  using entry_t = val_t;
  using insert_t = std::tuple<bool, std::string_view, val_t>;

  struct path_t {
    using val_t = sptr<locale_t>;
    list<val_t> path;

    void push(const val_t &locale) { path.push_back(locale); }
    void push(const val_t &&locale) { path.push_back(std::move(locale)); }

    val_t latest() const noexcept {
      // the path can't be empty
      return path.back();
    }
    const list<val_t> &all() const { return path; }
    bool empty() const { return path.empty(); }
    size_t size() const { return path.size(); }
    void clear() { path.clear(); }
  };
  using lookup_t = std::tuple<bool, path_t, entry_t>;
  sptr<locale_t> parent_;

  // general
  static sptr<locale_t> make_child(sptr<locale_t> self) {
    return std::make_shared<locale_t>(locale_t{self, {}});
  }

  sptr<locale_t> parent() { return parent_; }
  
  // implementation specific
  struct internal {
    std::unordered_map<std::string_view, entry_t> table;
    static lookup_t ancestor_lookup(sptr<locale_t> self, path_t &path,
                                    const std::string_view name) {
      return lookup<false>(self, path, name);
    }
    static lookup_t local_lookup(sptr<locale_t> self, path_t &path,
                                 const std::string_view name) {
      return lookup<true>(self, path, name);
    }
    template <bool is_local_search>
    static lookup_t lookup(sptr<locale_t> &self, path_t &path,
                           const std::string_view name) {
      path.push(self);
      auto it = self->internals.table.find(name);

      if (it != self->internals.table.end())
        return {true, path, it->second};

      if constexpr (!is_local_search)
        if (self->parent_)
          return ancestor_lookup(self->parent_, path, name);

      return {false, path, {nullptr}};
    }

    insert_t try_insert(std::string_view name, entry_t val) {
      auto res = table.try_emplace(name, val);
      return {res.second, name, res.first->second};
    }
  } internals;

  insert_t try_insert(std::string_view name, entry_t val) {
    return internals.try_insert(name, val);
  }
  static lookup_t ancestor_lookup(sptr<locale_t> self,
                                  const std::string_view name) {
    path_t path;
    return internal::lookup<false>(self, path, name);
  }
  static lookup_t local_lookup(sptr<locale_t> self,const std::string_view name) {
    path_t path;
    return internal::lookup<true>(self, path, name);
  }
};

struct context_t {
  // data
  token_buffer_t &toks;
  size_t decl_index;
  sptr<locale_t> loc;

  size_t increment() { return decl_index++; }
};

struct cursor_helper_t {
  cursor_helper_t(span_t s) : span_(s), cursor_(s.begin()) {}
  template <typename t, auto IN = medianc::VIRTUAL_EMPTY>
  std::optional<t> extract() {
    if (!std::holds_alternative<t>(cursor_->node()) || !within()) {
      return std::nullopt;
    }
    if constexpr (std::is_same_v<t, median_t>) {
      static_assert(std::is_same_v<decltype(IN), medianc::e>);
      auto med = cursor_->as_median();
      if constexpr (IN == medianc::VIRTUAL_EMPTY) {
        cursor_.advance();
        return med;
      } else {

        if (med.type() == IN) {
          cursor_.advance();
          return med;
        }
      }
    } else if constexpr (std::is_same_v<t, final_t>) {

      static_assert(std::is_same_v<decltype(IN), tokc::e>);
      auto fin = cursor_->as_final();
      if constexpr (IN == tokc::VIRTUAL_EMPTY) {
        cursor_.advance();
        return fin;
      } else {
        if (fin->isa(IN)) {
          cursor_.advance();
          return fin;
        }
      }
    } else {
      static_assert(false, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
    }
    return std::nullopt;
  }

  bool within() const { return span_.within(cursor_); }

private:
  span_t span_;
  cursor_t cursor_;
};

namespace symbols{
template <auto fn> void consume_list(context_t &ctx, median_t &med) {
  auto ch = med.children();
  auto cursor = ch.begin();
  while (ch.within(cursor)) {
    auto node = cursor->as_median(); // Get statement node
    fn(ctx, node);                   // Process using stmt()
    cursor.advance();
  }
}

auto type_decl(context_t &ctx, median_t med) -> void;
auto var_decl(context_t &ctx, median_t med) -> void;
auto scope_decl(context_t &ctx, median_t med) -> void;
auto forloop(context_t &ctx, median_t med) -> void;
auto expr(context_t &ctx, median_t med) -> void;
auto import(context_t &ctx, median_t med) -> void;
auto stmt(context_t &ctx, median_t stmt_node) -> void;
auto type_deduction(context_t &ctx, median_t type_med)
    -> sptr<type_t>;

// we do the primitives, records, unions, function, enums, ....
//  only the basic stuff no symbol resolution yet
//  we leave that on the resolution part that comes next
auto record(context_t &ctx, median_t med) -> sptr<type_t> {
  auto cursor = cursor_helper_t{med.children()};

  auto templates = cursor.extract<median_t, medianc::TEMPLATE_LIST>();
  auto body = cursor.extract<median_t, medianc::BODY>();

  if (templates) {
    // template code
  }
  if (!body) {
    throw std::runtime_error("Records need a body");
  }

  // this pattern is on other places as well
  //  make a function to handle it
  auto new_locale = locale_t::make_child(ctx.loc);

  auto old_locale = ctx.loc;
  ctx.loc = new_locale;
  // change this to something that is more restricted
  consume_list<stmt>(ctx, body.value());
  ctx.loc = old_locale;

  auto rec = type_s::record_t{new_locale};
  return std::make_shared<type_t>(rec);
};

auto union_(context_t &ctx, median_t med) -> sptr<type_t> {
  return nullptr;
};
auto enum_(context_t &ctx, median_t med) -> sptr<type_t> {
  return nullptr;
};
auto fnsig(context_t &ctx, median_t med) -> sptr<type_t> {
  return nullptr;
};

auto ptr(context_t &ctx, median_t med) -> sptr<type_t> {
  auto type_med = med.children().begin()->as_median();
  return std::make_shared<type_t>(type_s::ptr_t{type_deduction(ctx, type_med)});
}
auto ref(context_t &ctx, median_t med) -> sptr<type_t> {
  auto type_med = med.children().begin()->as_median();
  return std::make_shared<type_t>(type_s::ref_t{type_deduction(ctx, type_med)});
}
auto tup(context_t &ctx, median_t tup_node) -> sptr<type_t> {
  auto ch = tup_node.children();
  auto types = list<sptr<type_t>>{};
  types.reserve(ch.size2());
  for (auto& elm : ch) { // it has to do with auto&, it should have the &, before I had it as a value, I DO NOT KNOW 100% why I guess I use the pointer of that to get the next value
    { //the children are wrong, instead of getting the children of the element the only correcet thing is the parent for some reason
      auto elm_med = elm.as_median();
      auto elm_ch = elm_med.children();
       auto med = elm_ch.begin()->as_median();
      types.push_back(type_deduction(ctx, med));
    }
  }
  return std::make_shared<type_t>(type_s::tup_t{std::move(types)});
}

auto sint(context_t &ctx, final_t sint_node) -> sptr<type_t> {std::unreachable();}
auto uint(context_t &ctx, final_t uint_node) -> sptr<type_t> {std::unreachable();}
auto float_(context_t &ctx, final_t float_node) -> sptr<type_t> {std::unreachable();}
auto boolean_(context_t &ctx, final_t float_node) -> sptr<type_t> {std::unreachable();}
auto primitive(context_t &ctx, final_t float_node) -> sptr<type_t> {std::unreachable();}

auto type_deduction(context_t &ctx, median_t type_med) -> sptr<type_t> {
  auto node = type_med.children().begin()->node();
  return ovisit(
      node,
      [&ctx](median_t &med) -> sptr<type_t> {
        switch (med.type()) {
        case medianc::CHAIN:
          return std::make_shared<type_t>(type_s::unresoved_t{med});
        case medianc::RECORD:
          return record(ctx, med);
        case medianc::ENUM:
          return enum_(ctx, med);
        case medianc::UNION:
          return union_(ctx, med);
        case medianc::PTR:
          return ptr(ctx, med);
        case medianc::REF:
          return ref(ctx, med);
        case medianc::TUPLE:
          return tup(ctx,med);
        default:
          throw std::runtime_error(
              "type_deduction Invalid or unsupported type");
        }
        std::unreachable();
      },
      [&ctx](final_t &fin) -> sptr<type_t> {
        switch (fin->type()) {
        case tokc::BUILTIN_PTR:
          return std::make_shared<type_t>(type_s::optr_t{});
        case tokc::TYPE_INT:
          return std::make_shared<type_t>(type_s::sint_t{ctx.toks.str(fin)});
        case tokc::TYPE_UINT:
          return std::make_shared<type_t>(type_s::uint_t{ctx.toks.str(fin)});
        case tokc::TYPE_FLOAT:
          return std::make_shared<type_t>(type_s::float_t{ctx.toks.str(fin)});
        case tokc::TYPE_BOOLEAN:
          return std::make_shared<type_t>(type_s::uint_t{8});
        default:
          return nullptr;
          break;
        }
      },
      [&ctx](auto &val) -> sptr<type_t> {
        std::unreachable();
        return nullptr;
      });
  // return std::monostate{};
}

auto var_decl(context_t &ctx, median_t med) -> void {
  auto ch = med.children();
  cursor_helper_t cursor(ch);

  auto name_node = cursor.extract<final_t, tokc::ID>();
  auto type_node = cursor.extract<median_t, medianc::TYPE>();
  auto value_node = cursor.extract<median_t, medianc::VALUE>();

  // this will never fail because the grammar makes sure that they are false
  // i will keep this here for now though just to make sure i traverse properly
  // the array/tree
  if constexpr (true) {
    if (!name_node.has_value())
      throw std::runtime_error(
          "can't have a variable declaration without a name");
    if (!type_node.has_value() && !value_node.has_value())
      throw std::runtime_error(
          "can't have a variable declaration without a type and a "
          "value, one of them must exist");
  }

  const auto name_str = ctx.toks.str(name_node.value());
  auto [res, path, symbol] = locale_t::local_lookup(ctx.loc, name_str);
  if (res)
    throw std::runtime_error("symbol already exists " + std::string(name_str));

  // create the var_decl_t
  auto type_val = [&] -> sptr<type_t> {
    if (!type_node)
      return nullptr;                    // no type
    else if (type_node->type() == medianc::CHAIN) // this is for later
      return std::make_shared<type_t>(type_s::unresoved_t{type_node.value()});

    return type_deduction(ctx, type_node.value()); // resolve the literal type
  }();

  auto ptr =
      std::make_shared<decl_t>(decl_s::var_decl_t{type_val, nullptr},
                               name_str, ctx.increment());
  // insert symbol
  ctx.loc->try_insert(name_str, ptr);
}

auto type_decl(context_t &ctx, median_t med) -> void {

  auto ch = med.children();
  cursor_helper_t cursor(ch);

  auto name_node = cursor.extract<final_t, tokc::ID>();
  auto type_node = cursor.extract<median_t, medianc::TYPE>();
  if constexpr (true) {
    if (!name_node)
      throw std::runtime_error("can't have a type declaration without a name");
    if (!type_node)
      throw std::runtime_error("can't have a type declaration without a type");
  }

  const auto name_str = ctx.toks.str(name_node.value());
  auto [res, path, symbol] = locale_t::local_lookup(ctx.loc, name_str);
  if (res)
    throw std::runtime_error("symbol already exists " + std::string(name_str));

  auto type_val = type_deduction(ctx, type_node.value());

  auto ptr = std::make_shared<decl_t>(decl_s::type_decl_t{type_val}, name_str,
                                      ctx.increment());

  auto insert = ctx.loc->try_insert(name_str, ptr);
}

auto scope_decl(context_t &ctx, median_t med) -> void {

  auto ch = med.children();
  cursor_helper_t cursor(ch);

  auto name_node = cursor.extract<final_t, tokc::ID>();
  auto body_node = cursor.extract<median_t, medianc::BODY>();

  // these are enforced by the gramar
  if constexpr (true) {
    if (!name_node)
      throw std::runtime_error("can't have a scope declaration without a name");
    if (!body_node)
      throw std::runtime_error("can't have a scope declaration without a body");
  }

  const auto name_str = ctx.toks.str(name_node.value());
  auto [res, path, symbol] = locale_t::local_lookup(ctx.loc, name_str);
  ssptr<decl_s::scope_decl_t, decl_t> ptr = {nullptr};
  if (res) {
    ptr = symbol;
  } else {
    auto new_loc = locale_t::make_child(ctx.loc);
    auto name_str = ctx.toks.str(name_node.value());
    ptr = std::make_shared<decl_t>(decl_s::scope_decl_t{new_loc}, name_str,
                                   ctx.increment());
    ctx.loc->try_insert(name_str, ptr.ptr());
  }

  // change the locale to the new one
  auto old_loc = ctx.loc;
  ctx.loc = std::get<decl_s::scope_decl_t>(*ptr.ptr()).loc;
  // analyse the body
  consume_list<stmt>(ctx, body_node.value());
  // change the locale back to the parent
  ctx.loc = old_loc;
}

// how should this work?
//  protal to a different file? proccess? thread? #include?
// maybe this step should't be here
// maybe move this at the lexer?
auto import(context_t &ctx, median_t med) -> void {
  auto cursor = cursor_helper_t{med.children()};
  auto file_node = cursor.extract<final_t, tokc::STRLIT>();

  if (!file_node)
    throw std::runtime_error("Import must have a valid file path");

  auto file_path = ctx.toks.str(file_node.value());
}

auto forloop(context_t &ctx, median_t med) -> void {
}

auto expr(context_t &ctx, median_t med) -> void {
  // what do I even do here
}

auto stmt(context_t &ctx, median_t stmt_node) -> void {
  auto cursor = cursor_helper_t(stmt_node.children());

  auto attributes = cursor.extract<median_t, medianc::ATTRIBUTES>();
  auto med = cursor.extract<median_t>();
  if (attributes) {
    // attribute code
  }
  // this is guranteed by the grammar
  if (!med) {
    throw std::runtime_error(
        "A stmt should always have a median value attached to it");
  }
  auto val = med.value();
  switch (val.type()) {
  case medianc::DECL:
    var_decl(ctx, val);
    break;
  case medianc::TYPE_DECL:
    type_decl(ctx, val);
    break;
  case medianc::SCOPE_DECL:
    scope_decl(ctx, val);
    break;
  case medianc::FOR:
    forloop(ctx, val);
    break;
  case medianc::IMPORT:
    import(ctx, val);
    break;
  case medianc::EXPR: // this holds a lot of things in it, if, while loops,
                      // numbers ,functions, blocks, etc...
    expr(ctx, val);
    break;
  default:
    throw std::runtime_error("Unhandled statement type in stmt()");
  }
}

auto file(context_t &ctx, median_t med) -> void {
  consume_list<stmt>(ctx, med);
}

auto entry(token_buffer_t &toks, parser::node_buffer_t &nodes) -> auto {
  context_t ctx = {toks, 0, std::make_shared<locale_t>(locale_t{nullptr, {}})};
  auto file_node = nodes.begin()->as_median();
  auto file_node_end = nodes.end()-1;
  file(ctx, file_node);
  return ctx.loc;
}

auto visit_decl(sptr<decl_t> symbol, const size_t depth = 0) -> void;
auto visit_type(sptr<type_t> type_symbol, const size_t depth) -> void {
  const auto space = std::string(depth * 3, ' ');
  std::print("{}Type: ", space);
  if (type_symbol) {
    ovisit(
        *type_symbol,
        [depth](type_s::tup_t &val) {
          std::println("Tuple");
          for (auto &elm : val.types)
            visit_type(elm, depth + 1);
        },
        [depth](type_s::record_t &val) {
          std::println("Record");
          for (auto &elm : val.loc->internals.table)
            visit_decl(elm.second, depth + 1);
        },
        [depth](type_s::indirection_t &val) {
          std::print("Indirection ");
          ovisit(
              val, [](type_s::optr_t &val) {
                std::println("Opaque ptr");
              },
              [depth](type_s::ptr_t &val) {
                std::println("Ptr");
                visit_type(val.type, depth + 1);
              },
              [depth](type_s::ref_t &val) {
                std::println("Ref");
                visit_type(val.type, depth + 1);
              });
        },
        [](type_s::unresoved_t &val) {
          std::println("Unresolved type, wait for resolution");
        },
        [](type_s::numeric_t &numval) {
          std::print("Numeric: ");
          ovisit(
              numval,
              [&](type_s::float_t &val) {
                std::println("Float {} ", val.size);
              },
              [&](type_s::sint_t &val) { std::println("Sint {} ", val.size); },
              [&](type_s::uint_t &val) { std::println("Uint {} ", val.size); },
              [&](type_s::boolean_t &val) {
                std::println("Boolean {} ", val.size);
              });
        },
        [&](const auto &val) { std::println("Unknown type"); });
  } else {
    std::println("No type");
  }
}

auto visit_decl(sptr<decl_t> symbol, const size_t depth) -> void {
  const auto space = std::string(depth * 3, ' ');
  std::print("{}", space);
  ovisit(
      *symbol,
      [&](const decl_s::var_decl_t &val) {
        std::println("Var decl: {}", symbol->name);
        visit_type(val.type, depth + 1);
      },
      [&](const decl_s::type_decl_t &val) {
        std::println("Type decl: {}", symbol->name);
        visit_type(val.type, depth + 1);
      },
      [&](const decl_s::scope_decl_t &val) {
        std::println("Scope decl: {}", symbol->name);
        for (auto &elm : val.loc->internals.table)
          visit_decl(elm.second, depth + 1);
      },
      [&](const auto &val) { std::println("Symbol: {}", symbol->name); });
}
} // namespace symbols

namespace resolve {}
// analyse -> (
// decl -> get name, median_t for type, median_t for value
// expr -> unresolved -> save a median_t? unless it is something literal like
// @fn or @rec... then go deeper and continiue the cycle type deduction -> save
// a median_t? unless literal or builtin
// )
//
// resolve -> go down the locale_t tree -> ??? -> profit
} // namespace semantics
