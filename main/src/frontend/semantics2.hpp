#pragma once
//wrap anything that needs a callback to it's own function
//make a new raw sub-namespace for the make functions that already exist
//make new functions that provide higher level functionality
//????
//PROFIT

#include "../become.hpp"
#include "../nicknames.hpp"
#include "../table.hpp"
#include "./cursor_helper.hpp"
#include "./parser.hpp"

#include <boost/algorithm/string.hpp>
#include <boost/container/flat_map.hpp>
#include <boost/container/flat_set.hpp>
#include <boost/core/typeinfo.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <functional>
#include <list>
#include <iostream>
#include <mutex>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>

#define GENERATE_ACCESSOR(Type, VariantName)                                   \
  sptr<Type> as_##Type() {                                                     \
    if (std::holds_alternative<Type>(as_var()))                                \
      return &std::get<Type>(as_var());                                        \
    return {nullptr};                                                          \
  }                                                                            \
  sptr<const Type> as_##Type() const {                                         \
    if (std::holds_alternative<Type>(as_var()))                                \
      return &std::get<Type>(as_var());                                        \
    return {nullptr};                                                          \
  }                                                                            \
  bool is_##Type() const { return std::holds_alternative<Type>(as_var()); }

#define GENERATE_ACCESSOR_WRAPPER(r, var_name, type)                           \
  GENERATE_ACCESSOR(type, var_name)

#define GENERATE_ACCESSORS(VariantName, ...)                                   \
  BOOST_PP_SEQ_FOR_EACH(GENERATE_ACCESSOR_WRAPPER, VariantName,                \
                        BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__))

// Master macro
#define VAR(name, ...)                                                         \
  using name##_var = var<__VA_ARGS__>;                                         \
  struct name##_t : public name##_var {                                        \
    using name##_var::variant;                                                 \
    using name##_var::operator=;                                               \
    consteval auto self_sizeof() { return sizeof(name##_var); }                \
    constexpr auto &as_var() { return static_cast<name##_var &>(*this); }      \
    const constexpr auto &as_var() const {                                     \
      return static_cast<const name##_var &>(*this);                           \
    }                                                                          \
    template <typename T> constexpr auto &as() {                               \
      return std::get<T>(as_var());                                            \
    }                                                                          \
    GENERATE_ACCESSORS(var, __VA_ARGS__);                                      \
  };

#define ESTRUCT(name, var, members)                                            \
  struct name : public var {                                                   \
    using internal = var;                                                      \
    using internal::variant;                                                   \
    members;                                                                   \
    template <typename T> name &operator=(T &&rhs) {                           \
      var::operator=(std::forward<T>(rhs));                                    \
      return *this;                                                            \
    }                                                                          \
    template <typename T> name &operator=(var rhs) {                           \
      var::operator=(std::forward<T>(rhs));                                    \
      return *this;                                                            \
    }                                                                          \
  }

#define USTRUCT(name, var)                                                     \
  struct name : public var {                                                   \
    using internal = var;                                                      \
    using internal::variant;                                                   \
    template <typename T> name &operator=(T &&rhs) {                           \
      var::operator=(std::forward<T>(rhs));                                    \
      return *this;                                                            \
    }                                                                          \
    template <typename T> name &operator=(var rhs) {                           \
      var::operator=(std::forward<T>(rhs));                                    \
      return *this;                                                            \
    }                                                                          \
  }

using node_t = grammar::node_t::external_node;
using span_t = grammar::node_t::median_proxy_t<false>::span_t;
using cursor_t = span_t::iterator;
using median_t = grammar::node_t::median_proxy_t<false>;
using final_t = grammar::node_t::final_t;
using err_t = grammar::node_t::err_t;

namespace semantics {
struct defered_action_t {
  using action_fn = std::function<void()>;
  action_fn fn;
  auto operator()() const { return fn(); }
};
struct defered_actions_t {
  // invalid pointers must not happen since we might add during iteration so we
  // must use a linked list
  std::list<defered_action_t> actions;

  auto operator()() {
    for (auto &elm : actions) {
      elm();
    }
  }
  void schedule(const defered_action_t::action_fn action) {
    actions.emplace_back(action);
  }
};
struct context_t;

struct locale_t;

namespace util {
struct frame_t;
struct fnsig_t;
} // namespace util


namespace type_s {
struct callable_t;
}
struct type_t;

namespace decl_s {
struct var_decl_t;
}
struct decl_t;

namespace expr_s {}
struct expr_t;
// struct exprs_t;
namespace stmt_s {}
struct stmt_t;
struct stmts_t;

using locale_ptr = sptr<locale_t>;
using decl_ptr = sptr<decl_t>;
using type_ptr = sptr<type_t>;
using stmt_ptr = sptr<stmt_t>;
using stmts_ptr = sptr<stmts_t>;
using expr_ptr = sptr<expr_t>;
// using expr_ptr = sptr<exprs_t>;

namespace util {
struct frame_t {
  locale_ptr locale;
  stmts_ptr stmts;
  frame_t(locale_ptr l, stmts_ptr s) : locale(l), stmts(s) {}
};

struct fnsig_t {
  locale_ptr locale;
  list<svar_ptr<decl_t, decl_s::var_decl_t>> args;
  type_ptr ret;
};

struct mutability_t {
  enum e : int8_t { IMMUTABLE, MUTABLE };

  [[nodiscard]] static auto str(const e val) {
    switch (val) {
    case IMMUTABLE:
      return "IMMUTABLE";
    case MUTABLE:
      return "MUTABLE";
    }
  }
  [[nodiscard]] static auto str(const mutability_t &val) {
    if (!val.has_mutability())
      return "NO MUTABILITY MODIFIER";
    else
      return str(val.mutability());
  }

  opt<e> mut;
  mutability_t() : mut(std::nullopt) {}
  mutability_t(std::nullopt_t) : mut(std::nullopt) {}
  mutability_t(const opt<e> m) : mut(m) {}
  mutability_t(const bool m) : mut(static_cast<e>(m)) {}

  [[nodiscard]] bool has_mutability() const { return mut.has_value(); }
  [[nodiscard]] e mutability() const { return mut.value_or(MUTABLE); }
  [[nodiscard]] operator bool() const { return has_mutability(); }

  [[nodiscard]] bool operator==(const mutability_t &other) const {
    return mut == other.mut;
  }
  [[nodiscard]] bool operator!=(const mutability_t &other) const {
    return !(*this == other);
  }
};
} // namespace util

namespace decl_s {
struct var_decl_t {
  type_ptr type;
  expr_ptr init_expr;
};

struct type_decl_t {
  type_ptr type;
};

struct scope_decl_t {
  util::frame_t frame;
};

struct fn_decl_t {
  sptr<util::fnsig_t> sig;
  expr_ptr body;
};

struct unresolved_t {
  size_t index;
};
VAR(var, empty_t, unresolved_t, var_decl_t, type_decl_t, scope_decl_t, fn_decl_t);
} // namespace decl_s

namespace stmt_s {

struct become_t {
  expr_ptr expr;
};
struct return_t {
  expr_ptr expr;
};
struct break_t {
  expr_ptr expr;
};
struct import_t {
  std::string_view file;
};

struct for_t {
  decl_ptr decl;
  expr_ptr ctrl_expr;
  expr_ptr iteration_expr;
};
struct while_t {
  expr_ptr ctrl_expr;
};
struct unreachable_t {};
VAR(var, empty_t, decl_ptr, expr_ptr, import_t, for_t, while_t, unreachable_t,
    become_t, return_t, break_t);
} // namespace stmt_s

namespace type_s {
namespace primitive_s {
struct void_t {};

namespace number_s {
struct float_t {};
struct sint_t {};
struct uint_t {};
struct bool_t {};
VAR(var, float_t, sint_t, uint_t, bool_t);
} // namespace number_s
ESTRUCT(number_t, number_s::var_t, size_t bitsize;);

struct strlit_t {};
VAR(var, number_t, void_t, strlit_t);
} // namespace primitive_s
USTRUCT(primitive_t, primitive_s::var_t);

namespace indirection_s {
struct ptr_t {
  type_ptr type;
  util::mutability_t::e mut;
};
// struct mptr_t {
//   type_ptr type;
// };
// struct iptr_t {
//   type_ptr type;
// };
struct optr_t {};

struct array_t {
  expr_ptr length;
  type_ptr type;
};
VAR(var, ptr_t, optr_t, array_t);
} // namespace indirection_s
USTRUCT(indirection_t, indirection_s::var_t);

namespace aggregate_s {
struct rec_data_t{
  locale_ptr locale;
  std::vector<decl_ptr> members;
  boost::container::flat_map<std::string_view, size_t> name2index;
};
struct rec_t {
  sptr<rec_data_t> data;
  locale_ptr &locale() { return data->locale; }
  const locale_ptr &locale() const { return data->locale; }

  std::vector<decl_ptr> &members() { return data->members; }
  const std::vector<decl_ptr> &members() const { return data->members; }

  auto &name2index() { return data->name2index; }
  const auto &name2index() const { return data->name2index; }

  // Optional deref operators
  rec_data_t &operator*() { return *data; }
  const rec_data_t &operator*() const { return *data; }
  
  rec_data_t *operator->() { return data.get_ptr(); }
  const rec_data_t *operator->() const { return data.get_ptr(); }

  // Check if initialized
  explicit operator bool() const { return bool(data); }
  // locale_ptr locale;
  // std::vector<decl_ptr> members;
  // boost::container::flat_map<std::string_view, size_t> name2index;
};

struct tup_t {
  std::vector<type_ptr> members;
};
VAR(var, rec_t, tup_t);
} // namespace aggregate_s
USTRUCT(aggregate_t, aggregate_s::var_t);

struct fn_type_t {
  std::vector<type_ptr> args;
  type_ptr ret;
};
struct fn_template_t {
  sptr<util::fnsig_t> sig;
};

struct typeref_t {
  type_ptr ref;
};
struct infered_t {};

struct unresolved_t {
  size_t index;
};

VAR(var, empty_t, unresolved_t, primitive_t, typeref_t, indirection_t,
    aggregate_t, fn_type_t, fn_template_t, infered_t);
} // namespace type_s

namespace expr_s {
#include "operator.hpp"

struct operator_t;

struct operation_base {
  using e = op_operation_e;
  e type;
  const auto &meta() const { return op_table.at(static_cast<size_t>(type)); }
};

struct uop_t : operation_base {
  sptr<expr_t> operand;

  struct as_payload_t {
    type_ptr type;
  };

  union payload_t {
    empty_t empty;
    as_payload_t as_type;
  } payload;

  const auto as_payload() {
    if (type == operation_base::e::AS) {
      return payload.as_type;
    } else {
      throw std::runtime_error(
          "Operator is not an as operator so we can't exctract the as payload");
    }
  }
};

struct bop_t : operation_base {
  sptr<expr_t> lhs;
  sptr<expr_t> rhs;
};

VAR(operator_var, uop_t, bop_t);
ESTRUCT(operator_t, operator_var_t,
      const auto meta() const {
      auto val = this->as_var();
      return ovisit(val,
                    [](const auto &val) -> const auto & { return val.meta(); });
    }
);

namespace operand_s {
struct number_t {
  struct float_t {};
  struct int_t {};
  using var = var<float_t, int_t>;

  var val;
  std::string_view str;
};

struct bool_t {
  bool val;
};

struct result_t {
  expr_ptr val;
};

struct block_t {
  util::frame_t frame;
};

struct complit_t {
  type_ptr type;
  list<expr_ptr> init_vals;
};

namespace chain_s {

// inside only
struct deref_t {
  type_ptr type;
};
// inside only
struct address_t {
  type_ptr type;
};

// inside only
struct index_access_t {
  expr_ptr index;
  type_ptr type;
};

// inside only
struct call_t {
  std::vector<expr_ptr> args;
};

// inside only
struct field_access_t {
  svar_ptr<decl_t, decl_s::var_decl_t> val;
};
// both
struct fn_access_t {
  svar_ptr<decl_t, decl_s::fn_decl_t> val;
};
// intro only
struct var_access_t {
  svar_ptr<decl_t, decl_s::var_decl_t> val;
};
VAR(var, deref_t, index_access_t, var_access_t, fn_access_t, field_access_t,
    call_t);
} // namespace chain_s
USTRUCT(chain_elm_t, chain_s::var_t);
struct chain_t {
  std::vector<chain_elm_t> chain;
};

struct pipe_t {
  expr_ptr lhs;
  opt<chain_t> chain;
};

struct sizeof_t {
  var<type_ptr, expr_ptr> val;
};

struct if_t {
  struct if_link_t {
    expr_ptr ctrl_expr;
    stmts_ptr body;
    bool is_else() { return ctrl_expr.is_null(); }
  };
  // we could change this to a static array
  list<if_link_t> ifs;
};
struct fn_lit_t {
  svar_ptr<type_t, type_s::fn_template_t> sig;
  sptr<stmts_t> body;
};


VAR(var, if_t, complit_t, fn_lit_t, pipe_t, sizeof_t, number_t, result_t, block_t, chain_t);
} // namespace operand_s

USTRUCT(operand_t, operand_s::var_t);
VAR(var, empty_t, operator_t, operand_t);
} // namespace expr_s

//struct decl_t
ESTRUCT(decl_t, decl_s::var_t, std::string_view name; size_t index;);
ESTRUCT(type_t, type_s::var_t, util::mutability_t mut);
ESTRUCT(expr_t, expr_s::var_t, type_ptr type;);

// struct exprs_t {
//   expr_ptr expr;
//   type_ptr type;
// };

USTRUCT(stmt_t, stmt_s::var_t);
struct stmts_t {
  std::vector<sptr<stmt_t>> stmts;
};

struct locale_t {
  using val_t = decl_ptr;
  using entry_t = val_t;
  using map_entry_t = std::tuple<std::string_view, val_t>;

  struct path_t {
    using val_t = locale_ptr;
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

  struct insert_t {
    const bool inserted;
    const std::string_view name;
    val_t entry;
  };

  struct lookup_t {
    const bool found;
    const path_t path;
    entry_t symbol;
    operator bool() const { return found; }
  };

  template <typename SPECIALISED_TYPE> struct slookup_t {
    const bool found;
    const path_t path;
    svar_ptr<decl_t, SPECIALISED_TYPE> symbol;
    operator bool() const { return found; }
  };

  sptr<locale_t> parent_;
  // list<sptr<locale_t>> children_;

  // general
  static sptr<locale_t> make(allocator_t &allocator) {
    auto ptr = allocator.alloc<locale_t>(locale_t{{}});
    // self->children_.push_back(ptr);
    return ptr;
  }
  static sptr<locale_t> make_child(allocator_t &allocator,
                                   sptr<locale_t> self) {
    auto ptr = allocator.alloc<locale_t>(locale_t{self, {}});
    // self->children_.push_back(ptr);
    return ptr;
  }

  sptr<locale_t> parent() { return parent_; }

  // implementation specific
  struct internal {
    boost::container::flat_map<std::string_view, entry_t> table = {};
    mutable std::mutex mutex = {};

    internal() = default;

    internal(const internal &other) : table(other.table) {}

    // Custom copy assignment operator
    internal &operator=(const internal &other) {
      if (this != &other) {
        table = other.table;
      }
      return *this;
    }
    static lookup_t ancestor_lookup(sptr<locale_t> self, path_t &path,
                                    const std::string_view name) {
      self->internals.mutex.lock();
      auto val = lookup<false>(self, path, name);
      self->internals.mutex.unlock();
      return val;
    }
    static lookup_t local_lookup(sptr<locale_t> self, path_t &path,
                                 const std::string_view name) {
      self->internals.mutex.lock();
      auto val = lookup<true>(self, path, name);
      self->internals.mutex.unlock();
      return val;
    }
    template <bool is_local_search>
    static lookup_t lookup(sptr<locale_t> &self, path_t &path,
                           const std::string_view name) {
      path.push(self);
      auto it = self->internals.table.find(name);

      if (it != self->internals.table.end())
        return {true, path, it->second};

      if constexpr (!is_local_search)
        if (self->parent_) {
          return ancestor_lookup(self->parent_, path, name);
        }
      return {false, path, {nullptr}};
    }

    insert_t try_insert(std::string_view name, entry_t &&val) {
      mutex.lock();
      auto res = table.try_emplace(name, std::forward<entry_t>(val));
      mutex.unlock();
      return {res.second, name, res.first->second};
    }
    insert_t try_insert(std::string_view name, entry_t &val) {
      mutex.lock();
      auto res = table.try_emplace(name, val);
      mutex.unlock();
      return {res.second, name, res.first->second};
    }
  } internals;

  insert_t try_insert(const std::string name, entry_t &&val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(const std::string_view name, entry_t &&val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(const std::string name, entry_t &val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }
  insert_t try_insert(const std::string_view name, entry_t &val) {
    return internals.try_insert(name, std::forward<entry_t>(val));
  }

  static lookup_t ancestor_lookup(sptr<locale_t> self,
                                  const std::string_view name) {
    path_t path;
    return internal::lookup<false>(self, path, name);
  }
  static lookup_t local_lookup(sptr<locale_t> self,
                               const std::string_view name) {
    path_t path;
    return internal::lookup<true>(self, path, name);
  }

  template <typename VAR_TYPE, auto lookup_fn = local_lookup>
  static auto expect_lookup(sptr<locale_t> self, const std::string_view name)
      -> slookup_t<VAR_TYPE> {
    using ret_type = slookup_t<VAR_TYPE>;

    static_assert(lookup_fn == locale_t::local_lookup ||
                      lookup_fn == locale_t::ancestor_lookup,
                  "lookup_fn must be local_lookup or ancestor_lookup");

    const lookup_t lookup = lookup_fn(self, name);

    if (!lookup.found || !std::holds_alternative<VAR_TYPE>(*lookup.symbol))
      return ret_type{false, std::move(lookup.path), sptr<decl_t>{}};
    return ret_type{true, std::move(lookup.path), lookup.symbol};
  }
};
} // namespace semantics

namespace semantics {
struct unresolved_decl_t {
  locale_ptr locale;
  decl_ptr ptr;
  type_ptr type;
  opt<median_t> expr_val;
};

struct unresolved_type_t {
  locale_ptr locale;
  type_ptr ptr;
  median_t chain;
};

struct ctx_t {
  struct external_ctx {
    const token_buffer_t &toks;
    const std::map<size_t, size_t> &toks_symetrical_map;
    allocator_t &allocator;
  } ectx;

  auto &allocator() { return ectx.allocator; }
  const auto &toks() const { return ectx.toks; }
  const auto &symetrical_map() const { return ectx.toks_symetrical_map; }

  std::mutex mut; // not needed?

  size_t cindex;
  std::vector<unresolved_decl_t> unresolved_decl_list;
  std::vector<unresolved_type_t> unresolved_type_list;
  defered_actions_t actions;

  boost::container::flat_set<uintptr_t> known_non_recursive_types;

  operator allocator_t &() { return ectx.allocator; }
  operator const token_buffer_t &() { return ectx.toks; }
  operator defered_actions_t &() { return actions; }

  ctx_t(const token_buffer_t &t, const std::map<size_t, size_t> &tsm,
        allocator_t &a)
      : ectx(t, tsm, a), mut(), cindex(0), actions({}),
        known_non_recursive_types({}) {}

  ctx_t(const context_t &) = delete;
  ctx_t &operator=(const context_t &) = delete;

  size_t operator++() { return cindex++; }

  template <typename T, typename... Args> auto alloc(Args &&...val) -> sptr<T> {
    return ::alloc<T, Args...>(ectx.allocator, std::forward<Args>(val)...);
  }
  template <typename T> auto alloc(T &&val) -> sptr<T> {
    return ::alloc<T>(ectx.allocator, val);
  }
  template <typename T> auto alloc(T &val) -> sptr<T> {
    return ::alloc<T>(ectx.allocator, val);
  }

  template <typename ROOT, typename... PATH>
  auto salloc(ROOT &val) -> svar_ptr<ROOT, PATH...> {
    auto base_ptr = alloc(val);
    return svar_ptr<ROOT, PATH...>{base_ptr};
  }
  template <typename ROOT, typename... PATH>
  auto salloc(ROOT &&val) -> svar_ptr<ROOT, PATH...> {
    auto base_ptr = alloc(std::move(val));
    return svar_ptr<ROOT, PATH...>{base_ptr};
  }
};
} // namespace semantics
