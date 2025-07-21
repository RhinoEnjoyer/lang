#include "semantics2.hpp"
#include <atomic>
#include <boost/algorithm/string/case_conv.hpp>
#include <cstdint>
#include <source_location>
#include <print>
#include <utility>

// I need standardise the constructors

namespace semantics {
namespace node2ast {
namespace defer {
void resolve_chain_action(ctx_t &ctx, type_s::unresolved_t info);
defered_action_t::action_fn resolve_chain(ctx_t &ctx,
                                          type_s::unresolved_t info);

void resolve_decl_action(ctx_t &ctx, const decl_s::unresolved_t info);
defered_action_t::action_fn resolve_decl(ctx_t &ctx, decl_s::unresolved_t info);
} // namespace defer
} // namespace node2ast

namespace make {
namespace raw {

size_t type_numeric2bitsize(std::string_view str) {
  const auto size = std::stoull(std::string(str.substr(1)));
  return size;
}

auto numeric2bitsize(ctx_t &ctx, final_t fin) {
  return type_numeric2bitsize(ctx.toks().str(fin));
}

template <typename T> sptr<T> ptr(allocator_t &ctx, T &val) {
  return ctx.alloc<T>(val);
}
template <typename T> sptr<T> ptr(allocator_t&ctx, T &&val) {
  return ctx.alloc<T>(std::move(val));
}
template <typename T> sptr<T> ptr(allocator_t&ctx) { return ctx.alloc<T>(); }

semantics::type_t type(type_s::var_t &&val = empty_t{},
                       util::mutability_t mut = std::nullopt) {
  return {std::move(val), mut};
}

util::frame_t frame(sptr<locale_t> locale, sptr<stmts_t> stmts) {
  return {locale, stmts};
}
type_s::typeref_t typeref(sptr<type_t> type) { return {type}; }

util::fnsig_t fnsig(sptr<locale_t> locale,
                    std::vector<svar_ptr<decl_t, decl_s::var_decl_t>> &args,
                    sptr<type_t> ret) {
  return {locale, args, ret};
}
util::fnsig_t fnsig(sptr<locale_t> locale,
                    std::vector<svar_ptr<decl_t, decl_s::var_decl_t>> &&args,
                    sptr<type_t> ret) {
  return {locale, args, ret};
}

decl_s::fn_decl_t fn_decl(sptr<util::fnsig_t> sig) {
  return decl_s::fn_decl_t{sig};
}
decl_s::var_decl_t var_decl(type_ptr type, expr_ptr init_expr) {
  return {type, init_expr};
}
decl_s::type_decl_t type_decl(sptr<type_t> type) { return {type}; }
decl_s::scope_decl_t scope_decl(util::frame_t &&frame) {return {std::move(frame)};}

decl_t decl(std::string_view name, size_t index) {
  return {empty_t{}, name, index};
}
decl_t decl(std::string_view name, size_t index, decl_s::var_t &&val) {
  return {val, name, index};
}
decl_t decl(std::string_view name, size_t index, decl_s::var_t &val) {
  return {val, name, index};
}

stmt_t stmt(stmt_s::var_t &val) { return stmt_t{val}; }
stmt_t stmt(stmt_s::var_t &&val) { return stmt_t{std::move(val)}; }
stmts_t stmts() { return stmts_t{}; }
stmts_t stmts(std::vector<stmt_ptr> &vec) { return stmts_t{vec}; }
stmts_t stmts(std::vector<stmt_ptr> &&vec) { return stmts_t{std::move(vec)}; }

semantics::type_ptr typeref_ptr(allocator_t &ctx, sptr<type_t> ptr) {
  return make::raw::ptr(ctx, make::raw::type(type_s::typeref_t{ptr}));
}

semantics::type_ptr tptr(allocator_t &ctx, type_t val) {
  return make::raw::ptr(ctx, make::raw::type(std::move(val)));
}
auto tprimitive(semantics::type_s::primitive_s::var_t val) {
  return semantics::type_s::primitive_t{val};
}
auto tvoid() { return semantics::type_s::primitive_s::void_t{}; }
auto tfloat() {
  return semantics::type_s::primitive_s::number_s::float_t{};
}
auto tsint() { return semantics::type_s::primitive_s::number_s::sint_t{}; }
auto tuint() { return semantics::type_s::primitive_s::number_s::uint_t{}; }

auto type_boolean() {
  return semantics::type_s::primitive_s::number_s::bool_t{};
}

auto tnumber(semantics::type_s::primitive_s::number_s::var_t val,
                 const size_t size) {
  return semantics::type_s::primitive_s::number_t{val, size};
}

auto tindirection(type_s::indirection_s::var_t val) {
  return semantics::type_s::indirection_t{val};
}

auto tptr(const util::mutability_t::e mut, sptr<type_t> type) {
  return semantics::type_s::indirection_s::ptr_t{type, mut};
}

auto toptr() { return semantics::type_s::indirection_s::optr_t{}; }
auto tarray(semantics::type_ptr type, expr_ptr length) {
  return semantics::type_s::indirection_s::array_t{length, type};
}

auto taggregate(type_s::aggregate_s::var_t val) {
  return type_s::aggregate_t{val};
}
auto type_tup(std::vector<semantics::type_ptr> &&types) {
  return type_s::aggregate_s::tup_t{std::move(types)};
}
auto ttup(std::vector<semantics::type_ptr> &types) {
  return type_s::aggregate_s::tup_t{types};
}
auto trec_data(locale_ptr locale, std::vector<decl_ptr> &&members) {
  return type_s::aggregate_s::rec_data_t{locale, members};
}
auto trec(ctx_t& ctx, type_s::aggregate_s::rec_data_t&& val) {
  return type_s::aggregate_s::rec_t{ctx.alloc(val)};
}
auto trec(ctx_t &ctx, locale_ptr locale, std::vector<decl_ptr> &&members) {
  return type_s::aggregate_s::rec_t{
      ctx.alloc<type_s::aggregate_s::rec_data_t>(locale, members)};
}
auto tinfered() { return type_s::infered_t{}; }

auto uop(expr_s::operation_base::e op, expr_ptr operand,
         expr_s::uop_t::payload_t payload) {
  return expr_s::uop_t{{op}, operand, payload};
}
auto bop(expr_s::operation_base::e op, expr_ptr lhs, expr_ptr rhs) {
  return expr_s::bop_t{{op}, lhs, rhs};
}
auto as(expr_s::operation_base::e op, expr_s::uop_t::as_payload_t payload,
        expr_ptr operand) {
  return uop(op, operand, expr_s::uop_t::payload_t{.as_type = payload});
}

} // namespace raw
} // namespace make

template <bool localy_indistinct>
auto decl_name_preproc(ctx_t &ctx, locale_ptr locale,
                       const std::string_view name) {
  auto lookup = locale_t::local_lookup(locale, name);
  if (lookup.found) {
    if constexpr (localy_indistinct) {
      return lookup.symbol;
    } else {
      throw std::runtime_error("Local redeclaration error: symbol '" +
                               std::string(name) +
                               "' already exists in the current scope.");
    }
  } else {
    auto ptr =
        make::raw::ptr(ctx, make::raw::decl(name, ctx.cindex, empty_t{}));
    locale->try_insert(name, ptr);
    return ptr;
  }
}

type_ptr typeref_final(svar_ptr<type_t, type_s::typeref_t> ptr);
type_ptr typeref_final(type_s::typeref_t &ptr) {
  auto ref = ptr.ref;
  if (rholds<type_s::typeref_t>(*ref))
    return typeref_final(ptr.ref);
  return ref;
}

type_ptr typeref_final(svar_ptr<type_t, type_s::typeref_t> ptr) {
  auto ref = ptr.as<type_s::typeref_t>().ref;
  if (rholds<type_s::typeref_t>(*ref))
    return typeref_final(ptr.as<type_s::typeref_t>().ref);
  return ref;
}

void register_member(type_s::aggregate_s::rec_t &rec, const size_t index,
                     auto &member) {
  rec->members.push_back(member);
  rec->name2index.emplace(member->name, index);
};

//these assume that they have all been resolved
type_ptr get_type(expr_t &val) { return val.type; }
type_ptr get_type(expr_t &val, ctx_t &ctx) {
  auto type = val.type;
  if (type->is_empty_t()) {
    throw std::runtime_error("Found an expersion with a empty type and this "
                             "throw should be replaced by a resolve function");
  }
  return get_type(val);
}
type_ptr get_type(decl_s::var_decl_t &val) { return val.type; }
type_ptr get_type(decl_s::type_decl_t &val) { return val.type; }
type_ptr get_type(decl_t &val,
                  opt<std::reference_wrapper<ctx_t>> ctx = std::nullopt) {
  using ret = type_ptr;
  return ovisit(
      val, [](decl_s::var_decl_t &val) -> ret { return get_type(val); },
      [](decl_s::type_decl_t &val) -> ret { return get_type(val); },
      [&ctx](decl_s::unresolved_t &val) -> ret {
        if (!ctx)
          return nullptr;
        auto &vctx = ctx.value().get();
        auto info = vctx.unresolved_decl_list.at(val.index);
        node2ast::defer::resolve_decl_action(vctx, val);
        return get_type(*info.ptr, vctx);
      },
      [](auto &val) -> ret { return nullptr; });
}
// type_ptr get_type(decl_ptr val) { return get_type(*val); }

type_ptr get_type(decl_ptr val,
                  opt<std::reference_wrapper<ctx_t>> ctx = std::nullopt) {
  return get_type(*val, ctx);
}

[[nodiscard]] type_ptr get_type(type_ptr val) { return val; }
[[nodiscard]] type_ptr get_type(ctx_t &ctx, type_ptr val) {
  return ovisit(
      *val,
      [&ctx, &val](type_s::unresolved_t &index) -> type_ptr {
        node2ast::defer::resolve_chain_action(ctx, index);
        return val;
      },
      [&val](auto &) -> type_ptr { return val; });
}

template <bool external = false> locale_ptr get_locale(type_t &val) {
  using ret = locale_ptr;
  static auto avisit = [](type_s::aggregate_t &val) -> ret {
    static auto rec = [](type_s::aggregate_s::rec_t &val) -> ret {
      return val->locale;
    };
    static auto tup = [](type_s::aggregate_s::tup_t &) -> ret {
      return nullptr;
    };
    return ovisit(val, rec, tup);
  };

  return ovisit(
      val, [](type_s::aggregate_t &val) -> ret { return avisit(val); },
      [](type_s::fn_template_t &val) -> ret {
        if constexpr (external)
          return nullptr;
        return val.sig->locale;
      },
      [](auto &) -> ret { return nullptr; });
}

locale_ptr get_locale(decl_s::fn_decl_t &val) { return val.sig->locale; }

template <bool external = false> locale_ptr get_locale(decl_t &val) {
  using ret = locale_ptr;
  return ovisit(
      val, [](decl_s::scope_decl_t &val) -> ret { return val.frame.locale; },
      [](decl_s::type_decl_t &val) -> ret {
        return get_locale<external>(*val.type);
      },
      [](decl_s::fn_decl_t &val) -> ret {
        if constexpr (external)
          return nullptr;
        return get_locale(val);
      },
      [](auto &) -> ret { return nullptr; });
}

decl_ptr at(type_s::aggregate_s::rec_t &val, const size_t index) {
  if (index > val->members.size() - 1)
    throw std::runtime_error("Invalid index");
  return val->members.at(index);
}
decl_ptr at(type_s::aggregate_s::rec_t &val, const std::string_view name) {
  if (val->name2index.contains(name)) {
    return val->members.at(val->name2index.at(name));
  } else {
    throw std::runtime_error("Invalid member name");
  }
}
type_ptr at(type_s::aggregate_s::tup_t &val, const size_t index) {
  if (index > val.members().size() - 1)
    throw std::runtime_error("Invalid index");
  return val.members().at(index);
}

type_ptr decay(type_ptr type) {
  if (type->is_typeref_t()) {
    auto ptr = typeref_final(type);
    return decay(ptr);
  }
  if (auto ind = type->as_indirection_t()) {
    return ovisit(
        *ind,
        [](type_s::indirection_s::optr_t &) -> type_ptr {
          return nullptr;
          // throw std::runtime_error("Opaque pointers can't decay");
        },
        [](auto &val) -> type_ptr { return val.type; });
  } else {
    return nullptr;
    // throw std::runtime_error("Can't decay a non indirection type");
  }
}
type_ptr full_decay(type_ptr type) {
  if (auto ind = type->as_indirection_t()) {
    return ovisit(
        *ind,
        [&type](type_s::indirection_s::optr_t &) {
          // can't go further
          return type;
        },
        [](auto &val) { return full_decay(val.type); });
  } else {
    // can't go further
    return type;
  }
}




size_t member_count_builtin(const type_s::aggregate_s::rec_t &val) {
  return val.members().size();
}
size_t member_count_builtin(const type_s::aggregate_s::tup_t &val) {
  return val.members().size();
}
size_t member_count_builtin(const type_s::aggregate_t &val) {
  using namespace type_s::aggregate_s;
  return ovisit(
      val, [](const rec_t &val) { return member_count_builtin(val); },
      [](const tup_t &val) { return member_count_builtin(val); });
}
opt<size_t> member_count_builtin(const type_ptr type) {
  using namespace type_s;
  const auto &val = *type;
  return ovisit(
      val,
      [](const aggregate_t &val) -> opt<size_t> { return member_count_builtin(val); },
      [](const auto &val) -> opt<size_t> { return std::nullopt; });
}
//tood
size_t sizeof_builtin(const type_ptr type) { return 32; }

util::fnsig_t fntemplate2fnsig(ctx_t &ctx, type_s::fn_template_t &val) {
  auto locale = make::raw::ptr(ctx, locale_t{});

  // assign the parrent
  locale->parent_ = val.sig->locale->parent();
  auto ret = val.sig->ret;

  // assign the arguments and add them to the new locale
  std::vector<svar_ptr<decl_t, decl_s::var_decl_t>> args;
  for (auto &elm : val.sig->args) {
    auto decl = make::raw::ptr(ctx, *elm);
    args.emplace_back(decl);
    locale->try_insert(decl->name, decl);
  }

  return make::raw::fnsig(locale, std::move(args), ret);
}

auto type_recursion_check_action(ctx_t &ctx, type_ptr type) {
  struct type_recursion_checker {
  public:
    void type_visitor(type_ptr ptr) {
      auto aggregate_visitor = [this](type_s::aggregate_t &val) {
        using namespace type_s::aggregate_s;

        auto rec_visitor = [this](rec_t &val) {
          for (auto &decl : val->locale->internals.table) {
            auto type = get_type(decl.second, ctx);
            this->type(type);
          }
        };
        auto tup_visitor = [this](tup_t &val) {
          for (auto &mem : val.members()) {
            auto type = get_type(ctx, mem);
            this->type(type);
          }
        };
        ovisit(val, rec_visitor, tup_visitor);
      };

      auto unresolved_visitor = [this](type_s::unresolved_t &val) {
        auto info = val;
        auto ptr = ctx.unresolved_type_list.at(info.index).ptr;
        node2ast::defer::resolve_chain_action(ctx, info);
        return this->type_visitor(ptr);
      };
      auto typeref_visitor = [this](type_s::typeref_t &val) {
        return this->type(val.ref);
      };

      auto auto_visitor = [](auto &val) {};

      ovisit(*ptr, aggregate_visitor, typeref_visitor, unresolved_visitor,
             auto_visitor);
    }

    void type(type_ptr ptr) {
      if (is_forbiten(ptr)) {
        for (auto &elm : forbiten_set) {
          type_ptr ptr = type_ptr((type_t *)(elm));
          std::println("ptr: {}", ptr.as_void());
        }
        throw std::runtime_error("Found forbiten pointer");
      }
      insert_forbiten(ptr);
      type_visitor(ptr);
    }

    type_recursion_checker(ctx_t &ctx) : forbiten_set(), ctx(ctx) {}

    boost::container::flat_set<uintptr_t> forbiten_set;
    ctx_t &ctx;

    bool is_forbiten(type_ptr ptr) const {
      return this->forbiten_set.contains(ptr.as_uint());
    }
    void insert_forbiten(type_ptr ptr) {
      this->forbiten_set.insert(ptr.as_uint());
    }
  };

  // I am not that sure about this but it should work
  if (ctx.known_non_recursive_types.contains(type.as_uint()))
    return;

  auto checker = type_recursion_checker{ctx};
  checker.type(type);

  ctx.known_non_recursive_types.insert(checker.forbiten_set.begin(),
                                       checker.forbiten_set.end());
}

auto type_recursion_check(ctx_t &ctx, type_ptr type) {
  return [&ctx, type]() { return type_recursion_check_action(ctx, type); };
}

// FORWARD DEFS
namespace node2ast {
decl_ptr decl_fn(ctx_t &ctx, locale_ptr locale, const median_t &med);
type_ptr type_fn(ctx_t &ctx, locale_ptr locale, const median_t &type_med);
stmts_t stmts_fn(ctx_t &ctx, locale_ptr locale, span_t span);
void stmts_fn(ctx_t &ctx, locale_ptr locale, stmts_t &val, span_t span);
template <auto fn>
decl_ptr decl_spec_fn(ctx_t &ctx, locale_ptr locale, const median_t &med);
type_ptr type_fn(ctx_t &ctx, locale_ptr locale, const median_t &type_med);
void type_med_fn(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                 const median_t &med);
void type_final_fn(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                   const final_t fin);

// void expr_fn(ctx_t &ctx, locale_ptr locale, expr_ptr ptr, const median_t &med);
expr_ptr expr_fn(ctx_t &ctx, locale_ptr locale, span_t span);
expr_ptr expr_fn(ctx_t &ctx, locale_ptr locale, const median_t &med);

//////////
} // namespace node2ast

namespace node2ast {
namespace defer {

auto resolve_val(ctx_t &ctx, type_s::unresolved_t val) {
  resolve_chain_action(ctx, val);
  auto ptr = ctx.unresolved_type_list.at(val.index).ptr;
  return ptr;
}
auto resolve_val(ctx_t &ctx, decl_s::unresolved_t val) {
  resolve_decl_action(ctx, val);
  auto ptr = ctx.unresolved_decl_list.at(val.index).ptr;
  return ptr;
}


namespace chain_resolve_s {

struct precistent_ctx_t {
  semantics::ctx_t &ctx;
  span_t span;

  bool contains(const auto &val) const { return span.contains(val); }
  auto &toks() { return ctx.toks(); }
  auto str(final_t val) const { return ctx.toks().str(val); }
};

using ret = type_ptr;
[[nodiscard]] ret scope(precistent_ctx_t &ctx, decl_s::scope_decl_t &scope,
                        span_t::iterator &it);

[[nodiscard]] ret type_decl(precistent_ctx_t &ctx, locale_ptr locale,
                            decl_s::type_decl_t &val, span_t::iterator &it);

[[nodiscard]] ret record(precistent_ctx_t &ctx, type_s::aggregate_s::rec_t &val,
                         span_t::iterator &it);

//everytime we call this we advance the iterator
template <auto lookup_fn>
[[nodiscard]] auto lookup_must(precistent_ctx_t &ctx, locale_ptr locale,
                               span_t::iterator &it) {
  if (!ctx.contains(it))
    throw std::runtime_error("Oversteped the iterator");

  auto lookup = lookup_fn(locale, ctx.str(it->as_final()));

  if (!lookup)
    throw std::runtime_error("Did  not find symbol");

  it.advance();
  return lookup;
}

template <auto lookup_fn>
[[nodiscard]] ret intro_path(precistent_ctx_t &ctx, locale_ptr locale,
                              span_t::iterator &it, decl_ptr &symbol) {
  return ovisit(
      *symbol,
      [&](decl_s::type_decl_t &val) -> ret {
        return type_decl(ctx, locale, val, it);
      },
      [&](decl_s::scope_decl_t &val) -> ret { return scope(ctx, val, it); },
      [&](decl_s::unresolved_t &val) -> ret {
        auto ptr = defer::resolve_val(ctx.ctx, val);
        return intro_path<lookup_fn>(ctx, locale, it, ptr);
      },
      [](auto &val) -> ret {
        throw std::runtime_error(
            "Can't extract a type from this type of symbol");
      });
}

[[nodiscard]] ret record_path(precistent_ctx_t &ctx, locale_ptr locale,
                            span_t::iterator &it, decl_ptr decl) {
  return ovisit(
      *decl,
      [&](decl_s::type_decl_t &val) -> ret {
        return type_decl(ctx, locale, val, it);
      },
      [&](decl_s::unresolved_t &val) -> ret {
        auto ptr = resolve_val(ctx.ctx, val);
        return record_path(ctx, locale, it, ptr);
      },
      [](auto &) -> ret {
        throw std::runtime_error("A record can't lead to this type of symbol");
      });
}
ret record(precistent_ctx_t &ctx, type_s::aggregate_s::rec_t &val,
           span_t::iterator &it) {
  auto &locale = val->locale;
  auto lookup = lookup_must<locale_t::local_lookup>(ctx, locale, it);
  return record_path(ctx, locale, it, lookup.symbol);
}

[[nodiscard]] ret type_decl_path(precistent_ctx_t &ctx, type_ptr type,
                                 span_t::iterator &it) {
  return ovisit(
      *type,
      [&ctx, &it](type_s::aggregate_t &val) -> ret {
        return ovisit(
            val,
            [&ctx, &it](type_s::aggregate_s::rec_t &val) -> ret {
              return record(ctx, val, it);
            },
            [](auto &) -> ret {
              throw std::runtime_error(
                  "A aggregate can't lead to this type of symbol");
            });
      },
      [&ctx, &it](type_s::typeref_t &val) -> ret {
        auto type = typeref_final(val);
        return type_decl_path(ctx, type, it);
      },
      [&ctx, &it](type_s::unresolved_t &val) -> ret {
        auto ptr = resolve_val(ctx.ctx, val);
        return type_decl_path(ctx, ptr, it);
      },
      [](auto &) -> ret { throw std::runtime_error("Illegal"); });
}

ret type_decl(precistent_ctx_t &ctx, locale_ptr locale,
              decl_s::type_decl_t &val, span_t::iterator &it) {
  if (!ctx.contains(it))
    return val.type;
  return type_decl_path(ctx, val.type, it);
}

ret scope(precistent_ctx_t &ctx, decl_s::scope_decl_t &scope,
          span_t::iterator &it) {
  if (!ctx.contains(it))
    throw std::runtime_error("Scopes can't be used as types");

  if (!it->is_final())
    throw std::runtime_error("Not allowed");

  auto lookup =
      lookup_must<locale_t::local_lookup>(ctx, scope.frame.locale, it);
  return intro_path<locale_t::local_lookup>(ctx, scope.frame.locale, it, lookup.symbol);
}

[[nodiscard]] ret template_init_list(precistent_ctx_t &ctx, locale_ptr locale,
                                     type_ptr ptr, span_t::iterator &it) {
  throw std::runtime_error("Templates are not supported at this time");
}

ret intro(precistent_ctx_t &ctx, locale_ptr locale, span_t::iterator it) {
  if (!it->is_final())
    throw std::runtime_error("type chain can't begin with a median");
  auto lookup = lookup_must<locale_t::ancestor_lookup>(ctx, locale, it);
  return intro_path<locale_t::local_lookup>(ctx, locale, it, lookup.symbol);
}
} // namespace chain_resolve_s

void resolve_chain_action(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                          span_t chain) {
  if (!rholds<type_s::unresolved_t>(*ptr))
    return;
  auto it = chain.begin();
  chain_resolve_s::precistent_ctx_t val = {ctx, chain};
  *ptr = make::raw::typeref(chain_resolve_s::intro(val, locale, it));
}

void resolve_chain_action(ctx_t &ctx, type_s::unresolved_t index) {
  auto &info = ctx.unresolved_type_list.at(index.index);
  return resolve_chain_action(ctx, info.locale, info.ptr,
                              info.chain.children());
}
defered_action_t::action_fn resolve_chain(ctx_t &ctx,
                                          type_s::unresolved_t info) {
  return [&ctx, info]() { resolve_chain_action(ctx, info); };
}

void resolve_decl_action(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
                         type_ptr type, const opt<median_t> expr_val) {
  if (!ptr->is_unresolved_t())
    return;

  struct spec {
    static void visit(ctx_t &ctx, locale_ptr locale, decl_ptr ptr, semantics::type_ptr tptr, const opt<median_t> value_med) {
      const auto fn_template = [&](type_s::fn_template_t &val) {
        auto fndecl_type =
            make::raw::fn_decl(make::raw::ptr(ctx, fntemplate2fnsig(ctx, val)));
        if (!value_med)
          throw std::runtime_error("Expected to have a function body");

        fndecl_type.body = expr_fn(
            ctx, get_locale(fndecl_type),
            value_med.value().fchild().as_median().expect<medianc::EXPR>());
        *ptr = std::move(fndecl_type);
      };
      const auto fn_type = [&](type_s::fn_type_t &val) {
        //make a function pointer
        // throw std::runtime_error("Problem for future me");
        // auto ptr = make::raw::ptr()
        auto typeptr = make::raw::ptr(
            ctx, make::raw::type(make::raw::tprimitive(make::raw::tnumber(
                                     make::raw::tfloat(), 32)),
                                 tptr->mut));
        *ptr = make::raw::var_decl(
            typeptr, ((value_med) ? expr_fn(ctx, locale,
                                            value_med.value()
                                                .fchild()
                                                .as_median()
                                                .expect<medianc::EXPR>())
                                  : nullptr));
      };
      const auto var_decl = [&](auto &) {
        const auto isinfered = rholds<type_s::infered_t>(*tptr);
        if (isinfered) {
          throw std::runtime_error("For now we do not support infered types on variables");
          // resolve the expresion
          // it's types and get it back to the declaration
        }
        auto type = (isinfered) ? tptr : make::raw::typeref_ptr(ctx, tptr);
        expr_ptr expr = (value_med)? expr_fn(ctx, locale, value_med.value().fchild().as_median().expect<medianc::EXPR>() ) : nullptr;
        // check expresion type or add it for later
        // resolve expresion -> resolve type -> check types
        auto decl_val = make::raw::var_decl(type, expr);
        *ptr = decl_val;
      };
      auto def = [&](auto &val) { return ovisit(val, fn_template, var_decl); };
      ovisit(
          *tptr,
          [&](type_s::typeref_t &) {
            auto type = typeref_final(tptr);
            return def(*type);
          },
          [&](type_s::unresolved_t &val) {
            defer::resolve_chain_action(ctx, val);
            return visit(ctx, locale, ptr, tptr, value_med);
          }, 
          fn_template, 
          var_decl);
    }
  };

  if (auto val = type->as_unresolved_t())
    defer::resolve_chain_action(ctx, *val);
  return spec::visit(ctx, locale, ptr, type, expr_val);
}

void resolve_decl_action(ctx_t &ctx, const decl_s::unresolved_t index) {
  auto &info = ctx.unresolved_decl_list.at(index.index);
  resolve_decl_action(ctx, info.locale, info.ptr, info.type, info.expr_val);
}

defered_action_t::action_fn resolve_decl(ctx_t &ctx, decl_s::unresolved_t info) {
  return [&ctx, info] { resolve_decl_action(ctx, info); };
}

void fn_arg_check_action(decl_ptr ptr) {
  if (const auto var = ptr->as_var_decl_t()) [[likely]] {
    if (!var->init_expr.is_null()) [[unlikely]]
      throw std::runtime_error("Function arguments can't be initialised");
  } else {
    throw std::runtime_error(
        "Functions must only have variable declarations as arguments");
  }
}

defered_action_t::action_fn fn_arg_check(decl_ptr ptr) {
  return [ptr] { fn_arg_check_action(ptr); };
}

void expr_type_resolve(ctx_t &ctx, expr_ptr expr) {}

} // namespace defer
} // namespace node2ast

// TODO

template <bool firstcall = true> 
void mutability_check_action_impl(type_ptr ptr) {
  if constexpr (firstcall) {
    if (!ptr->mut)
      return;

    if (auto tref= ptr->as_typeref_t()) {
      become mutability_check_action_impl<false>(tref->ref);
    }
  }

  static auto indirection_visitor = [](type_s::indirection_t &val) [[clang::preserve_all]] {
    using namespace type_s::indirection_s;
    ovisit(
        val,
        [](ptr_t &val) [[clang::preserve_all]] {
          auto ptr_mut_str = std::string(util::mutability_t::str(val.mut));
          boost::algorithm::to_lower(ptr_mut_str);
          throw std::runtime_error("Illegal use of mutability modifier on an " +
                                   ptr_mut_str + " pointer");
        },
        [](optr_t &) {},
        [](array_t &val)[[clang::preserve_all]] {
          throw std::runtime_error("Illegal use of mutability modifier on array type");
        },
        [](auto &)[[clang::preserve_all]] {
          throw std::runtime_error("Unsupported indirection type or passed the "
                                   "wrong type to this visitor.");
        });
  };
  static auto fn_type_visitor = [](type_s::fn_type_t &val) {
    // it is fine since those are for function pointers so I should check if
    // this come from a function pointer I guess
    // or I should implicitly convert any type that allows it
    //  throw std::runtime_error("Callable types can't have mutability");
  };
  static auto fn_template_visitor = [](type_s::fn_template_t &val) {
    throw std::runtime_error("Function template types can't have mutability");
  };
  static auto auto_visitor = [](auto &val) {};

  ovisit(*ptr, indirection_visitor, fn_type_visitor, fn_template_visitor,
         auto_visitor);
}

void mutability_check_action(type_ptr ptr) {
    mutability_check_action_impl(ptr);
}

auto mutability_check(type_ptr ptr) {
  return [ptr] { 
    mutability_check_action_impl(ptr); 
  };
}

void type(ctx_t &ctx, locale_ptr locale, type_ptr ptr, util::mutability_t mut,
          type_s::var_t var) {
  *ptr = make::raw::type(std::move(var), mut);
  ctx.actions.schedule(mutability_check(ptr));
}

namespace make {

template <bool localy_indistinct, typename... Args>
auto decl(ctx_t &ctx, locale_ptr locale, const std::string_view name,
          auto assign_function, Args &&...args) {
  auto ptr = decl_name_preproc<localy_indistinct>(ctx, locale, name);
  assign_function(ctx, locale, ptr, std::forward<Args>(args)...);
  return ptr;
}

auto type_rec(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
              locale_ptr rec_locale, auto &&members, const auto &&decls) {

  *ptr = make::raw::taggregate(
      make::raw::trec(ctx, rec_locale, std::move(members)));

  // struct record_validation {
  //   std::vector<decl_ptr> &decls;
  //   ctx_t& ctx;

  //   void push(decl_ptr ptr) { decls.emplace_back(ptr); }
  //   void scope_decl(decl_s::scope_decl_t &val) {
  //     for (auto &elm : val.frame.stmts->stmts) {
  //       ovisit(
  //           *elm,
  //           [this](decl_ptr &val) {
  //             if (auto decl_val = val->as_var_decl_t()){
  //               push(val);
  //             }
  //             else if (auto scope = val->as_scope_decl_t())
  //               scope_decl(*scope);
  //           },
  //           [](auto &) {
  //             throw std::runtime_error(
  //                 "Record can't contain any non declarations");
  //           });
  //     }
  //   }
  // };

  auto record_register_members = [ptr, decls]() {
    auto &rec = *ptr->as_aggregate_t()->as_rec_t();
    size_t member_index = 0;
    for (size_t index = 0; index < decls.size(); ++index) {
      auto &elm = decls.at(index);
      ovisit(
          *elm,
          [&rec, &decls, index, &member_index](decl_s::var_decl_t &val) {
            if (val.type->is_infered_t()) {
              throw std::runtime_error("Members can't have their type infered");
            }
            if (val.type->is_empty_t() || !val.type) {
              throw std::runtime_error("Members need a type");
            }
            if (val.init_expr) {
              throw std::runtime_error("Fields can't have init values");
            }
            auto member = decls.at(index);
            register_member(rec, member_index++, member);
          },
          [](decl_s::type_decl_t &) {},
          [](auto &val) {
            throw std::runtime_error(
                "Only type and variable declarations are allowed in records");
          });
    }
  };
  ctx.actions.schedule(record_register_members);
}
type_ptr type_rec(ctx_t &ctx, locale_ptr locale, locale_ptr rec_locale,
                  auto &members, auto &decls) {
  auto ptr = make::raw::ptr(ctx, make::raw::type());
  type_rec(ctx, locale, ptr, rec_locale, members, decls);
}

void type(ctx_t &ctx, locale_ptr locale, type_ptr ptr, type_s::var_t var) {
  type(ctx, locale, ptr, ptr->mut, var);
}

}; // namespace make

namespace node2ast {

// type_ptr type_or_infered(ctx_t &ctx, locale_ptr locale, opt<median_t> med) {
//   if (med)
//     return type_fn(ctx, locale, med.value());
//   else
//     return make::raw::type_ptr(ctx, {make::raw::type_infered()});
// }
enum class fnsig_mode_t { FNDECL, FNTEMPLATE };
template <fnsig_mode_t mode>
sptr<util::fnsig_t> fnsig_fn(ctx_t &ctx, locale_ptr locale,
                             const median_t &med);

namespace type_fn_s {
template <bool is_mut>
auto ptr(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};

  auto type_med = cursor.extract<medianc::TYPE>();
  auto type = type_fn(ctx, locale, type_med.value());

  *ptr = make::raw::tindirection(make::raw::tptr(util::mutability_t::e{is_mut}, type));
}
auto array(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};
  auto [array_len_med, type_med] =
      cursor.tuple_extract<medianc::ARRAY_LENGTH, medianc::TYPE>();
  auto type = type_fn(ctx, locale, type_med.value());
  *ptr = make::raw::tindirection(make::raw::tarray(type, nullptr));
}

//when this is used we basicaly get a function pointer\
//maybe add it to the indirection types
void fn_type(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};
  const auto [args_med, ret_med] =
      cursor.tuple_extract<medianc::FN_ARGS, medianc::FN_RET>();
  std::vector<type_ptr> args;
  type_ptr ret;
  if (args_med) {
    auto cursor = grammar::cursor_helper_t{args_med->children()};
    while (cursor.within()) {
      auto type_med = cursor.extract<medianc::ARGUMENT>();
      auto ptr = type_fn(ctx, locale, type_med.value().fchild().as_median());
      args.push_back(ptr);
    }
  }

  if (ret_med)
    ret = type_fn(ctx, locale, ret_med->fchild().as_median());
  else
    ret = make::raw::tptr(ctx, {make::raw::tinfered()});

  *ptr = type_s::fn_type_t{std::move(args), ret};
}

void fn_template(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                 const median_t &med) {
  auto sig = fnsig_fn<fnsig_mode_t::FNTEMPLATE>(ctx, locale, med);
  *ptr = type_s::fn_template_t{sig};
  ctx.actions.schedule(mutability_check(ptr));
}

auto collection(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                const median_t &med) {
  throw std::runtime_error("TODO");
}

auto typeof(ctx_t & ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};
  auto expr_med = cursor.must_extract<medianc::EXPR>();
  auto expr = expr_fn(ctx, locale, expr_med);
}

auto tup(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto ch = med.children();
  auto types = std::vector<sptr<type_t>>{};
  for (auto &elm : ch) {
    auto elm_med = elm.as_median();
    auto elm_ch = elm_med.children();
    auto med = elm_ch.begin()->as_median();
    types.push_back(type_fn(ctx, locale, med));
  }
  *ptr = make::raw::taggregate(make::raw::ttup(types));
}

void rec(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med) {
  auto body_med = grammar::cursor_helper_t{med.children()}.must_extract<medianc::BODY>();
  std::vector<decl_ptr> members;
  std::vector<decl_ptr> decls;
  auto new_locale = locale_t::make_child(ctx, locale);
  auto cursor = grammar::cursor_helper_t{body_med.children()};
  while (cursor.within()) {
    const auto elm =
        cursor.must_extract<medianc::ELEMENT>().fchild().as_median();

    auto decl = decl_fn(ctx, new_locale, elm);
    decls.emplace_back(decl);
  }

  return make::type_rec(ctx, locale, ptr, new_locale, std::move(members),std::move(decls));
}
auto infer(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med)
    -> void {
  *ptr = make::raw::tinfered();
}

auto chain(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const median_t &med)
    -> void {
  auto index = type_s::unresolved_t{ctx.unresolved_type_list.size()};
  auto info = unresolved_type_t{locale, ptr, med};
  ctx.unresolved_type_list.push_back(info);
  *ptr = index;
  ctx.actions.schedule(defer::resolve_chain(ctx, index));
}
} // namespace type_fn_s

void type_med_fn(ctx_t &ctx, locale_ptr locale, type_ptr ptr,
                 const median_t &med) {
  using namespace type_fn_s;
  switch (med.type()) {
  case medianc::RECORD:
    return rec(ctx, locale, ptr, med);
  case medianc::TUPLE:
    return tup(ctx, locale, ptr, med);
  case medianc::COLLECTION:
    return collection(ctx, locale, ptr, med);
  case medianc::FN_TYPE:
    return fn_type(ctx, locale, ptr, med);
  case medianc::FN_TEMPLATE:
    return fn_template(ctx, locale, ptr, med);
  case medianc::TYPEOF:
    return typeof(ctx, locale, ptr, med);
  case medianc::PTR:
    return type_fn_s::ptr<true>(ctx, locale, ptr, med);
  case medianc::IMMUTABLE_PTR:
    return type_fn_s::ptr<false>(ctx, locale, ptr, med);
  case medianc::ARRAY:
    return type_fn_s::array(ctx, locale, ptr, med);
  case medianc::INFER:
    return infer(ctx, locale, ptr, med);
  case medianc::CHAIN:
    return type_fn_s::chain(ctx, locale, ptr, med);
  default:
    throw std::runtime_error("ERROR");
  }
}
void type_final_fn(ctx_t &ctx, locale_ptr locale, type_ptr ptr, const final_t fin) {
  auto type = fin->type();
  switch (type) {
  case tokc::BUILTIN_VOID:
    *ptr = make::raw::tprimitive(make::raw::tvoid());
    break;
  case tokc::BUILTIN_PTR: {
    *ptr = type_s::indirection_t{type_s::indirection_s::optr_t{}};
  } break;
  case tokc::TYPE_FLOAT:
    *ptr = make::raw::tprimitive(make::raw::tnumber(
        make::raw::tfloat(), make::raw::numeric2bitsize(ctx, fin)));
    break;
  case tokc::TYPE_INT:
    *ptr = make::raw::tprimitive(make::raw::tnumber(
        make::raw::tsint(), make::raw::numeric2bitsize(ctx, fin)));
    break;
  case tokc::TYPE_UINT:
    *ptr = make::raw::tprimitive(make::raw::tnumber(
        make::raw::tuint(), make::raw::numeric2bitsize(ctx, fin)));
    break;
  case tokc::TYPE_BOOLEAN:
    *ptr = make::raw::tprimitive(make::raw::tnumber(
        make::raw::tfloat(), make::raw::numeric2bitsize(ctx, fin)));
    break;
  default:
    throw std::runtime_error("ERROR");
  }
  return;
}

util::mutability_t deduce_mutability(const auto &mut_fin, const auto &imut_fin) {
  if (mut_fin) {
    return util::mutability_t::e::MUTABLE;
  } else if (imut_fin) {
    return util::mutability_t::e::IMMUTABLE;
  }
  return std::nullopt;
}
util::mutability_t extract_mutability(grammar::cursor_helper_t &cursor) {
  auto [imut_fin, mut_fin] =
      cursor.tuple_extract<tokc::BUILTIN_IMMUTABLE, tokc::BUILTIN_MUTABLE>();
  return deduce_mutability(mut_fin, imut_fin);
}
void type_fn_path(ctx_t &ctx, locale_ptr locale, semantics::type_ptr ptr,
                  const median_t &type_med) {
  auto cursor = grammar::cursor_helper_t{type_med.children()};

  auto mut = extract_mutability(cursor);
  auto [fin, med] = cursor.tuple_extract<tokc::any, medianc::any>();

  if (fin) {
    type_final_fn(ctx, locale, ptr, fin.value());
  } else if (med) {
    type_med_fn(ctx, locale, ptr, med.value());
  } else [[unlikely]] {
    throw std::runtime_error("There should always be a type");
  }
  ptr->mut = mut;

  if (ptr->mut)
    ctx.actions.schedule(mutability_check(ptr));
  ctx.actions.schedule(type_recursion_check(ctx, ptr));
}

type_ptr type_fn(ctx_t &ctx, locale_ptr locale, const median_t &type_med) {
  auto ptr = make::raw::tptr(ctx, make::raw::type(empty_t{}));
  type_fn_path(ctx, locale, ptr, type_med);
  return ptr;
}

namespace decl_spec {
enum class decl_mode_t { BASE, ARGUMENT };
template <decl_mode_t mode>
void process_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
                  grammar::cursor_helper_t cursor);
void arg_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
              grammar::cursor_helper_t cursor);
void base_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
               grammar::cursor_helper_t cursor);
void arg_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
              grammar::cursor_helper_t cursor);
} // namespace decl_spec

template <fnsig_mode_t mode>
sptr<util::fnsig_t> fnsig_fn(ctx_t &ctx, locale_ptr locale,
                             const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};

  struct {
    opt<median_t> state_med, template_med, args_med, ret_med;
  } meds;

  if constexpr (mode == fnsig_mode_t::FNDECL) {
    std::tie(meds.state_med, meds.template_med) =
        cursor.tuple_extract<medianc::FN_STATE_LIST, medianc::TEMPLATE_ARGUMENT_LIST>();
  }

  std::tie(meds.args_med, meds.ret_med) =
      cursor.tuple_extract<medianc::FN_ARGS, medianc::FN_RET>();

  if constexpr (mode == fnsig_mode_t::FNDECL) {
    const bool is_closure = [&meds] {
      if (meds.state_med) [[unlikely]]
        return (meds.state_med->len() > 0);
      return false;
    }();
  }
  auto rec_locale = locale_t::make_child(ctx, locale);

  if constexpr (mode == fnsig_mode_t::FNDECL) {
    // what do we do with state lists???
    if (meds.state_med) {
    }
    // what do we do with templates???
    if (meds.template_med) {
    }
  }

  std::vector<svar_ptr<decl_t, decl_s::var_decl_t>> args;
  if (meds.args_med) [[likely]] {
    auto cursor = grammar::cursor_helper_t{meds.args_med->children()};
    while (cursor.within()) {
      // auto decl_med = cursor.must_extract<medianc::ARGUMENT>();
      const auto med = cursor.must_extract<medianc::ARGUMENT>()
                           .fchild()
                           .as_median()
                           .expect<medianc::DECL>();
      auto ptr = decl_spec_fn<decl_spec::arg_decl>(ctx, rec_locale, med);
      args.emplace_back(ptr);
    }
  }

  type_ptr ret = type_fn(ctx, locale, meds.ret_med->fchild().as_median());

  auto ptr =
      make::raw::ptr(ctx, make::raw::fnsig(rec_locale, std::move(args), ret));
  return ptr;
}

namespace decl_spec {

template <decl_mode_t mode>
void process_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
                  grammar::cursor_helper_t cursor) {
  auto [type_med, val_med] =
      cursor.tuple_extract<medianc::TYPE, medianc::VALUE>();

  auto type = type_fn(ctx, locale, type_med.value());
  {
    auto info = unresolved_decl_t{locale, ptr, type, val_med};
    const auto index = ctx.unresolved_decl_list.size();
    ctx.unresolved_decl_list.emplace_back(info);
    *ptr = decl_s::unresolved_t{index};
    ctx.actions.schedule(defer::resolve_decl(ctx, {index}));
  }

  if constexpr (mode == decl_mode_t::ARGUMENT)
    ctx.actions.schedule([ptr] {
      if (auto decl = ptr->as_var_decl_t()) {
        if (decl->init_expr) [[unlikely]] {
          throw std::runtime_error(
              "Init expresions are not allowed on function arguments");
        }
        if (decl->type->is_infered_t()) [[unlikely]] {
          throw std::runtime_error("Arguments can't be infered");
        }
      } else {
        throw std::runtime_error(
            "Function arguments must be variable declarations");
      }
    });
}

void base_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
               grammar::cursor_helper_t cursor) {
  return process_decl<decl_mode_t::BASE>(ctx, locale, ptr, cursor);
}


void arg_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
              grammar::cursor_helper_t cursor) {
  return process_decl<decl_mode_t::ARGUMENT>(ctx, locale, ptr, cursor);
}

void scope_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
                grammar::cursor_helper_t cursor) {
  auto body = cursor.must_extract<medianc::BODY>();
  if (auto scope = ptr->as_scope_decl_t()) {
    stmts_fn(ctx, scope->frame.locale, *scope->frame.stmts, body.children());
  } else if (ptr->is_empty_t()) {
    auto scope = make::raw::scope_decl(
        make::raw::frame(locale_t::make_child(ctx, locale),
                         make::raw::ptr(ctx, make::raw::stmts())));
    stmts_fn(ctx, scope.frame.locale, *scope.frame.stmts, body.children());
    *ptr = scope;
  } else {
    throw std::runtime_error(
        "Symbol is not a scope but is used to delcare one");
  }
}

void type_decl(ctx_t &ctx, locale_ptr locale, decl_ptr ptr,
               grammar::cursor_helper_t cursor) {
  auto template_list_args_med =
      cursor.extract<medianc::TEMPLATE_ARGUMENT_LIST>();
  auto type_med = cursor.must_extract<medianc::TYPE>();


  if (template_list_args_med) {
    // TODO
  }

  auto type_ptr = type_fn(ctx, locale, type_med);
  *ptr = make::raw::type_decl(type_ptr);
}
} // namespace decl_spec

template <auto fn>
decl_ptr decl_spec_fn(ctx_t &ctx, locale_ptr locale, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};
  const auto name = ctx.toks().str(cursor.extract<tokc::ID>().value());
  constexpr bool localy_indistinct = fn == decl_spec::scope_decl;
  return make::decl<localy_indistinct>(ctx, locale, name, fn, cursor);
}

decl_ptr decl_fn(ctx_t &ctx, locale_ptr locale, const median_t &med) {
  switch (med.type()) {
  case medianc::SCOPE_DECL:
    return decl_spec_fn<decl_spec::scope_decl>(ctx, locale, med);
  case medianc::TYPE_DECL:
    return decl_spec_fn<decl_spec::type_decl>(ctx, locale, med);
  case medianc::DECL:
    return decl_spec_fn<decl_spec::base_decl>(ctx, locale, med);
  default:
    [[unlikely]] std::unreachable();
  }
}

namespace expr {
auto token_to_operator(const tokc::e token) -> expr_s::operator_t{
  using namespace make::raw;
  expr_ptr dummy = nullptr;
  switch (token) {
    case tokc::EQUALS:          return {bop(expr_s::op_operation_e::EQ, dummy, dummy)};
    case tokc::GEQUALS:         return {bop(expr_s::op_operation_e::GEQ, dummy, dummy)};
    case tokc::LEQUALS:         return {bop(expr_s::op_operation_e::LEQ,  dummy, dummy)};
    case tokc::EMARKEQUALS:     return {bop(expr_s::op_operation_e::NEQ, dummy, dummy)};
    case tokc::PLUSASIGN:       return {bop(expr_s::op_operation_e::PLUSASSIGN, dummy, dummy)};
    case tokc::MINUSASIGN:      return {bop(expr_s::op_operation_e::MINUSASSIGN, dummy, dummy)};
    case tokc::DIVASIGN:        return {bop(expr_s::op_operation_e::DIVASSIGN, dummy, dummy)};
    case tokc::MULASIGN:        return {bop(expr_s::op_operation_e::MULTASSIGN, dummy, dummy)};
    case tokc::ASIGN:           return {bop(expr_s::op_operation_e::ASSIGN, dummy, dummy)};
    case tokc::PLUSPLUS:        return {uop(expr_s::op_operation_e::PLUSPLUS, dummy, {})};
    case tokc::MINUSMINUS:      return {uop(expr_s::op_operation_e::MINUSMINUS, dummy, {})};
    case tokc::GREATERGREATER:  return {bop(expr_s::op_operation_e::SRIGHT, dummy, dummy)};
    case tokc::LESSLESS:        return {bop(expr_s::op_operation_e::SLEFT, dummy, dummy)};
    case tokc::LESSGREATER:     return {bop(expr_s::op_operation_e::NEQ, dummy, dummy)};
    case tokc::XOR:             return {bop(expr_s::op_operation_e::XOR, dummy, dummy)};
    case tokc::AND:             return {bop(expr_s::op_operation_e::AND, dummy, dummy)};
    case tokc::OR:              return {bop(expr_s::op_operation_e::OR, dummy, dummy)};
    case tokc::MODULO:          return {bop(expr_s::op_operation_e::MOD, dummy, dummy)};
    case tokc::PLUS:            return {bop(expr_s::op_operation_e::PLUS, dummy, dummy)};
    case tokc::MINUS:           return {bop(expr_s::op_operation_e::MINUS, dummy, dummy)};
    case tokc::DIV:             return {bop(expr_s::op_operation_e::DIV, dummy, dummy)};
    case tokc::MUL:             return {bop(expr_s::op_operation_e::MULT, dummy, dummy)};
    case tokc::LESS:            return {bop(expr_s::op_operation_e::LESS, dummy, dummy)};
    case tokc::GREATER:         return {bop(expr_s::op_operation_e::GREATER, dummy, dummy)};
    case tokc::DIAMOND:         return {bop(expr_s::op_operation_e::DIAMOND, dummy, dummy)};
    case tokc::EMARK:           return {uop(expr_s::op_operation_e::NOT, dummy, {})};
    case tokc::AMPERSAND:       return {uop(expr_s::op_operation_e::ADDRESS, dummy, {})};
    case tokc::ANDASIGN:        return {bop(expr_s::op_operation_e::ASSIGN, dummy, dummy)};
    case tokc::ORASIGN:         return {bop(expr_s::op_operation_e::ASSIGN, dummy, dummy)};
    case tokc::MINUSGREATER:    return {bop(expr_s::op_operation_e::PIPE, dummy, dummy)};
    default:
      throw std::runtime_error("This token is not an operator");
  }
}

auto as_payload_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::uop_t::as_payload_t {
  auto type_med = med.fchild().as_median().expect<medianc::TYPE>();
  return expr_s::uop_t::as_payload_t{type_fn(ctx, loc, type_med)};
}

auto operator_fn(ctx_t &ctx, locale_ptr locale, expr_ptr parent,const median_t med)
    -> svar_ptr<expr_t, expr_s::operator_t> {
  auto node = med.fchild().node();
  auto base = ovisit(
      node,
      [](const final_t &val) -> expr_s::operator_t {
        auto t = token_to_operator(val->type());
        return t;
      },
      [&ctx, &locale](const median_t &val) -> expr_s::operator_t {
        switch (val.type()) {
        case medianc::AS: [[likely]]
          return expr_s::operator_t{
              make::raw::as(expr_s::op_operation_e::AS,
                            as_payload_fn(ctx, locale, val), nullptr)};
        default:
          std::unreachable();
        }
      },
      [](const auto &) -> expr_s::operator_t { std::unreachable(); });
  return ctx.alloc<expr_t>(base, parent, ctx.alloc<type_t>(empty_t{}));
}

// TODO add the chain
// auto self_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med)
//     -> expr_s::self{
//   return empty_t{};
// }

// TODO add the chain
auto block_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med) -> expr_s::operand_s::block_t {
  auto locale = locale_t::make_child(ctx, loc);

  auto body_med = grammar::cursor_helper_t{med.children()}.must_extract<medianc::BODY>();
  auto stmts = ctx.alloc(stmts_fn(ctx, locale, body_med.children()));

  return {make::raw::frame(locale, stmts), nullptr};
}

auto result_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med) -> expr_s::operand_s::result_t {
  auto cursor = grammar::cursor_helper_t{med.children()};

  auto body_med = cursor.must_extract<medianc::BODY>();
  auto expr_ptr =
      expr_fn(ctx, loc, body_med.fchild().as_median().expect<medianc::EXPR>());

  return {expr_ptr};
}

auto pipe_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::operand_s::pipe_t{
  /* auto ch = med.children();
  auto cursor = ch.begin();
  while (ch.contain(cursor)) {
    cursor.advance();
  } */
  return expr_s::operand_s::pipe_t{{nullptr}};
}

auto sizeof_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med) -> expr_s::operand_t {
  auto expr_or_type_med = med.
    fchild().
    as_median().
    expect<medianc::TYPE, medianc::EXPR>();

  switch (expr_or_type_med.type()) {
  case medianc::TYPE:
    return {expr_s::operand_s::sizeof_type_t{type_fn(ctx, loc, expr_or_type_med)}};
  case medianc::EXPR:
    return {expr_s::operand_s::sizeof_expr_t{expr_fn(ctx, loc, expr_or_type_med)}};
  default:
    std::unreachable();
  }
}

expr_s::operand_s::var_t fn_complit_fn(
    ctx_t &ctx,
    opt<median_t> cinit_med,
    type_ptr &type) {
  auto locale = get_locale(*type);
  auto stmts = ctx.alloc(stmts_fn(ctx, locale, cinit_med->children()));
  return expr_s::operand_s::fn_lit_t{type, stmts};
}
auto complit_fn(ctx_t &ctx, sptr<locale_t> loc, const median_t med)
    -> expr_s::operand_s::var_t {
  auto cursor = grammar::cursor_helper_t{med.children()};
  auto [ctype_med, cinit_med] =
      cursor.tuple_extract<medianc::COMPOUND_LITERAL_TYPE,
                           medianc::COMPOUND_LITERAL_INIT>();

  auto type = type_fn(ctx, loc, ctype_med->fchild().as_median());
  // if (cinit_med->children().begin()->as_median().type() == medianc::STMT) {
  if (rholds<type_s::fn_template_t>(*type)) {
    return fn_complit_fn(ctx, cinit_med, type);
  } else {
    list<expr_ptr> exprs;
    auto ch = cinit_med->children();
    auto cursor = ch.begin();
    while (ch.contains(cursor)) {
      auto med = cursor->as_median().expect<medianc::EXPR>();
      exprs.push_back(expr_fn(ctx, loc, med));
      cursor.advance();
    }
    return expr_s::operand_s::complit_t{type, exprs};
  }
}

auto if_fn(ctx_t &ctx, locale_ptr locale, const median_t med)
    -> expr_s::operand_s::if_t {
      
  auto cursor = grammar::cursor_helper_t{med.children()};
  auto val = expr_s::operand_s::if_t{{}};
  bool one_else = false;
  while (cursor.within()) {
    auto var_med =
        cursor.extract<medianc::any>();
    if (!var_med) [[unlikely]]
      throw std::runtime_error(std::string(__PRETTY_FUNCTION__) +
                               " This shouldn't happen");

    if (var_med->type() == medianc::IF) {
      auto cursor = grammar::cursor_helper_t{var_med->children()};
      auto [ctrl_expr_med, body_med] =
          cursor.tuple_extract<medianc::CTRL_EXPR, medianc::BODY>();

      auto expr = expr_fn(ctx, locale, ctrl_expr_med.value().children().begin()->as_median());
      auto stmts = ctx.alloc(stmts_fn(ctx, locale, body_med->children()));
      auto elif = expr_s::operand_s::if_t::if_link_t{expr, stmts};

      val.ifs.emplace_back(std::move(elif));
    } else if (var_med->type() == medianc::ELSE) {
      {
        if (cursor.within()) [[unlikely]]
          throw std::runtime_error(
              "The else part of the if expr should be last");
        if (one_else) [[unlikely]]
          throw std::runtime_error(
              "Can't have more than one else in a if expr");
      }
      one_else = true;
      auto body_med =
          grammar::cursor_helper_t{var_med->children()}.extract<medianc::BODY>();
      auto el = expr_s::operand_s::if_t::if_link_t{{nullptr}, ctx.alloc(stmts_fn(ctx, locale, body_med->children()))};
      ;
      val.ifs.push_back({el});
    } else {
      throw std::runtime_error(std::string(__PRETTY_FUNCTION__) +
                               "This shouldn't happen");
    }
  }
  return val;
}

auto operand_final_fn(ctx_t &ctx, locale_ptr locale, const final_t& val)
    -> expr_s::operand_t {
        switch (val->type()) {
        // this is kind of retarded but I am not sure
        // how to represent it
        case tokc::INT:
          return expr_s::operand_t{expr_s::operand_s::number_t{{expr_s::operand_s::number_t::int_t{}}, ctx.toks().str(val)}};
        case tokc::FLOAT:
          return expr_s::operand_t{expr_s::operand_s::number_t{{expr_s::operand_s::number_t::float_t{}}, ctx.toks().str(val)}};
        default:
          std::unreachable();
        }
    }
auto operand_med_fn(ctx_t &ctx, locale_ptr locale,const median_t& val)
    -> expr_s::operand_t{
        switch (val.type()) {
        case medianc::CHAIN:
          // TODO:
          // ctx.insert_callback(ptr, resolve::expr_elm_chain_fn(ctx, loc, ptr));
          // return unresolved_t{val};
          throw std::runtime_error("Chains on expresions are not supporetd for now");
        case medianc::RESULT:
          return expr_s::operand_t{result_fn(ctx, locale, val)};
        case medianc::BLOCK_EXPR:
          return expr_s::operand_t{block_fn(ctx, locale, val)};
        case medianc::SIZEOF:
          return expr_s::operand_t{sizeof_fn(ctx, locale, val)};
        case medianc::COMPOUND_LITERAL:
          return expr_s::operand_t{complit_fn(ctx, locale, val)};
        case medianc::FN_LITERAL:
          // return expr_s::operand_t{complit_fn(ctx, locale, val)};
          throw std::runtime_error("Function litterals/lambdas are not supported yet");
        case medianc::IF_EXPR:
          return expr_s::operand_t{if_fn(ctx, locale, val)};
        case medianc::PIPE:
          return expr_s::operand_t{pipe_fn(ctx, locale, val)};
        // case medianc::SELF:
        //   return self_fn(ctx, loc, val);
        default:
          val.expect<medianc::last>();
          // std::unreachable();
        }
    }
auto operand_fn(ctx_t &ctx, locale_ptr locale, expr_ptr parent, const median_t med)
    -> expr_ptr {
  // ctx.dbg_add_call();
  auto node = med.children().begin()->node();
  auto ptr = ctx.alloc<expr_t>();
  auto var_val = ovisit(
      node,
      [&ctx](const final_t &val) -> expr_s::var_t {
        switch (val->type()) {
        case tokc::INT:
          return expr_s::operand_t{expr_s::operand_s::number_t{{expr_s::operand_s::number_t::int_t{}}, ctx.toks().str(val)}};
        case tokc::FLOAT:
          return expr_s::operand_t{expr_s::operand_s::number_t{{expr_s::operand_s::number_t::float_t{}}, ctx.toks().str(val)}};
        default:
          std::unreachable();
        }
      },
      [&ctx, &locale](const median_t &val) -> expr_s::var_t {
        return operand_med_fn(ctx, locale, val);
      },
      [](const auto &val) -> expr_s::var_t { std::unreachable(); });
  *ptr = expr_t{var_val, parent, ctx.alloc<type_t>(empty_t{})};
  return ptr;
}

namespace operator_s {
void prefix_fallback(sptr<expr_s::operator_t> &op) {
  auto operation = op->meta().op;
  switch (operation) {
  case expr_s::op_operation_e::PLUS:
    *op = make::raw::uop(expr_s::op_operation_e::POSITIVE, 0, {});
    break;
  case expr_s::op_operation_e::MINUS:
    *op = make::raw::uop(expr_s::op_operation_e::NEGATIVE, 0, {});
    break;
  default:
    throw std::runtime_error("Expected a prefix operator but found " +
                             std::string(expr_s::str(op->meta())));
  }
}
} // namespace operator_s

expr_ptr pratt_parsing(ctx_t &ctx, locale_ptr locale, span_t ch, cursor_t &cursor, expr_ptr parent, size_t min_prec) {
  expr_ptr lhs = nullptr;
  auto med = cursor++->as_median();
  switch (med.type()) {
  case medianc::OPERATOR: {
    auto op_ptr = operator_fn(ctx, locale, parent, med);
    auto op = op_ptr->as_operator_t();
    if (op->meta() != expr_s::op_pos_e::PREFIX) {
      operator_s::prefix_fallback(op);
    }
    const auto &meta = op->meta();
    if (auto uop = op_ptr->as_operator_t()->as_uop_t()) [[likely]] {
      // this causes the + not passing since it set's it too high
      uop->operand = pratt_parsing(ctx, locale, ch, cursor, op_ptr, meta.prec);
      lhs = op_ptr;
    } else {
      throw std::runtime_error("This should be a unary operator");
    }
    break;
  }
  case medianc::OPERAND: {
    lhs = operand_fn(ctx, locale, parent, med);
    break;
  }
  default: {
    throw std::runtime_error("Neither a Operator or Operand on the prefix stage of an expresion");
  }
  }
  while (ch.contains(cursor)) {
    auto med = cursor->as_median();
    if (med.type() != medianc::OPERATOR)
      break;
    auto op_ptr = operator_fn(ctx, locale, parent, med);
    auto &op = op_ptr.as<expr_s::operator_t>();
    const auto &meta = op.meta();
    // Prefix
    if (meta == expr_s::op_pos_e::PREFIX)
      throw std::runtime_error("Expected Postfix or Infix operators");
    // Postfix
    else if (meta == expr_s::op_pos_e::POSTFIX) {
      auto &val = *op.as_uop_t();
      auto lprec = left_bp(meta);
      if (lprec < min_prec)
        break;
      cursor.advance();

      val.operand = lhs;
      lhs = op_ptr;
      continue;
    }
    // Infix
    else if (meta == expr_s::op_pos_e::INFIX) {
      auto &val = *op.as_bop_t();
      auto lprec = left_bp(meta);
      auto rprec = right_bp(meta);
      if (lprec < min_prec)
        break;
      cursor.advance();

      expr_ptr rhs = pratt_parsing(ctx, locale, ch, cursor, parent, rprec);
      val.lhs = lhs; // <--- attach lhs and rhs to the operator
      val.rhs = rhs;
      lhs = op_ptr; // <--- new lhs is the combined binary operation
      continue;
    }
    break;
  }
  return lhs;
}
void expr_printer(const token_buffer_t &toks, expr_ptr expr,
                  const size_t indent = 0) {
  using namespace expr_s;
  auto indent_str = std::string(indent * 3, ' ');
  return ovisit(
      *expr,
      [&](operator_t &val) {
        ovisit(
            val,
            [&](uop_t &uop) {
              std::cout << indent_str << "operator: unary ("
                        << expr_s::str(uop.meta().op) << ")\n";
              if (!uop.operand) {
                std::cout << indent_str << "  operand: null\n";
              } else {
                expr_printer(toks, uop.operand, indent + 1);
              }
            },
            [&](bop_t &bop) {
              std::cout << indent_str << "operator: binary ("
                        << expr_s::str(bop.meta().op) << ")\n";

              if (!bop.lhs) {
                std::cout << indent_str << "  lhs: null\n";
              } else {
                std::cout << indent_str << "  lhs:\n";
                expr_printer(toks, bop.lhs, indent + 2);
              }

              if (!bop.rhs) {
                std::cout << indent_str << "  rhs: null\n";
              } else {
                std::cout << indent_str << "  rhs:\n";
                expr_printer(toks, bop.rhs, indent + 2);
              }
            });
      },
      [&](operand_t &val) {
        ovisit(
            val,
            [&](operand_s::result_t &res) {
              std::cout << indent_str << "operand: result\n";
              expr_printer(toks, res.val, indent + 1);
            },
            [&](operand_s::number_t &) {
              std::cout << indent_str << "operand: number\n";
            },
            [&](auto &) { std::cout << indent_str << "operand: other\n"; });
      },
      [&](auto &) { std::cout << indent_str << "unknown expr type\n"; });
}

expr_ptr expr_tree(ctx_t &ctx, locale_ptr locale, span_t ch) {
  auto cursor = ch.begin();
  auto ptr = pratt_parsing(ctx, locale, ch, cursor, 0, 0);
  if (ch.contains(cursor)) {
    throw std::runtime_error("Did not consume the whole expresion");
  }

  if (ptr) {
    expr_printer(ctx, ptr);
  } else {
    throw std::runtime_error("Expresions can't be null >_< ");
  }

  ctx.actions.schedule([ptr] {
    
    // enrich the nested expresions with their parents
    // find every expresion under this one that has a null parent
    // if you found it, it means that is your child
    // resolve the type
    (void)(ptr);
  });
  return ptr;
}
} // namespace expr

expr_ptr expr_fn(ctx_t &ctx, locale_ptr locale, span_t span) {
  return expr::expr_tree(ctx, locale, span);
}



expr_ptr expr_fn(ctx_t &ctx, locale_ptr locale, const median_t &med) {
  if (med.type() != medianc::EXPR)
    throw std::runtime_error("What was passed was not an expresion it was: " +
                             std::string(medianc::str(med.type())));
  auto ptr = expr::expr_tree(ctx, locale, med.children());


  return ptr;
}

template <typename T>
void exit_ctrl_fn(ctx_t &ctx, locale_ptr locale, stmt_ptr ptr,
                  const median_t &med) {

  auto expr_med = grammar::cursor_helper_t{med.children()}.extract<medianc::EXPR>();
  expr_ptr exprs;

  if (expr_med) {
    exprs = expr_fn(ctx, locale, expr_med.value());
  }

  *ptr = T{exprs};
}

void import_fn(ctx_t &ctx, locale_ptr locale, stmt_ptr ptr, const median_t &med) {
  auto strlit_node =
      grammar::cursor_helper_t{med.children()}.must_extract<tokc::STRLIT>();
  *ptr = stmt_s::import_t{ctx.toks().str(strlit_node)};
}

void stmt_fn(ctx_t &ctx, locale_ptr locale, stmt_ptr ptr, const median_t &med) {
  auto cursor = grammar::cursor_helper_t{med.children()};
  auto [attributes_med, stmt_med] =
      cursor.tuple_extract<medianc::ATTRIBUTES, medianc::any>();

  if (!stmt_med) [[unlikely]] {
    throw std::runtime_error("Failed to exctract stmt median");
  }

  switch (stmt_med->type()) {
  case medianc::SCOPE_DECL:
  case medianc::TYPE_DECL:
  case medianc::DECL: {
    *ptr = decl_fn(ctx, locale, stmt_med.value());
  } break;
  case medianc::IMPORT:
    return import_fn(ctx, locale, ptr, stmt_med.value());
  case medianc::RETURN:
    return exit_ctrl_fn<stmt_s::return_t>(ctx, locale, ptr, *stmt_med);
  case medianc::BREAK:
    return exit_ctrl_fn<stmt_s::break_t>(ctx, locale, ptr, *stmt_med);
  case medianc::BECOME:
    return exit_ctrl_fn<stmt_s::become_t>(ctx, locale, ptr, *stmt_med);
  case medianc::EXPR: {
    *ptr = expr_fn(ctx, locale, *stmt_med);
  } break;
  default:
    throw std::runtime_error("Unknown median type: " +
                             std::string(medianc::str(stmt_med->type())));
  }
}
stmt_ptr stmt_fn(ctx_t &ctx, locale_ptr  locale, const median_t &med) {
  auto ptr = make::raw::ptr(ctx, stmt_t{});
  stmt_fn(ctx, locale, ptr, med);
  return ptr;
}

void stmts_fn(ctx_t &ctx, locale_ptr locale, stmts_t &val, span_t span) {
  for (auto &elm : span)
    val.stmts.emplace_back(stmt_fn(ctx, locale, elm.as_median()));
}

stmts_t stmts_fn(ctx_t &ctx, sptr<locale_t> locale, span_t span) {
  stmts_t stmts = {};
  stmts_fn(ctx, locale, stmts, span);
  return stmts;
}

}; // namespace node2ast

auto entry(allocator_t &allocator, const token_buffer_t &toks,
           const std::map<size_t, size_t> &toks_symetrical_map,
           grammar::node_t &file) {
  ctx_t ctx(toks, toks_symetrical_map, allocator);
  auto frame = make::raw::frame(make::raw::ptr<locale_t>(ctx),
                                make::raw::ptr<stmts_t>(ctx));
  auto span = file.as_median().children();

  node2ast::stmts_fn(ctx, frame.locale, frame.stmts.get_val(), span);
  ctx.actions();

  // PRINTING
  // {
  //   std::println("");
  //   for (auto &elm : frame.locale->internals.table) {
  //     std::print("name: {} ", elm.first);
  //     ovisit(*elm.second, [](auto &val) {
  //       std::println("type:{}", boost::core::demangled_name(typeid(val)));
  //     });
  //   }
  // }

  return;
}
} // namespace semantics
