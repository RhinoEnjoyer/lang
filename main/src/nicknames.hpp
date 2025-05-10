#pragma once

#include "../libs/mempool.hpp"
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

using empty_t = std::monostate;

template <typename T> using list = std::vector<T>;
template <typename T> using opt = std::optional<T>;
template <typename... T> using var = std::variant<T...>;
// I might create a series of pools to holds these pointers
// for now we are just going to leak the whole thing

template <typename T> struct sptr {
  using type =  T;
  using pointer = T *;
  using ref = T &;
  pointer ptr;

  sptr(pointer p = nullptr) : ptr(p) {}

  auto *get_ptr() const { return ptr; }
  auto &get_val() const { return *ptr; }

  void *as_void() const { return ptr; }
  uintptr_t as_uint() { return reinterpret_cast<uintptr_t>(as_void()); }

  auto *operator->() const { return get_ptr(); }
  auto &operator*() const { return get_val(); }

  bool is_null() const { return (ptr == nullptr); }
  operator bool() const { return !is_null(); }
};

template <typename... Types> struct varptr {
  var<empty_t*, Types *...> ptr;
  template <typename T> varptr(T *p = (empty_t *)(nullptr)) : ptr(p) {}
  template <typename T> varptr(sptr<T> p) : ptr(p.get_ptr()) {}
  template <typename T> auto to_sptr() -> sptr<T> { return sptr<T>{std::get<T*>(ptr)}; }
  template <typename T> operator sptr<T>() { return to_sptr<T>(); }

  void *optr() {
    return ovisit(ptr, [](auto &&val) -> void * { return val; });
  }
};

using allocator_t = mempool_t;
template <typename T, typename... Args>
auto make_ptr(allocator_t &allocator, Args &&...val) -> sptr<T> {
  return allocator.alloc<T>(std::forward<Args>(val)...);
}
template <typename T>
auto make_ptr(allocator_t &allocator, T &val) -> sptr<T> {
  return allocator.alloc<T>(val);
}
template <typename T>
auto make_ptr(allocator_t &allocator, T &&val) -> sptr<T> {
  return allocator.alloc<T>(std::forward<T>(val));
}

// TODO: check the value that i inserted into it
template <typename T, typename BASE> struct ssptr {
  sptr<BASE> val;
  ssptr() : val(nullptr) {}
  ssptr(sptr<BASE> &ptr) : val(ptr) {}
  ssptr(const sptr<BASE> &ptr) : val(ptr) {}
  ssptr(const sptr<BASE> &&ptr) : val(std::move(ptr)) {}
  ssptr(sptr<BASE> &&ptr) : val(std::move(ptr)) {}

  T &get() { return std::get<T>(val.get_val()); }
  const T &get() const { return std::get<T>(val.get_val()); }

  sptr<BASE> &ptr() { return val; }
  const sptr<BASE> ptr() const { return val; }
};

template <typename T> auto holds(auto &&val) -> bool {
  return std::holds_alternative<T>(val);
}

// made to traverse type hierarchies
template <typename T, typename... Ts> auto rholds(auto &&val) -> bool {
  if (holds<T>(val)) [[likely]] {
    if constexpr (sizeof...(Ts) > 0)
      return rholds<Ts...>(std::get<T>(val));
    return true;
  }
  return false;
}

template <typename T, typename Variant> struct var_index;

template <typename T, typename... Types>
struct var_index<T, std::variant<Types...>> {
  static constexpr std::size_t value = [] consteval {
    constexpr std::size_t n = sizeof...(Types);
    std::size_t index = 0;
    ((std::is_same_v<T, Types> ? index : ++index), ...);
    return index == n ? -1 : index; // Return -1 if T is not found
  }();
};

template <typename... Types> class svar {
public:
  using var_type = var<sptr<Types>...>;

  template <typename T,
            typename = std::enable_if_t<(std::is_same_v<T, Types> || ...)>>
  svar(T &&value)
      : variant_(make_sptr<std::decay_t<T>>(std::forward<T>(value))) {}

  template <typename T,
            typename = std::enable_if_t<(std::is_same_v<T, Types> || ...)>>
  svar(sptr<T> ptr) : variant_(std::move(ptr)) {}

  template <typename T> bool holds() const {
    return std::holds_alternative<sptr<T>>(variant_);
  }

  // Get the stored shared_ptr for a specific type
  template <typename T> sptr<T> get() const {
    if (holds<T>()) {
      return std::get<sptr<T>>(variant_);
    }
    return nullptr;
  }

private:
  var_type variant_;
};

template <typename IT> struct bounded_cursor_t {
  IT cursor;
  const IT end;

  bounded_cursor_t next() const { return {cursor + 1, end}; }
  IT current() const { return cursor; }
  template<size_t offset = 0>
  bool within() const {
    return (cursor + offset) >= end;
  }
  void advance() { cursor++; }
};
