#pragma once

#include <optional>
#include <memory>
#include <variant>

template <typename T> using opt = std::optional<T>;
template <typename T> using sptr = std::shared_ptr<T>;
template <typename... T> using var = std::variant<T...>;
template <typename T, typename BASE> struct ssptr {
  sptr<BASE> val;

  T &get() { return std::get<T>(*val); }
  const T &get() const { return std::get<T>(*val); }

  sptr<BASE> ptr() { return val; }
  const sptr<BASE> ptr() const { return val; }

  ssptr(sptr<BASE> ptr) : val(ptr) {}
  ssptr(BASE v) : val(std::make_shared<BASE>(v)) {}
};
