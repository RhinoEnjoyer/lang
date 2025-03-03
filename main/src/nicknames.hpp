#pragma once

#include <memory>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

using empty_t = std::monostate;

template <typename T> using list = std::vector<T>;
template <typename T> using opt = std::optional<T>;
template <typename T> using sptr = std::shared_ptr<T>;

template <typename T, typename... Args>
auto make_sptr(Args &&...val) -> sptr<T> {
  return std::make_shared<T>(std::forward<Args>(val)...);
}
template <typename T> auto make_sptr(T &&val) -> sptr<T> {
  return std::make_shared<T>(std::forward<T>(val));
}

template <typename... T> using var = std::variant<T...>;

// check the value that i inserted into it
template <typename T, typename BASE> struct ssptr {
  sptr<BASE> val;

  T &get() { return std::get<T>(*val); }
  const T &get() const { return std::get<T>(*val); }

  sptr<BASE> ptr() { return val; }
  const sptr<BASE> ptr() const { return val; }

  ssptr(sptr<BASE> ptr) : val(ptr) {}
  template <typename... Args>
  ssptr(Args &&...v) : val(std::make_shared<BASE>(std::forward<Args>(v)...)) {}
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
      : variant_(std::make_shared<std::decay_t<T>>(std::forward<T>(value))) {}

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
