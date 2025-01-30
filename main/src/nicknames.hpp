#pragma once

#include <optional>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

template <typename T> using list = std::vector<T>;
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

template <typename T, typename Variant>
struct var_index;

template <typename T, typename... Types>
struct var_index<T, std::variant<Types...>> {
    static constexpr std::size_t value = [] consteval {
        constexpr std::size_t n = sizeof...(Types);
        std::size_t index = 0;
        ((std::is_same_v<T, Types> ? index : ++index), ...);
        return index == n ? -1 : index; // Return -1 if T is not found
    }();
};

template <typename... Types>
class svar {
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
    template <typename T>
    sptr<T> get() const {
        if (holds<T>()) {
            return std::get<sptr<T>>(variant_);
        }
        return nullptr;
    }

private:
    var_type variant_;
};
