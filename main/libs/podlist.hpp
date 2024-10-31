#pragma once

#include <cstdint>
#include <llvm/Support/Compiler.h>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <optional>
#include <type_traits>
#include <utility>

/* Move only container
 *  made for PODs
 *  manualy allocated
    manualy deallocated */
template <typename T>
struct podlist_t {
  using len_t = std::uint64_t;
  T* begin_;
  len_t len_;  // maybe use pointers? it is the same thing
  len_t cap_;  // maybe use pointers? it is the same thing

  podlist_t(T* begin, len_t len, len_t cap)
      : begin_(begin), len_(len), cap_(cap) {}
  podlist_t() : begin_(nullptr), len_(0), cap_(0) {}

  constexpr static podlist_t create(const len_t cap) {
    return podlist_t((T*)std::malloc(cap * sizeof(T)), 0, cap);
    // std::cout << __PRETTY_FUNCTION__ << " " << cap << std::endl;
  }

  template <const len_t cap = 0>
  static podlist_t create() {
    if constexpr (cap == 0) {
      return podlist_t{nullptr, 0, 0};
    } else {
      return podlist_t((T*)std::malloc(cap * sizeof(T)), 0, cap);
    }
  }
  constexpr static podlist_t make() { return podlist_t{nullptr, 0, 0}; }

  template <typename... Args>
  static podlist_t<T> make(Args&&... args) {
    constexpr len_t cap = sizeof...(Args);
    auto p = podlist_t<T>::create(cap);
    p.append(std::move(args)...);
    return std::move(p);
  }

  template <typename... Args>
  static podlist_t<T> make_from_opt(Args&... args) {
    constexpr len_t cap = sizeof...(Args) + 1;
    // auto p = podlist_t((T *)std::malloc(cap * sizeof(T)), 0, cap);
    auto p = podlist_t<T>::create<cap>();
    (
        [&](std::optional<T>& v) {
          if (v.has_value())
            p.push_back(std::move(v.value()));
        }(args),
        ...);
    return std::move(p);
  }

  T& at(len_t index) { return begin_[index]; }
  const T& at(len_t index) const { return begin_[index]; }

  void resize(const len_t newcap) {
    T* const re = static_cast<T*>(realloc(begin_, newcap * sizeof(T)));
    // if(re){

    begin_ = re;
    cap_ = newcap;
    // }
    // it is never gonna fail :)
  }

  void release() {
    if (begin_) {
      free(begin_);
    }
  }

  void copy() {
    if (len_ == 0)
      return podlist_t<T>();

    auto p = podlist_t<T>::make(len_);
    std::memcpy(p.begin_, this->begin_, len_ * sizeof(T));
    return std::move(p);
  }

  podlist_t(podlist_t&& other) noexcept {
    this->begin_ = other.begin_;
    this->len_ = other.len_;
    this->cap_ = other.cap_;

    // other.begin_ = nullptr;
    // other.len_ = 0;
    // other.cap_ = 0;
  }
  // Move assignment operator
  podlist_t& operator=(podlist_t&& other) noexcept {
    if (LLVM_UNLIKELY(this != &other)) {
      this->release();  // Free the current resources
      begin_ = other.begin_;
      len_ = other.len_;
      cap_ = other.cap_;

      // other.begin_ = nullptr;
      // other.len_ = 0;
      // other.cap_ = 0;
    }
    return *this;
  }

  // Delete copy constructor and copy assignment operator
  podlist_t(const podlist_t&) = delete;
  podlist_t& operator=(const podlist_t&) = delete;

  void shrink_to_fit() { resize(len_); }
  auto size() { return len_; }
  auto size_in_bytes() { return len_ * sizeof(T); }
  auto length() { return len_; }
  auto cap() { return cap_; }
  auto cap_in_bytes() { return cap_ * sizeof(T); }
  auto data() { return begin_; }

  auto& back() { return *(end() - 1); }
  auto& front() { return *(begin()); }

#define PODLIST_GROWTH_FACTOR ((cap_ > 50000) ? 2 : 30.0)
  // #define PODLIST_GROWTH_FACTOR 1.1
  void push_back(T& val) {
    if (__builtin_expect((bool)(len_ == cap_), false))
      resize(cap_ * PODLIST_GROWTH_FACTOR + 1);
    memcpy(begin_ + len_, &val, sizeof(T));
    // *(begin_ + len_) = std::move(val);

    ++len_;
  }
  void push_back(T&& val) {
    if (__builtin_expect((bool)(len_ == cap_), false))
      resize(cap_ * PODLIST_GROWTH_FACTOR + 1);
    memcpy(begin_ + len_, &val, sizeof(T));
    ++len_;
  }

  template <typename... Args>
  void append(Args&&... args) {
    constexpr auto arg_len = sizeof...(Args);

    if (__builtin_expect((bool)(len_ + arg_len >= cap_), false))
      resize(cap_ * PODLIST_GROWTH_FACTOR + arg_len);

    auto a = std::array<T, arg_len>{std::move(args)...};
    memcpy(begin_ + len_, a.begin(), sizeof(T) * arg_len);
    len_ += a.size();
  }
  template <typename... Args>
  void append(Args&... args) {
    constexpr auto arg_len = sizeof...(Args);

    if (__builtin_expect((bool)(len_ + arg_len >= cap_), false))
      resize(cap_ * PODLIST_GROWTH_FACTOR + arg_len);

    constexpr auto a = std::array<T, sizeof...(Args)>{std::move(args)...};
    memcpy(begin_ + len_, a.begin(), sizeof(T) * arg_len);
    len_ += a.size();
  }

  // template <typename... Args> void emplace_back(Args &&...args) {
  //   if (__builtin_expect((bool)(len_ == cap_), false))
  //     resize(cap_ * PODLIST_GROWTH_FACTOR);
  //   // memcpy(begin_ + len_, &val, sizeof(T));
  //   begin_[len_] = T{args...};
  //   ++len_;
  // }

  void regress(const len_t rlen) {
    if (len_ < rlen)
      return;
    len_ -= rlen;
  }
  void pop_back() { regress(1); }

  template <bool is_const>
  struct podlist_iterator_t {
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = T;
    using pointer = typename std::conditional<is_const, const T*, T*>::type;
    using reference = typename std::conditional<is_const, const T&, T&>::type;

    explicit podlist_iterator_t(pointer ptr) : ptr_(ptr) {}

    reference operator*() const { return *ptr_; }
    pointer operator->() const { return ptr_; }

    // Pre-increment
    podlist_iterator_t& operator++() {
      ptr_++;
      return *this;
    }

    // Post-increment
    podlist_iterator_t operator++(int) {
      podlist_iterator_t tmp = *this;
      ++(*this);
      return tmp;
    }

    // Pre-decrement
    podlist_iterator_t& operator--() {
      ptr_--;
      return *this;
    }

    // Post-decrement
    podlist_iterator_t operator--(int) {
      podlist_iterator_t tmp = *this;
      --(*this);
      return tmp;
    }

    // Adding/subtracting an offset
    podlist_iterator_t operator+(int offset) const {
      return podlist_iterator_t(ptr_ + offset);
    }
    podlist_iterator_t operator-(int offset) const {
      return podlist_iterator_t(ptr_ - offset);
    }

    // Equality/Inequality comparison
    friend bool operator==(const podlist_iterator_t& a,
                           const podlist_iterator_t& b) {
      return a.ptr_ == b.ptr_;
    }

    friend bool operator!=(const podlist_iterator_t& a,
                           const podlist_iterator_t& b) {
      return a.ptr_ != b.ptr_;
    }

    pointer base() const { return ptr_; }

    podlist_iterator_t& advance(len_t off = 1){
      ptr_ += off;
      return *this;
    }
    
   private:
    pointer ptr_;
  };

  // Define types for both const and non-const iterators
  using iterator = podlist_iterator_t<false>;
  using const_iterator = podlist_iterator_t<true>;

  using it = iterator;
  using c_it = const_iterator;

  // Accessor functions for iterators
  iterator begin() { return iterator(begin_); }
  iterator end() { return iterator(begin_ + len_); }

  const_iterator begin() const { return const_iterator(begin_); }
  const_iterator end() const { return const_iterator(begin_ + len_); }

  const_iterator cbegin() const { return const_iterator(begin_); }
  const_iterator cend() const { return const_iterator(begin_ + len_); }
};
