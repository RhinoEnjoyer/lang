#pragma once

#include "./podlist.hpp"
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>

// if one allocation doesn't fit
//  it will cause some dead space to be left on the previous page
//  and for now we do not go back

struct page_t {
  void *data;
  size_t cap;
  void *current;
  // size_t allocated;

  static page_t make(size_t cap) {
    auto ptr = new std::uint8_t[cap];
    return {ptr, cap, ptr};
  }

  void release() {
    delete[] static_cast<std::byte *>(data);
  }

  std::byte * begin_addr() const { return static_cast<std::byte *>(data); }
  std::byte * end_addr() const { return begin_addr() + cap - 1; }
  std::byte * current_addr() const { return static_cast<std::byte *>(current); }

  size_t remaining_cap() const { return end_addr() - current_addr(); }
  size_t allocated_size() const { return current_addr() - begin_addr(); }

  static bool is_aligned(const void *const ptr, const size_t alignment) {
    return (reinterpret_cast<uintptr_t>(ptr) & (alignment - 1)) == 0;
  }

  void *alloc(const size_t alignment, const size_t bytesize) {
    auto voidptr = (void *)current_addr();
    auto space = remaining_cap();
    if (std::align(alignment, bytesize, voidptr, space)) [[likely]] {
      void *result = voidptr;
      current = static_cast<std::byte *>(voidptr) + bytesize;
      return result;
    }
    return nullptr;
  }

  template <typename T> T *alloc() {
    auto ptr = (T *)alloc(alignof(T), sizeof(T));
    return ptr;
  }
};

struct mempool_t {
  using page_list = podlist_t<page_t>;
  page_list pages;
  std::vector<std::function<void()>> destructors;

  static mempool_t make() {
    auto list = page_list::create(10);
    list.push_back(page_t::make(1024));
    return {std::move(list), {}};
  }

  size_t capacity_size() const {
    size_t s = 0;
    for (auto &page : pages)
      s += page.cap;
    return s;
  }
  size_t allocated_size() const {
    size_t s = 0;
    for (const auto &page : pages)
      s += page.allocated_size();
    return s;
  }
  size_t pool_size() const { return pages.size(); }

  void release() {
    for (auto &d : destructors)
      d();
    for (auto &page : pages)
      page.release();
    pages.release();
  }
  auto &top_page() { return pages.back(); }
  const auto &top_page() const { return pages.back(); }

  void make_new_page() {
    pages.push_back(page_t::make(4096 * (pages.size() + 1)));
  }

  template <typename T, typename ...Args> 
  T *alloc(Args&&... args) {
      using U = std::remove_reference_t<T>;
      
      auto& page = top_page();
      auto ptr = page.alloc<U>();
  
      if (!ptr) {
          make_new_page();
          return alloc<T>(std::forward<Args>(args)...);
      }

      if constexpr (sizeof...(Args) > 0) {
        new (ptr) U(std::forward<Args>(args)...);
      } else {
        new (ptr) U();
      }

      destructors.push_back([ptr]() { ptr->~U(); });
  
      return ptr;
  }};
