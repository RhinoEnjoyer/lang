#include "mempool.hpp"
#include <iostream>
#include <print>

// for testing
int main() {
  auto a = podlist_t<int>::make(1, 2, 3, 4, 5);

  std::cout << a.length() << " " << a.cap() << std::endl;
  for (const auto &elm : a) {
    std::cout << elm << std::endl;
  }
  std::cout << std::endl;

  auto b = podlist_t<int>::make(44, 45, 46, 77, 55, 56, 57, 58, 59, 60, 61, 52,
                                63, 64, 65, 66, 69);

  // a.insert(1, b.view());

  std::cout << a.length() << " " << a.cap() << std::endl;
  for (const auto &elm : a) {
    std::cout << elm << std::endl;
  }
  //
  //
  auto pool = mempool_t::make();

  auto i1 = pool.alloc<int>();
  auto i2 = pool.alloc<int>();
  auto i3 = pool.alloc<long long>();

  *i1 = 14;
  *i2 = 21;
  *i3 = 33;

  std::println("{}, {}, {}", *i1, *i2, *i3);
}
