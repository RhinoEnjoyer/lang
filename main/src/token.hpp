#pragma once

#include "../libs/podlist.hpp"
#include "./source_buffer.hpp"
#include "./token_enum.hpp"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <stddef.h>
#include <stdint.h>

template <typename T> using vec = podlist_t<T>;

struct srcloc_t {
  std::int32_t line_;
  std::int32_t length_;
  std::int64_t index_; // might change this to std::uint64_t
};

// How are you gonna do 65k layers of depth?
using depth_t = std::int32_t;

struct token_t {
  tokc::e type_;

  template <typename... Types>
  [[nodiscard]] auto isa(Types... types) const -> bool {
    return ((type_ == types) || ...);
  }
};

// srcloc_t = 16 bytes each
// token_t  = 4 bytes each

struct token_buffer_t {
  vec<token_t> toks;
  vec<srcloc_t> locs;
  const src_buffer_t *src;

  auto to_index(vec<token_t>::c_it it) const -> std::size_t {
    return it.base() - this->toks.begin().base();
  }
  auto to_index(vec<token_t>::it it) const -> std::size_t {
    return it.base() - toks.begin().base();
  }

  auto total_size_in_bytes() {
    return toks.size_in_bytes() + locs.size_in_bytes();
  }
  // auto token_depth(token_buffer_index_t index) { return
  // toks.at(index).depth_; }
  auto type(vec<token_t>::c_it it) const { return it->type_; }
  auto len(vec<token_t>::c_it it) const { return locs.at(to_index(it)).length_; }
  auto row(vec<token_t>::c_it it) const {
    return locs.at(to_index(it)).line_;
  }
  auto col(vec<token_t>::c_it it) const {
    const char *block = src->buffer().begin() + locs.at(to_index(it)).index_;
    while (block != src->buffer().begin() && *block != '\n')
      --block;

    std::size_t prev_nl = block - src->buffer().begin();
    return locs.at(to_index(it)).index_ - prev_nl;
  }
  std::string_view str(vec<token_t>::c_it it) const {
    return std::string_view(src->buffer().begin() + locs.at(to_index(it)).index_,
                            locs.at(to_index(it)).length_);
  }
};
