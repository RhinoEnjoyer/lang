#pragma once

#include "../overloaded.hpp"
#include "../nicknames.hpp"
#include "parser.hpp"
#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string_view>
#include <utility>
#include <variant>
#include <print>

namespace semantics {

struct depth_inh{
  size_t depth = 0;
  template<typename Fn,typename ...T>
  auto dive(Fn fn, T...args){
    depth += 1;
    std::invoke(std::forward<Fn>(fn), std::forward<T>(args)...);
    depth -= 1;
  }
};
struct context_t: public depth_inh {
  parser::node_buffer_t &nodes;
  token_buffer_t &toks;
};

using span_t = parser::node_t::median_proxy_t<false>::span_t;
using cursor_t = span_t::iterator;

template <typename Func>
concept node_fn = requires(Func func, context_t& context, parser::node_t& node) {
    { func(context, node) } -> std::same_as<void>; // Ensures the function returns void
};

template <node_fn Func> void for_each(context_t &ctx, span_t span, Func fn) {
  for (auto &elm : span)
    fn(ctx, elm);
}

void print(context_t &ctx, parser::node_t &elm) {
  const auto depth_str = std::string(3 * ctx.depth, ' ');
  ovisit(elm.node,
      [&](const parser::node_t::final_t &val) {
        std::println("{}final: {}, {}", depth_str, ctx.toks.str(val),
                     token_code_str(val->type_));
      },
      [&](const parser::node_t::median_t &val) {
        std::println("{}median: {}", depth_str, medianc::to_str(val.type_));
        ctx.dive([&] { for_each(ctx, elm.as_median().children(), print); });
      },
      [&](const auto &val) { std::println("Error"); });
}

auto entry(podlist_t<parser::node_t> &nodes,token_buffer_t &toks) -> sptr<locale_t> {
  auto ctx = context_t{{}, nodes, toks};

  auto span = nodes.front().as_median().children();
  for_each(ctx, span, print);
  return nullptr;
}

} // namespace semantics
