#ifndef CCHECK_HPP
#define CCHECK_HPP


#include <llvm/Support/raw_ostream.h>
#define CCHECK(...)                                                         \
  (__VA_ARGS__) ? 0                                                   \
                :[]->int{llvm::errs()                                              \
                      << "CHECK failure at " << __FILE__ << ":" << __LINE__ \
                      << ": " #__VA_ARGS__                                  \
                      << '\n';return 0;}


#endif
