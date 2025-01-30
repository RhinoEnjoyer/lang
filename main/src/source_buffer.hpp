#pragma once

#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/VirtualFileSystem.h>

using source_index_t = std::size_t;

struct src_buffer_t {
  using bufptr_t = std::unique_ptr<llvm::MemoryBuffer>;
  bufptr_t buffer_;

  std::size_t length() const { return buffer_->getBufferSize(); }
  src_buffer_t(bufptr_t ptr) : buffer_(std::move(ptr)) {}

  static auto make(llvm::vfs::FileSystem *const fs, const std::string &filepath)
      -> llvm::ErrorOr<src_buffer_t> {
    auto buf = fs->getBufferForFile(filepath);
    if (!buf)
      return buf.getError();
    return std::move(buf.get());
  }

  auto ref(const source_index_t index, const std::size_t length)
      -> const llvm::StringRef {
    return llvm::StringRef(buffer_->getBufferStart() + index, length);
  }

  auto buffer() const { return buffer_.get()->getBuffer(); }
};
