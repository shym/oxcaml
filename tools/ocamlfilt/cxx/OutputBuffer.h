//===--- Utility.h ----------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Provide some utility classes for use in the demangler.
// There are two copies of this file in the source tree.  The one in libcxxabi
// is the original and the one in llvm is the copy.  Use cp-to-llvm.sh to update
// the copy.  See README.txt for more details.
//
//===----------------------------------------------------------------------===//

#include "StringView.h"
#include <array>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <limits>
#include <string>

// Stream that AST nodes write their string representation into after the AST
// has been parsed.
class OutputBuffer {
  char *Buffer = nullptr;
  size_t CurrentPosition = 0;
  size_t BufferCapacity = 0;

  // Ensure there are at least N more positions in the buffer.
  void grow(size_t N) {
    size_t Need = N + CurrentPosition;
    if (Need > BufferCapacity) {
      // Reduce the number of reallocations, with a bit of hysteresis. The
      // number here is chosen so the first allocation will more-than-likely not
      // allocate more than 1K.
      Need += 1024 - 32;
      BufferCapacity *= 2;
      if (BufferCapacity < Need)
        BufferCapacity = Need;
      Buffer = static_cast<char *>(std::realloc(Buffer, BufferCapacity));
      if (Buffer == nullptr)
        std::terminate();
    }
  }


public:
  OutputBuffer(char *StartBuf, size_t Size)
      : Buffer(StartBuf), BufferCapacity(Size) {}
  OutputBuffer(char *StartBuf, size_t *SizePtr)
      : OutputBuffer(StartBuf, StartBuf ? *SizePtr : 0) {}
  OutputBuffer() = default;
  // Non-copyable
  OutputBuffer(const OutputBuffer &) = delete;
  OutputBuffer &operator=(const OutputBuffer &) = delete;

  operator StringView() const { return StringView(Buffer, CurrentPosition); }

  OutputBuffer &operator+=(StringView R) {
    if (size_t Size = R.size()) {
      grow(Size);
      std::memcpy(Buffer + CurrentPosition, R.begin(), Size);
      CurrentPosition += Size;
    }
    return *this;
  }

  OutputBuffer &operator+=(char C) {
    grow(1);
    Buffer[CurrentPosition++] = C;
    return *this;
  }

  OutputBuffer &operator<<(StringView R) { return (*this += R); }

  OutputBuffer &operator<<(char C) { return (*this += C); }

  size_t getCurrentPosition() const { return CurrentPosition; }
  void setCurrentPosition(size_t NewPos) { CurrentPosition = NewPos; }

  bool empty() const { return CurrentPosition == 0; }

  char *getBuffer() { return Buffer; }
  char *getBufferEnd() { return Buffer + CurrentPosition - 1; }
  size_t getBufferCapacity() const { return BufferCapacity; }
};

#if 0

#include <iostream>

int main() {
  OutputBuffer OB;
  OB << "message" << '\n' << '\0';
  std::cout << OB.getBuffer();
  return 0;
}

#endif
