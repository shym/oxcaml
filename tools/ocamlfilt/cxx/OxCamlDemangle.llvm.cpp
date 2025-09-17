//===--- OxCamlDemangle.cpp -------------------------------------*- C++ -*-===//
//
// Demangler for the new OxCaml mangling scheme in C++ for inclusion in LLDB
//
// SPDX-License-Identifier: MIT
// Copyright 2025 Tarides
//
//===----------------------------------------------------------------------===//

#include <cassert>

#include "llvm/Demangle/Demangle.h"
#include "llvm/Demangle/StringView.h"
#include "llvm/Demangle/Utility.h"

using llvm::itanium_demangle::OutputBuffer;
using llvm::itanium_demangle::StringView;

#define ERROR (~((unsigned)0))

static unsigned ConsumeUnsignedDecimal(StringView& sv) {
  unsigned res = 0, i = 0;
  while(sv[i] >= '0' && sv[i] <= '9') {
    res = res * 10 + (sv[i] - '0');
    i++;
  }
  sv = sv.dropFront(i);
  if(i == 0)
    return ERROR;
  return res;
}

static unsigned ConsumeUnsigned26(StringView& sv) {
  unsigned res = 0, i = 0;
  while(sv[i] >= 'A' && sv[i] <= 'Z') {
    res = res * 26 + (sv[i] - 'A');
    i++;
  }
  sv = sv.dropFront(i);
  if(i == 0)
    return ERROR;
  return res;
}

static bool islowerhex(char c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
}

static unsigned lowerhex(char c) {
  if(c >= '0' && c <= '9')
    return c - '0';
  else {
    assert(c >= 'a' && c <= 'f');
    return c - 'a' + 10;
  }
}

char *llvm::oxcamlDemangle(const char *MangledName) {
  StringView Mangled(MangledName);
  if(!Mangled.consumeFront("_O"))
    return nullptr;

  // Allocate the buffer at a reasonable size, as OutputBuffer allocates 992
  // bytes when starting from an empty buffer
  char *DemangledBuffer;
  DemangledBuffer = static_cast<char *>(std::malloc(Mangled.size()));
  if (DemangledBuffer == nullptr)
    std::terminate();
  OutputBuffer Demangled(DemangledBuffer, Mangled.size());

#define ENDONERROR() do {           \
  std::free(Demangled.getBuffer()); \
  return nullptr;                   \
} while(0)

  if(Mangled.consumeFront('N')) {
    // Named symbol
    while(!Mangled.empty()) {
      if(Mangled.consumeFront('u')) {
        if(!Demangled.empty())
          Demangled << '.';
        unsigned len = ConsumeUnsignedDecimal(Mangled);
        if(len == ERROR || len <= 0 || len > Mangled.size())
          ENDONERROR();
        size_t split = Mangled.find('_');
        if(split >= len) ENDONERROR();
        StringView coded = Mangled.substr(0, split);
        StringView raw = Mangled.substr(split+1, len-split-1);
        while(!coded.empty()) {
          unsigned chunklen = ConsumeUnsigned26(coded);
          if(chunklen == ERROR || chunklen > raw.size())
            ENDONERROR();
          Demangled << raw.substr(0,chunklen);
          raw = raw.dropFront(chunklen);
          unsigned i;
          for(i = 0; i+1 < coded.size() && islowerhex(coded[i]); i+=2) {
            if(!islowerhex(coded[i+1]))
              ENDONERROR();
            char c = (char)(lowerhex(coded[i]) << 4 | lowerhex(coded[i+1]));
            Demangled << c;
          }
          coded = coded.dropFront(i);
        }
        if(!raw.empty())
          Demangled << raw;
        Mangled = Mangled.dropFront(len);
      } else if(!Mangled.consumeFront('_')) {
        if(!Demangled.empty())
          Demangled << '.';
        unsigned len = ConsumeUnsignedDecimal(Mangled);
        if(len == ERROR || len <= 0 || len > Mangled.size())
          ENDONERROR();
        Demangled << Mangled.substr(0, len);
        Mangled = Mangled.dropFront(len);
      } else {
        // we are on the _ that separates the symbol per se from its unique id,
        // so we have nothing left to do
        break;
      }
    }
  } else {
    // Anonymous symbol
    if(!Mangled.consumeFront('A'))
      ENDONERROR();
    char *demangled_name = static_cast<char *>(std::malloc(sizeof("anonymous")));
    std::memcpy(demangled_name, "anonymous", sizeof("anonymous"));
    return demangled_name;
  }

  Demangled << '\0';

  return Demangled.getBuffer();
}
