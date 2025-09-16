//===--- OxCamlDemangle.h ---------------------------------------*- C++ -*-===//
//
// Demangler for the new OxCaml mangling scheme in C++ for inclusion in LLDB
//
// SPDX-License-Identifier: MIT
// Copyright 2025 Tarides
//
//===----------------------------------------------------------------------===//

namespace llvm {
  char *oxcamlDemangle(const char*);
};
