/******************************************************************************/
/*                                                                            */
/*                                 OCaml                                      */
/*                                                                            */
/*                           Samuel Hym, Tarides                              */
/*                                                                            */
/*   Copyright 2025 Tarides                                                   */
/*                                                                            */
/* Permission is hereby granted, free of charge, to any person obtaining a    */
/* copy of this software and associated documentation files (the "Software"), */
/* to deal in the Software without restriction, including without limitation  */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,   */
/* and/or sell copies of the Software, and to permit persons to whom the      */
/* Software is furnished to do so, subject to the following conditions:       */
/*                                                                            */
/* The above copyright notice and this permission notice shall be included in */
/* all copies or substantial portions of the Software.                        */
/*                                                                            */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        */
/* DEALINGS IN THE SOFTWARE.                                                  */
/*                                                                            */
/******************************************************************************/

#include <cstdlib>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "OxCamlDemangle.h"

extern "C" value caml_demangle_cxx(value mangled_name) {
  CAMLparam1(mangled_name);
  CAMLlocal1(demangled);
  char *demangled_name = llvm::oxcamlDemangle(String_val(mangled_name));
  if(demangled_name != nullptr) {
    demangled = caml_copy_string(demangled_name);
    std::free(demangled_name);
  } else
    demangled = caml_copy_string("");
  CAMLreturn(demangled);
}
