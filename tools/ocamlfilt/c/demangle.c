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

#include <string.h>
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* Maximal length of a symbol */
#define SYMBOL_MAX (1024*1024)
#define ERROR (~((unsigned)0))

/* Decode the decimal integer at *pos in sym
   Require a non-empty integer to appear
   Leave *pos to the first byte after the integer */
static unsigned decode_decimal(const char *sym, size_t *pos) {
  unsigned res = 0;
  size_t p = *pos;
  while (sym[p] >= '0' && sym[p] <= '9') {
    if(res > SYMBOL_MAX)
      return ERROR;
    res = res * 10 + (sym[p] - '0');
    p++;
  }
  if(*pos == p)
    // No digit was found
    return ERROR;
  *pos = p;
  return res;
}

static unsigned decode_26(const char *sym, size_t *pos) {
  unsigned res = 0;
  size_t p = *pos;
  while (sym[p] >= 'A' && sym[p] <= 'Z') {
    if(res > SYMBOL_MAX)
      return ERROR;
    res = res * 26 + (sym[p] - 'A');
    p++;
  }
  if(*pos == p)
    // No digit was found
    return ERROR;
  *pos = p;
  return res;
}

static int is_hex(char c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
}

static int hex(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  else
    return c - 'a' + 10;
}

char *demangle_ocaml_v0(const char *sym) {
  char *outbuf, *tmp;
  size_t sympos, outpos, codedpos, endpos, len, l;
  unsigned raw;

  if(sym[0] != '_' || sym[1] != 'O')
    return NULL;

  sympos = 2;
  outpos = 0;
  len = strlen(sym);

  switch(sym[sympos++]) {
    case 'N':
      /* The result is always shorter than the encoded symbol */
      /* Is it worth to compute the precise length instead? */
      outbuf = malloc(len + 1);
      if(!outbuf)
        return NULL;

#define ENDONERROR() do { \
  free(outbuf);           \
  return NULL;            \
} while(0)

      while (sympos < len) {
        if (sym[sympos] == 'u') {
          sympos++;
          if (outpos)
            outbuf[outpos++] = '.';
          l = decode_decimal(sym, &sympos);
          if(l == ERROR || l == 0 || sympos + l > len) ENDONERROR();
          codedpos = sympos;
          endpos = sympos + l;
          tmp = strchr(sym + sympos, '_');
          if(!tmp) ENDONERROR();
          sympos = (size_t)(tmp - sym + 1);
          if(sympos > endpos) ENDONERROR();
          while(sym[codedpos] != '_') {
            raw = decode_26(sym, &codedpos);
            if(raw == ERROR || sympos + raw > endpos) ENDONERROR();
            tmp = stpncpy(outbuf + outpos, sym + sympos, raw);
            sympos += raw;
            outpos += raw;
            if((size_t) (tmp - outbuf) != outpos) ENDONERROR();
            while(is_hex(sym[codedpos])) {
              if(!is_hex(sym[codedpos+1])) ENDONERROR();
              outbuf[outpos++] = hex(sym[codedpos]) << 4 | hex(sym[codedpos+1]);
              codedpos += 2;
            }
          }
          if(sympos < endpos) {
            tmp = stpncpy(outbuf + outpos, sym + sympos, endpos - sympos);
            outpos += endpos - sympos;
            sympos = endpos;
            if((size_t) (tmp - outbuf) != outpos) ENDONERROR();
          }
        } else if (sym[sympos] != '_') {
          if (outpos)
            outbuf[outpos++] = '.';
          l = decode_decimal(sym, &sympos);
          if(l == ERROR || l == 0 || sympos + l > len) ENDONERROR();
          tmp = stpncpy(outbuf + outpos, sym + sympos, l);
          sympos += l;
          outpos += l;
          if((size_t) (tmp - outbuf) != outpos) ENDONERROR();
        } else {
          // we are on the _ that separates the symbol per se from its unique
          // id, so we have nothing left to do in that loop
          break;
        }
      }
      outbuf[outpos] = '\0';
      break;
    case 'A':
      outbuf = strdup("anonymous");
      break;
    default:
      return NULL;
  }

  return outbuf;
}

value caml_demangle_c(value mangled_name) {
  CAMLparam1(mangled_name);
  CAMLlocal1(demangled);
  char *demangled_name = demangle_ocaml_v0(String_val(mangled_name));
  if (demangled_name) {
    demangled = caml_copy_string(demangled_name);
    free(demangled_name);
  } else
    demangled = caml_copy_string("");
  CAMLreturn(demangled);
}
