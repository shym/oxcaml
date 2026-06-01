#!/bin/sh

# Generate the body of `derived_system`:
regexp='.*\<arch=([^;]*); (model=([^;]*); )?system=([^]]*)].*'

sed -En 's/'"$regexp"'/\1, "\3", "\4" -> \4/p' configure.ac \
  | sed -e 's/""/"default"/' \
        -e 's/beos$/BeOS/' \
        -e 's/cygwin$/Cygwin/' \
        -e 's/dragonfly$/Dragonfly/' \
        -e 's/freebsd$/FreeBSD/' \
        -e 's/gnu$/GNU/' \
        -e 's/linux$/Linux/' \
        -e 's/macosx$/MacOS_like/' \
        -e 's/mingw$/MinGW_32/' \
        -e 's/mingw64$/MinGW_64/' \
        -e 's/netbsd$/NetBSD/' \
        -e 's/openbsd$/OpenBSD/' \
        -e 's/solaris$/Solaris/' \
        -e 's/win32$/Win32/' \
        -e 's/win64$/Win64/' \
        -e 's/^amd64,/  | X86_64,/' \
        -e 's/^arm,/  | ARM,/' \
        -e 's/^arm64,/  | AArch64,/' \
        -e 's/^i386,/  | IA32,/' \
        -e 's/^power,/  | POWER,/' \
        -e 's/^riscv,/  | Riscv,/' \
        -e 's/^s390x,/  | Z,/' \
  | sort | uniq

# The sort | uniq is necessary because there are cases such as ARMv8 Linux that
# appear twice at different places
