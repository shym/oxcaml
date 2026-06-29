#define MATCH_TARGET(PAT,NATIVE,ARCH,MODEL,SYSTEM) \
  [[PAT]], \
    [has_native_backend=NATIVE; arch=ARCH; model=MODEL; system=SYSTEM],
#define MATCH_TARGET_AND_64(PAT,ARCH64,MODEL64,ARCH32,MODEL32,SYSTEM) \
  [[PAT]], \
    [AS_IF([$arch64], \
      [has_native_backend=yes; arch=ARCH64; model=MODEL64; system=SYSTEM], \
      [has_native_backend=no; arch=ARCH32; model=MODEL32; system=SYSTEM])],
#define DEFAULT(NATIVE,ARCH,MODEL,SYSTEM) \
  [has_native_backend=NATIVE; arch=ARCH; model=MODEL; system=SYSTEM]

AS_CASE([$target],
#include "../utils/target_system_cases.tbl"
)
