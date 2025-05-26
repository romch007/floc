#include "wrapper.h"

#include <llvm/TargetParser/Triple.h>

extern "C" {

arch_t arch_from_target_triple(const char* target_triple) {
  llvm::Triple triple(target_triple);

  switch (triple.getArch()) {
  #define X(name) case llvm::Triple::name: return arch_##name;
    ARCH_LIST
  #undef X
    default:
      return arch_unknown;
  }
}

}
