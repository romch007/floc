#include "wrapper.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Triple.h>

extern "C" {

using namespace llvm;

arch_t arch_from_target_triple(const char *target_triple) {
  Triple triple(target_triple);

  switch (triple.getArch()) {
#define X(name)                                                                \
  case Triple::name:                                                           \
    return arch_##name;
    ARCH_LIST
#undef X
  default:
    return arch_unknown;
  }
}

int is_msvc(const char *target_triple) {
  Triple triple(target_triple);

  return triple.getEnvironment() == Triple::EnvironmentType::MSVC;
}
}
