#include "wrapper.h"

#include <llvm/Config/llvm-config.h>
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
    ARCH_LIST_COMMON
#if LLVM_VERSION_MAJOR >= 23
    ARCH_LIST_LLVM_23
#endif
#undef X

#if LLVM_VERSION_MAJOR >= 23
  case Triple::amdgpu:
#else
  case Triple::amdgcn:
#endif
    return arch_amdgpu;

  default:
    return arch_unknown;
  }
}

int is_msvc(const char *target_triple) {
  Triple triple(target_triple);

  return triple.getEnvironment() == Triple::EnvironmentType::MSVC;
}
}
