#include "wrapper.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Triple.h>

extern "C" {

arch_t arch_from_target_triple(const char *target_triple) {
  llvm::Triple triple(target_triple);

  switch (triple.getArch()) {
#define X(name)                                                                \
  case llvm::Triple::name:                                                     \
    return arch_##name;
    ARCH_LIST
#undef X
  default:
    return arch_unknown;
  }
}

int is_msvc(const char *target_triple) {
  llvm::Triple triple(target_triple);

  return triple.getEnvironment() == llvm::Triple::EnvironmentType::MSVC;
}

void add_comment_section(LLVMModuleRef module_ref,
                         const char *compiler_string) {
  llvm::Module *module = llvm::unwrap(module_ref);
  llvm::LLVMContext &context = module->getContext();

  std::string asmText = std::string(".section .comment\n") + ".string \"" +
                        compiler_string + "\"\n";

  llvm::FunctionType *fnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);

  llvm::Function *fn = llvm::Function::Create(
      fnTy, llvm::GlobalValue::InternalLinkage, "__emit_comment", module);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "entry", fn);
  llvm::IRBuilder<> builder(bb);

  llvm::InlineAsm *ia = llvm::InlineAsm::get(fnTy, asmText, "", true);

  builder.CreateCall(ia);
  builder.CreateRetVoid();
}
}
