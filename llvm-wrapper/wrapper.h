#ifndef WRAPPER_H
#define WRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

#define ARCH_LIST \
  X(arm) \
  X(armeb) \
  X(aarch64) \
  X(aarch64_be) \
  X(aarch64_32) \
  X(arc) \
  X(avr) \
  X(bpfel) \
  X(bpfeb) \
  X(csky) \
  X(dxil) \
  X(hexagon) \
  X(loongarch32) \
  X(loongarch64) \
  X(m68k) \
  X(mips) \
  X(mipsel) \
  X(mips64) \
  X(mips64el) \
  X(msp430) \
  X(ppc) \
  X(ppcle) \
  X(ppc64) \
  X(ppc64le) \
  X(r600) \
  X(amdgcn) \
  X(riscv32) \
  X(riscv64) \
  X(sparc) \
  X(sparcv9) \
  X(sparcel) \
  X(systemz) \
  X(tce) \
  X(tcele) \
  X(thumb) \
  X(thumbeb) \
  X(x86) \
  X(x86_64) \
  X(xcore) \
  X(xtensa) \
  X(nvptx) \
  X(nvptx64) \
  X(amdil) \
  X(amdil64) \
  X(hsail) \
  X(hsail64) \
  X(spir) \
  X(spir64) \
  X(spirv) \
  X(spirv32) \
  X(spirv64) \
  X(kalimba) \
  X(shave) \
  X(lanai) \
  X(wasm32) \
  X(wasm64) \
  X(renderscript32) \
  X(renderscript64) \
  X(ve)

typedef enum {
#define X(name) arch_##name,
  ARCH_LIST
#undef X
  arch_unknown
} arch_t;

arch_t arch_from_target_triple(const char *target_triple);

int is_msvc(const char* target_triple);

#ifdef __cplusplus
}
#endif

#endif
