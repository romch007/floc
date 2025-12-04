use std::ffi::CString;

mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(unused)]
    #![allow(clippy::upper_case_acronyms)]
    #![allow(clippy::enum_variant_names)]

    include!(concat!(env!("OUT_DIR"), "/llvm_wrapper.rs"));
}

macro_rules! define_arch_enum {
    (
        from: $enum_from:ty,
        name: $enum_name:ident,
        archs: [$($variant:ident),*],
        unknown_field: $unknown_field:ident
    ) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        #[allow(non_camel_case_types)]
        pub enum $enum_name {
            $($variant),*,
        }

        impl $enum_name {
            pub fn from_wrapper(arch: $enum_from) -> Option<Self> {
                match arch {
                    $( <$enum_from>::$variant => Some(<$enum_name>::$variant), )*
                    <$enum_from>::$unknown_field => None,
                }
            }
        }
    };
}

define_arch_enum!(
    from: sys::arch_t,
    name: Arch,
    archs: [
        arm,
        armeb,
        aarch64,
        aarch64_be,
        aarch64_32,
        arc,
        avr,
        bpfel,
        bpfeb,
        csky,
        dxil,
        hexagon,
        loongarch32,
        loongarch64,
        m68k,
        mips,
        mipsel,
        mips64,
        mips64el,
        msp430,
        ppc,
        ppcle,
        ppc64,
        ppc64le,
        r600,
        amdgcn,
        riscv32,
        riscv64,
        sparc,
        sparcv9,
        sparcel,
        systemz,
        tce,
        tcele,
        thumb,
        thumbeb,
        x86,
        x86_64,
        xcore,
        xtensa,
        nvptx,
        nvptx64,
        amdil,
        amdil64,
        hsail,
        hsail64,
        spir,
        spir64,
        spirv,
        spirv32,
        spirv64,
        kalimba,
        shave,
        lanai,
        wasm32,
        wasm64,
        renderscript32,
        renderscript64,
        ve
    ],
    unknown_field: unknown
);

pub fn get_arch_from_target_triple(target_triple: &str) -> Option<Arch> {
    let target_triple = CString::new(target_triple).unwrap();

    let ret = unsafe { sys::arch_from_target_triple(target_triple.as_ptr()) };

    Arch::from_wrapper(ret)
}

pub fn is_msvc(target_triple: &str) -> bool {
    let target_triple = CString::new(target_triple).unwrap();

    let ret = unsafe { sys::is_msvc(target_triple.as_ptr()) };

    ret != 0
}

pub fn add_comment_section(module: &inkwell::module::Module, compiler_string: &str) {
    let compiler_string = CString::new(compiler_string).unwrap();
    let module_ptr = module.as_mut_ptr();

    unsafe { sys::add_comment_section(module_ptr as *mut _, compiler_string.as_ptr()) };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_target_triple_arch() {
        assert_eq!(
            get_arch_from_target_triple("aarch64-apple-darwin").unwrap(),
            Arch::aarch64,
        );

        assert_eq!(
            get_arch_from_target_triple("x86_64-unknown-linux-gnu").unwrap(),
            Arch::x86_64,
        );
    }

    #[test]
    fn target_triple_is_msvc() {
        assert!(is_msvc("x86_64-pc-windows-msvc"));
        assert!(!is_msvc("x86_64-pc-windows-gnu"));
    }
}
