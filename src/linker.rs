use cc::windows_registry;
use miette::{Context, IntoDiagnostic, bail};
use std::{
    ffi::{OsStr, OsString},
    path::Path,
    process::{Command, Stdio},
};

use crate::utils;

macro_rules! os {
    ($val:expr) => {{ OsStr::new($val) }};
}

pub fn link_msvc(
    object_file: &Path,
    output_file: &Path,
    link_static: bool,
    arch: &utils::Arch,
    verbose: bool,
) -> miette::Result<()> {
    let msvc_arch = match arch {
        utils::Arch::x86_64 => "x64",
        utils::Arch::x86 => "x86",
        utils::Arch::aarch64 => "arm64",
        _ => bail!("invalid arch {arch:?} for MSVC"),
    };

    if verbose {
        eprintln!("using MSVC arch {msvc_arch}");
    }

    let linker = windows_registry::find_tool(msvc_arch, "link.exe")
        .wrap_err_with(|| format!("cannot find link.exe for arch {msvc_arch}"))?;

    let mut out_arg = OsString::from("/OUT:");
    out_arg.push(output_file.as_os_str());

    let args = [
        object_file.as_os_str(),
        os!("legacy_stdio_definitions.lib"),
        if link_static {
            os!("libcmt.lib")
        } else {
            os!("msvcrt.lib")
        },
        os!("/SUBSYSTEM:CONSOLE"),
        out_arg.as_os_str(),
    ];

    if verbose {
        eprintln!("calling {} with args {args:?}", linker.path().display());
    }

    let stdio = || {
        if verbose {
            Stdio::inherit()
        } else {
            Stdio::null()
        }
    };

    let res = linker
        .to_command()
        .args(args)
        .stdout(stdio())
        .stderr(stdio())
        .status()
        .into_diagnostic()
        .wrap_err("link.exe failed to start")?;

    if !res.success() {
        bail!("link.exe returned a non-zero exit code");
    }

    Ok(())
}

const LINK_WRAPPER: &str = "cc";

pub fn link_cc(
    object_file: &Path,
    output_file: &Path,
    link_static: bool,
    linker: Option<&str>,
    verbose: bool,
) -> miette::Result<()> {
    let mut args = vec![
        object_file.as_os_str().to_owned(),
        os!("-o").to_owned(),
        output_file.as_os_str().to_owned(),
    ];

    if link_static {
        args.push(os!("-static").into());
    }

    if let Some(linker) = linker {
        let mut arg = os!("-fuse-ld=").to_owned();
        arg.push(linker);

        args.push(arg);
    }

    if verbose {
        eprintln!("calling {LINK_WRAPPER} with args {args:?}");
    }

    let res = Command::new(LINK_WRAPPER)
        .args(args)
        .stdout(if verbose {
            Stdio::inherit()
        } else {
            Stdio::null()
        })
        .stderr(Stdio::inherit())
        .status()
        .into_diagnostic()
        .wrap_err_with(|| format!("`{LINK_WRAPPER}` failed to start"))?;

    if !res.success() {
        bail!("`{LINK_WRAPPER}` returned a non-zero exit code");
    }

    Ok(())
}
