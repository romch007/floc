use cc::windows_registry;
use miette::{bail, Context, IntoDiagnostic};
use std::{
    ffi::{OsStr, OsString},
    path::Path,
    process::{Command, Stdio},
};

macro_rules! os {
    ($val:expr) => {{
        OsStr::new($val)
    }};
}

pub fn link_msvc(object_file: &Path, output_file: &Path) -> miette::Result<()> {
    let linker = windows_registry::find_tool("x64", "link.exe").wrap_err("cannot find link.exe")?;

    let mut out_arg = OsString::from("/OUT:");
    out_arg.push(output_file.as_os_str());

    let args = [
        object_file.as_os_str(),
        os!("legacy_stdio_definitions.lib"),
        os!("msvcrt.lib"),
        os!("/SUBSYSTEM:CONSOLE"),
        out_arg.as_os_str(),
    ];

    let res = linker
        .to_command()
        .args(args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .into_diagnostic()
        .wrap_err("link.exe failed to start")?;

    if !res.success() {
        bail!("link.exe returned a non-zero exit code");
    }

    Ok(())
}

pub fn link_cc(object_file: &Path, output_file: &Path) -> miette::Result<()> {
    let args = [object_file.as_os_str(), os!("-o"), output_file.as_os_str()];

    let res = Command::new("cc")
        .args(args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .into_diagnostic()
        .wrap_err("`cc` failed to start")?;

    if !res.success() {
        bail!("`cc` returned a non-zero exit code");
    }

    Ok(())
}
