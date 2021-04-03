#![feature(format_args_capture)]

fn main() {
    const BUILTINS: &str = "walrus_builtins";
    println!("cargo:rerun-if-changed=src/{BUILTINS}.c");
    cc::Build::new()
        .file(format!("src/{BUILTINS}.c"))
        .no_default_flags(true)
        .warnings(false)
        .compile(BUILTINS);
}
