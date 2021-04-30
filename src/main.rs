mod codegen;
mod syntax_tree;
mod tokens;

use std::ffi::OsStr;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // The 'first argument' is the path to the executable.
    if args.len() != 2 {
        eprintln!("Expected one input folder: jack-compiler [path-to-dir]");
        std::process::exit(-1);
    }
    // Overly complicated way of getting the filename argument to avoid copying it out of the
    // vector. <sarcasm> it is an absolutely necessary performance measure to make sure the program
    // can be used on consumer-grade CPUs. </sarcasm>
    let dirname = args.into_iter().skip(1).next().unwrap();

    for entry in std::fs::read_dir(&dirname[..]).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension() == Some(OsStr::new("jack")) {
            let file_contents = match std::fs::read_to_string(&path) {
                Ok(contents) => contents,
                Err(err) => {
                    eprintln!(
                        "Failed to open input file '{}', caused by:\n{}",
                        dirname, err
                    );
                    std::process::exit(-1);
                }
            };

            let classes = match syntax_tree::parse(
                &file_contents[..],
                &path.file_name().unwrap().to_string_lossy(),
            ) {
                Ok(data) => data,
                Err(err) => {
                    eprintln!("\n\n{}", err);
                    std::process::exit(-1);
                }
            };

            let base_output_path = path.with_extension("vm");
            let expected_class_name = base_output_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .replace(".vm", "");
            for class in classes {
                let vm_code = match codegen::generate_code(&class) {
                    Ok(v) => v,
                    Err(err) => {
                        eprintln!(
                            "\n\nEncountered while processing class '{}':\n{}",
                            class.name, err
                        );
                        std::process::exit(-1);
                    }
                };
                let output_path = if class.name == expected_class_name {
                    base_output_path.clone()
                } else {
                    base_output_path
                        .with_file_name(format!("{}.{}.vm", expected_class_name, class.name))
                };
                // println!("{:?}:\n{}", output_path, vm_code);
                if let Err(err) = std::fs::write(&output_path, vm_code) {
                    eprintln!(
                        "ERROR: Failed to write generated code to '{:?}', caused by:\n{}",
                        output_path, err
                    );
                    std::process::exit(-1);
                }
            }
        }
    }
}
