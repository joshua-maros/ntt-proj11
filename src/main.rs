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

    let mut classes = Vec::new();
    for entry in std::fs::read_dir(&dirname[..]).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        println!("{:?}", path);
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
            match syntax_tree::parse(
                &file_contents[..],
                &path.file_name().unwrap().to_string_lossy(),
            ) {
                Ok(mut data) => classes.append(&mut data),
                Err(err) => {
                    eprintln!("\n\n{}", err);
                    std::process::exit(-1);
                }
            }
        }
    }

    println!("{:#?}", classes);
}
