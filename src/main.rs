mod syntax_tree;
mod tokens;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // The 'first argument' is the path to the executable.
    if args.len() != 2 {
        eprintln!("Expected one input file: jack-compiler [filename.jack]");
        std::process::exit(-1);
    }
    // Overly complicated way of getting the filename argument to avoid copying it out of the
    // vector. <sarcasm> it is an absolutely necessary performance measure to make sure the program
    // can be used on consumer-grade CPUs. </sarcasm>
    let filename = args.into_iter().skip(1).next().unwrap();

    let file_contents = match std::fs::read_to_string(&filename[..]) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!(
                "Failed to open input file '{}', caused by:\n{}",
                filename, err
            );
            std::process::exit(-1);
        }
    };

    match syntax_tree::parse(&file_contents[..], &filename[..]) {
        Ok(program) => println!("{:#?}", program),
        Err(err) => {
            eprintln!("\n\n{}", err);
            std::process::exit(-1);
        }
    }
}
