use anyon::{
    build::{execute_build_or_run, BuildMode},
    error::AnyonError,
    new::execute_new,
};
use clap::{Parser, Subcommand};
use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;

#[derive(Parser)]
#[command(name = "Anyon")]
#[command(version, about = "Jarvil's Package Manager and Build System")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    New { project_name: String },
    Build,
    Run,
}

fn execute_cmd(commands: &Commands) -> Result<(), AnyonError> {
    match commands {
        Commands::New { project_name } => execute_new(project_name),
        Commands::Build => execute_build_or_run(BuildMode::Build),
        Commands::Run => execute_build_or_run(BuildMode::Run),
        // Commands::Version => execute_version(),
    }
}

fn main() {
    // hook for styling of the error messages
    miette::set_hook(Box::new(|_err| {
        let mut my_theme = GraphicalTheme::default();
        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));

    let cli = Cli::parse();
    let Some(commands) = &cli.command else { return };
    if let Err(err) = execute_cmd(commands) {
        println!("{:?}", err);
    }
}
