//! The `anyon` command line entry point.
//!
//! Parses arguments, configures how diagnostics are rendered, and exits
//! non-zero when a command fails so the tool composes with scripts and CI.

use anyon::{
    build::{BuildMode, execute_build_or_run},
    error::AnyonError,
    new::execute_new,
};
use clap::{Parser, Subcommand};
use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;
use std::io::IsTerminal;

// Mirrors the decision `miette::GraphicalTheme::default()` makes, so our own
// styling is applied on exactly the runs where miette itself chose to colourise.
fn use_color() -> bool {
    if !std::io::stdout().is_terminal() || !std::io::stderr().is_terminal() {
        return false;
    }

    !matches!(std::env::var("NO_COLOR"), Ok(value) if value != "0")
}

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
    }
}

fn main() {
    // hook for styling of the error messages
    let _ = miette::set_hook(Box::new(|_err| {
        // `GraphicalTheme::default()` decides for itself whether colour is
        // appropriate, falling back to an uncoloured ASCII theme when output is
        // redirected or `NO_COLOR` is set. Only layer our palette on top when it
        // chose a colourful theme -- otherwise `anyon build 2> log` would write
        // escape codes into the file.
        let mut my_theme = GraphicalTheme::default();

        if use_color() {
            my_theme.styles.linum = Style::new().bright_blue();
            my_theme.styles.error = Style::new().red();
            my_theme.styles.warning = Style::new().yellow();
            my_theme.styles.advice = Style::new().yellow();
            my_theme.styles.help = Style::new().white();
        }

        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));

    let cli = Cli::parse();
    let Some(commands) = &cli.command else { return };

    // errors go to `stderr` so that they stay separate from the output of the
    // program being run, and a non-zero exit code lets CI and the test harness
    // tell a failed build from a successful one
    if let Err(err) = execute_cmd(commands) {
        eprintln!("{:?}", err);
        std::process::exit(1);
    }
}
