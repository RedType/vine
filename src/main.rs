use simplelog as sl;
use structopt::StructOpt;

mod rope;
mod util;

#[derive(StructOpt)]
struct Opt {
    /// May be off, error, warn, info, debug, trace
    #[structopt(
        short,
        long,
        default_value = "info",
        parse(from_str = parse_log_level),
    )]
    log_level: sl::LevelFilter,
}

fn parse_log_level(level: &str) -> sl::LevelFilter {
    match level {
        "off" | "Off" | "OFF" => sl::LevelFilter::Off,
        "error" | "Error" | "ERROR" => sl::LevelFilter::Error,
        "warn" | "Warn" | "WARN" => sl::LevelFilter::Warn,
        "info" | "Info" | "INFO" => sl::LevelFilter::Info,
        "debug" | "Debug" | "DEBUG" => sl::LevelFilter::Debug,
        "trace" | "Trace" | "TRACE" => sl::LevelFilter::Trace,
        _ => panic!("Unexpected log level \"{}\"", level),
    }
}

fn main() {
    let opt = Opt::from_args();

    sl::TermLogger::init(
        opt.log_level,
        Default::default(),
        sl::TerminalMode::Mixed,
    ).or_else(|_| sl::SimpleLogger::init(
        opt.log_level,
        Default::default(),
    )).expect("Could not initialize logging");
}
