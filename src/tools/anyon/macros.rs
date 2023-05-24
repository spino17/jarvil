macro_rules! impl_variants {
    ($t: ident, $u: ident) => {
        match $t {
            AnyonCommand::NEW(new_driver) => new_driver.$u(),
            AnyonCommand::BUILD(build_driver) => build_driver.$u(),
            AnyonCommand::FMT(fmt_driver) => fmt_driver.$u(),
            AnyonCommand::VERSION(version_driver) => version_driver.$u(),
            AnyonCommand::HELP(help_driver) => help_driver.$u(),
        }
    };
}
