macro_rules! impl_variants {
    ($t: ident, $u: ident) => {
        match $t {
            AnyonCommand::New(new_driver) => new_driver.$u(),
            AnyonCommand::Build(build_driver) => build_driver.$u(),
            AnyonCommand::Fmt(fmt_driver) => fmt_driver.$u(),
            AnyonCommand::Version(version_driver) => version_driver.$u(),
            AnyonCommand::Help(help_driver) => help_driver.$u(),
        }
    };
}
