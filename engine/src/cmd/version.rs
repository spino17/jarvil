const TOML: &'static str = include_str!("../../Cargo.toml");
pub fn jarvil_version() -> String {
    let mut base_str = String::from("jarvil ");
    if let Some(line) = TOML.split("\n").filter(|x| x.starts_with("version")).next() {
        let start = line.find('"').unwrap() + 1;
        let end = line.rfind('"').unwrap();
        base_str.push_str(&line[start..end]);
        base_str
    } else {
        panic!("parsing toml file failed!\n...raise an issue on jarvil tracker if you see this message");
    }
}