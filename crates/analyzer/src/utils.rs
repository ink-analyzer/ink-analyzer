//! Generic utilities.

/// Converts a name to Pascal case (i.e. UpperCamelCase).
///
/// Ref: <https://github.com/serde-rs/serde/blob/dad15b9fd0bef97b7a7c90a8a165b6ffbc682cae/serde_derive/src/internals/case.rs#L86-L100>.
pub fn pascal_case(name: &str) -> String {
    let mut pascal_name = String::new();
    let mut capitalize = true;
    for ch in name.chars() {
        if ch == '_' {
            capitalize = true;
        } else if capitalize {
            pascal_name.push(ch.to_ascii_uppercase());
            capitalize = false;
        } else {
            pascal_name.push(ch);
        }
    }
    pascal_name
}
