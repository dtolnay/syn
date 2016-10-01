#![cfg(feature = "specialization")]

#[string = "a"]
#[raw_string = r"a"]
#[more_raw_string = r##"#"#"##]
#[byte_string = b"a"]
#[raw_byte_string = br"a"]
#[more_raw_byte_string = br##"#"#"##]
struct Success;
