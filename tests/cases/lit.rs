literals! {
    // Str
    ""
    "0"
    "\n"
    r"\n"

    // ByteStr
    b""
    b"0"
    b"\n"
    br"\n"

    // Byte
    b'0'
    b'\n'

    // Char
    '0'
    '\n'

    // Int
    0
    0u8
    1_0u8

    // Float
    0e0
    0e+0
    0e-0
    0.
    0.0
    0.0e0
    0.0e+0
    0.0e-0
    0f32
    0.0f32
    0e0f32
    1_2_.3_e+_4_f32

    // Bool
    true
    false
}
