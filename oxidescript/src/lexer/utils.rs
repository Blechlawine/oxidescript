use std::string::FromUtf8Error;

pub fn convert_vec_utf8(input: Vec<u8>) -> Result<String, FromUtf8Error> {
    String::from_utf8(input)
}

pub fn concat_slice_vec(slice: &[u8], vec: Vec<u8>) -> Vec<u8> {
    let mut new_vec = slice.to_vec();
    new_vec.extend(vec);
    new_vec
}
