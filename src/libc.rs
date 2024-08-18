use std::cell::Cell;

use crate::{Char, Ptr};

pub fn isspace(c: std::ffi::c_int) -> std::ffi::c_int {
    char::from_u32(c as u32).unwrap().is_whitespace() as std::ffi::c_int
}

pub fn isdigit(c: std::ffi::c_int) -> std::ffi::c_int {
    char::from_u32(c as u32).unwrap().is_ascii_digit() as std::ffi::c_int
}

pub fn ispunct(c: std::ffi::c_int) -> std::ffi::c_int {
    char::from_u32(c as u32).unwrap().is_ascii_punctuation() as std::ffi::c_int
}

pub fn isascii(c: std::ffi::c_int) -> std::ffi::c_int {
    char::from_u32(c as u32).unwrap().is_ascii() as std::ffi::c_int
}

pub fn exit(status: std::ffi::c_int) {
    std::process::exit(status as i32);
}

pub fn strncmp(mut str1: Ptr<Char>, mut str2: Ptr<Char>, mut n: usize) -> i32 {
    while (n > 0 && str1.get() != 0 && str2.get() != 0) {
        if (str1.get() != str2.get()) {
            return (str1.get() - str2.get()).into();
        }
        str1.inc();
        str2.inc();
        n -= 1;
    }
    
    // If we reach this point, either both strings are at their ends or 
    // the number of characters compared is less than `n`.
    if (n == 0) {
        return 0; // They matched up to `n` characters.
    } else {
        return (str1.get() - str2.get()).into(); // One string ended before the other.
    }
}

pub fn strlen(mut str: Ptr<Char>) -> usize {
    let mut count = 0;
    while str.get() != 0 { // Loop until null terminator is reached
      count += 1;
      str.inc();
    }
    return count;
}