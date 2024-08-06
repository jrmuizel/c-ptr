## Memory Safety

- When converting from usize we check type validity. This is conservative
  you could imagine a program that makes a pointer of the wrong type to some memory but doesn't deref it.
- On deref we check that memory is live
