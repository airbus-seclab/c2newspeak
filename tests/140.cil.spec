Warning: Probable invalid cast from pointer to integer in 140.c line 32
Newspeak output
---------------
main() {
  (30)^int32;
  (31)^ptr;
  (32)^choose {
    | (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
    | ! (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
  }
}


