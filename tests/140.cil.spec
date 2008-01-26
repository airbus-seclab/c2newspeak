Warning: Probable invalid cast from pointer to integer in 140.c line 32
Newspeak output
---------------
140.c
main() {
  (140.c:30#1132)^int32;
  (140.c:31#1141)^ptr;
  (140.c:32#1149)^choose {
    | (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
    | ! (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
  }
}


