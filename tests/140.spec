Warning: Probable invalid cast from integer to pointer in 140.c line 32
Newspeak output
---------------
main() {
  (140.c:30#1132)^int32;
  (140.c:31#1142)^ptr;
  (140.c:32#1149)^choose {
    | ((ptr) 1-_int32 ==_ptr 0-_ptr) -->
    | ! ((ptr) 1-_int32 ==_ptr 0-_ptr) -->
  }
}


