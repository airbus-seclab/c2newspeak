Warning: Probable invalid cast from integer to pointer in 140.c line 32
Newspeak output
---------------
main() {
  (30)^int4;
  (31)^ptr;
  (32)^choose {
    | ((ptr) 1-_int4 ==_ptr 0-_ptr) -->
    | ! ((ptr) 1-_int4 ==_ptr 0-_ptr) -->
  }
}


