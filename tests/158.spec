Warning: Probable invalid cast from pointer to integer in 158.c line 29
Newspeak output
---------------
main() {
  (27)^int32;
  (28)^int8[10];
  (29)^choose {
    | (1-_int32 > (int32) &_80(0-)) -->
    | ! (1-_int32 > (int32) &_80(0-)) -->
  }
}


