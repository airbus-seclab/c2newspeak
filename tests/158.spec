Warning: Probable invalid cast from pointer to integer in 158.c line 29
Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int1[10];
  (29)^choose {
    | (1-_int4 > (int4) &_10(0-)) -->
    | ! (1-_int4 > (int4) &_10(0-)) -->
  }
}


