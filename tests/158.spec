Warning: Probable invalid cast from pointer to integer in 158.c line 29
Newspeak output
---------------
158.c
main() {
  (158.c:27#1072)^int32;
  (158.c:28#1082)^int8[10];
  (158.c:29#1091)^choose {
    | (1-_int32 > (int32) &_80(0-)) -->
    | ! (1-_int32 > (int32) &_80(0-)) -->
  }
}


