Warning: 158.c:29: dirty cast from pointer to integer accepted
Newspeak output
---------------
158.c
main() {
  (158.c:27#6)^int32 x;
  (158.c:28#7)^int8[10] t;
  (158.c:29#2)^choose {
    | (1-_int32 > (int32) &_80(0-)) -->
    | ! (1-_int32 > (int32) &_80(0-)) -->
  }
}


