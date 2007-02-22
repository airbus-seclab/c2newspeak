Newspeak output
---------------
main() {
  int4 x;
  0- =(int4) 0;
  while (1) {
    choose {
      | (10 > 0-_int4) -->
      | (0-_int4 >= 10) -->
        goto lbl2;
    }
    0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
  }
  lbl2:
}


