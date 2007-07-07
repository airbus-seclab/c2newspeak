Newspeak output
---------------
main() {
  int4 i;
  0- =(int4) 0;
  0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
  while (1) {
    choose {
      | (100 > 0-_int4) -->
      | (0-_int4 >= 100) -->
        goto lbl2;
    }
    0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
  }
  lbl2:
}


