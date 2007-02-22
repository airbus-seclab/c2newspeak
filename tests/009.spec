Newspeak output
---------------
main() {
  int4 x;
  choose {
    | (10 > 0-_int4) -->
      1- =(int4) 1;
      goto lbl1;
    | (0-_int4 >= 10) -->
  }
  1- =(int4) 0;
  lbl1:
}


