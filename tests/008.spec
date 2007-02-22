Newspeak output
---------------
main() {
  int4 x;
  choose {
    | (10 > 0-_int4) -->
      0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    | (0-_int4 >= 10) -->
  }
}


