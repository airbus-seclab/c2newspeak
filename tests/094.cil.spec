Newspeak output
---------------
main() {
  (27)^int4;
  (-1)^int4;
  (28)^choose {
    | ! (0 > 1-_int4) -->
      (28)^choose {
        | (10 > 1-_int4) -->
          (28)^0- =(int4) 1;
        | ! (10 > 1-_int4) -->
          (28)^0- =(int4) 0;
      }
    | (0 > 1-_int4) -->
      (28)^0- =(int4) 0;
  }
  (28)^1- =(int4) 0-_int4;
}


