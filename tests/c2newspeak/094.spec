Newspeak output
---------------
094.c
main() {
  (094.c:27#6)^int32 x;
  (094.c:28#2)^choose {
    | ! (0 > 0-_int32) -->
      (094.c:28#2)^0- =(int32) (10 > 0-_int32);
    | (0 > 0-_int32) -->
      (094.c:28#2)^0- =(int32) 0;
  }
}


