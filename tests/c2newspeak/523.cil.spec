Newspeak output
---------------
523.c
main() {
  (523.c:27#1072)^int32 x;
  int32 tmp;
  (523.c:28#1099)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (523.c:28#1099)^0- =(int32) 1;
    | (1-_int32 ==_int32 0) -->
      (523.c:28#1099)^0- =(int32) 0;
  }
  (523.c:28#1099)^1- =(int32) 0-_int32;
}

