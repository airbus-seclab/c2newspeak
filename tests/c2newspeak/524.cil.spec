Newspeak output
---------------
524.c
main() {
  (524.c:27#1072)^int32 x;
  (524.c:28#1081)^int32 y;
  int32 tmp;
  (524.c:29#1108)^1- =(int32) 1;
  (524.c:29#1108)^choose {
    | ! (2-_int32 ==_int32 0) -->
      (524.c:29#1108)^0- =(int32) 1;
    | (2-_int32 ==_int32 0) -->
      (524.c:29#1108)^0- =(int32) 0;
  }
  (524.c:29#1108)^2- =(int32) 0-_int32;
}


