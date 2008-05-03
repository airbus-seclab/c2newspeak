Warning: conditional expression are ugly: use if else instead in 272.c line 29
Newspeak output
---------------
272.c
main() {
  (272.c:27#1072)^int32 x;
  (272.c:28#1081)^int32 y;
  (272.c:29#1086)^int32 tmp0;
  (272.c:29#1086)^choose {
    | ! (2-_int32 ==_int32 0) -->
      (272.c:29#1086)^0- =(int32) 0;
    | (2-_int32 ==_int32 0) -->
      (272.c:29#1086)^0- =(int32) 1;
  }
  (272.c:29#1086)^1- =(int32) 0-_int32;
}


