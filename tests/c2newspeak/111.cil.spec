Newspeak output
---------------
111.c
void main(void) {
  (111.c:27#1072)^int32 x;
  (111.c:27#1075)^int32 y;
  (111.c:28#1080)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (111.c:28#1080)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
    | (1-_int32 ==_int32 0) -->
  }
}


