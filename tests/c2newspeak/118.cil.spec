Newspeak output
---------------
118.c
void main(void) {
  (118.c:27#1072)^int32 x;
  (118.c:28#1081)^int32 y;
  (118.c:29#1086)^choose {
    | ! (1-_int32 ==_int32 0) -->
    | (1-_int32 ==_int32 0) -->
      (118.c:29#1086)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
  }
}


