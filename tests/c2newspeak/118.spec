Newspeak output
---------------
118.c
main() {
  (118.c:27#6)^int32 x;
  (118.c:28#6)^int32 y;
  (118.c:29#2)^choose {
    | ! (1-_int32 ==_int32 0) -->
    | (1-_int32 ==_int32 0) -->
      (118.c:29#2)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
  }
}


