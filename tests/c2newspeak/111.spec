Newspeak output
---------------
111.c
main() {
  (111.c:27#6)^int32 x;
  (111.c:27#9)^int32 y;
  (111.c:28#2)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (111.c:28#2)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
    | (1-_int32 ==_int32 0) -->
  }
}


