Newspeak output
---------------
022.c
main() {
  (022.c:30#1132)^int32;
  (022.c:30#1135)^int32;
  (022.c:30#1138)^int32;
  (022.c:31#1143)^choose {
    | ! (2-_int32 ==_int32 0) -->
      (022.c:31#1143)^choose {
        | ! (1-_int32 ==_int32 0) -->
          (022.c:32#1161)^2- =(int32) 1;
        | (1-_int32 ==_int32 0) -->
          (022.c:33#1177)^choose {
            | ! (0-_int32 ==_int32 0) -->
              (022.c:34#1190)^1- =(int32) 2;
            | (0-_int32 ==_int32 0) -->
          }
      }
    | (2-_int32 ==_int32 0) -->
      (022.c:33#1177)^choose {
        | ! (0-_int32 ==_int32 0) -->
          (022.c:34#1190)^1- =(int32) 2;
        | (0-_int32 ==_int32 0) -->
      }
  }
}


