Newspeak output
---------------
373.c
main() {
  (373.c:28#1096)^int32 x;
  (373.c:28#1099)^int32 y;
  (373.c:29#1104)^int32 !tmp-1073741821;
  (373.c:29#1104)^{
    int32 value_of_f;
    (373.c:29#1104)^f();
    (373.c:29#1104)^1- =(int32) 0-_int32;
  }
  (373.c:29#1104)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (373.c:29#1104)^choose {
        | ! (2-_int32 ==_int32 0) -->
          (373.c:30#1131)^2- =(int32) 0;
          (373.c:31#1142)^2- =(int32) 0;
        | (2-_int32 ==_int32 0) -->
          (373.c:29#1104)^choose {
            | ! (1-_int32 ==_int32 0) -->
              (373.c:30#1131)^2- =(int32) 0;
              (373.c:31#1142)^2- =(int32) 0;
            | (1-_int32 ==_int32 0) -->
              (373.c:33#1164)^2- =(int32) 1;
              (373.c:34#1175)^2- =(int32) 1;
          }
      }
    | (0-_int32 ==_int32 0) -->
      (373.c:33#1164)^2- =(int32) 1;
      (373.c:34#1175)^2- =(int32) 1;
  }
}


