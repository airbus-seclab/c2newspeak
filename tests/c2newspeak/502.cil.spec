Newspeak output
---------------
502.c
main() {
  (502.c:33#1129)^choose {
    | ! (a_int32 ==_int32 0) -->
      (502.c:33#1129)^choose {
        | ! (b_int32 ==_int32 0) -->
          (502.c:34#1145)^x =(int32) 0;
          (502.c:35#1156)^x =(int32) 1;
          (502.c:36#1167)^x =(int32) 2;
          (502.c:37#1178)^x =(int32) 3;
          (502.c:38#1189)^x =(int32) 4;
        | (b_int32 ==_int32 0) -->
          (502.c:40#1211)^x =(int32) 0;
          (502.c:41#1222)^x =(int32) 1;
          (502.c:42#1233)^x =(int32) 2;
          (502.c:43#1244)^x =(int32) 3;
          (502.c:44#1255)^x =(int32) 4;
          (502.c:45#1266)^x =(int32) 5;
      }
    | (a_int32 ==_int32 0) -->
      (502.c:40#1211)^x =(int32) 0;
      (502.c:41#1222)^x =(int32) 1;
      (502.c:42#1233)^x =(int32) 2;
      (502.c:43#1244)^x =(int32) 3;
      (502.c:44#1255)^x =(int32) 4;
      (502.c:45#1266)^x =(int32) 5;
  }
}

int32 a = 0;
int32 b = 0;
int32 x = 0;

