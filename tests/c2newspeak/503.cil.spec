Newspeak output
---------------
503.c
main() {
  int32 tmp___0;
  int32 tmp___1;
  (503.c:29#1094)^choose {
    | ! (a_int32 ==_int32 0) -->
      (503.c:29#1094)^0- =(int32) b_int32;
    | (a_int32 ==_int32 0) -->
      (503.c:29#1094)^0- =(int32) c_int32;
  }
  (503.c:29#1094)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (503.c:29#1094)^1- =(int32) d_int32;
    | (0-_int32 ==_int32 0) -->
      (503.c:29#1094)^1- =(int32) e_int32;
  }
  (503.c:29#1094)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (503.c:30#1117)^x =(int32) 0;
      (503.c:31#1128)^x =(int32) 1;
      (503.c:32#1139)^x =(int32) 2;
      (503.c:33#1150)^x =(int32) 3;
      (503.c:34#1161)^x =(int32) 4;
    | (1-_int32 ==_int32 0) -->
      (503.c:36#1183)^x =(int32) 0;
      (503.c:37#1194)^x =(int32) 1;
      (503.c:38#1205)^x =(int32) 2;
      (503.c:39#1216)^x =(int32) 3;
      (503.c:40#1227)^x =(int32) 4;
      (503.c:41#1238)^x =(int32) 5;
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 e = 0;
int32 x = 0;
