Newspeak output
---------------
495.c
f() {
  (495.c:32#2)^int32 tmp1;
  (495.c:32#2)^choose {
    | ! (a_int32 ==_int32 0) -->
      (495.c:32#2)^choose {
        | ! (b_int32 ==_int32 0) -->
          (495.c:32#2)^0- =(int32) 1;
        | (b_int32 ==_int32 0) -->
          (495.c:32#2)^0- =(int32) c_int32;
      }
    | (a_int32 ==_int32 0) -->
      (495.c:32#2)^0- =(int32) c_int32;
  }
  (495.c:32#2)^a =(int32) 0-_int32;
}

g() {
  (495.c:39#2)^a =(int32) c_int32;
}

int32 a = 0;
int32 b = 0;
int32 c = 0;

