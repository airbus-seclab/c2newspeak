Newspeak output
---------------
495.c
void f(void) {
  (495.c:32#2)^choose {
    | ! (a_int32 ==_int32 0) -->
      (495.c:32#2)^choose {
        | ! (b_int32 ==_int32 0) -->
          (495.c:32#2)^a =(int32) 1;
        | (b_int32 ==_int32 0) -->
          (495.c:32#2)^a =(int32) ! (c_int32 ==_int32 0);
      }
    | (a_int32 ==_int32 0) -->
      (495.c:32#2)^a =(int32) ! (c_int32 ==_int32 0);
  }
}

void g(void) {
  (495.c:39#2)^a =(int32) ! (c_int32 ==_int32 0);
}

int32 a = 0;
int32 b = 0;
int32 c = 0;

