Newspeak output
---------------
512.c
void main(void) {
  int32 tmp;
  (512.c:29#1076)^choose {
    | ! (x_int32 ==_int32 0) -->
      (512.c:29#1076)^0- =(int32) 1;
    | (x_int32 ==_int32 0) -->
      (512.c:29#1076)^0- =(int32) 0;
  }
  (512.c:29#1076)^choose {
    | ! (0-_int32 ==_int32 0) -->
    | (0-_int32 ==_int32 0) -->
  }
}

int32 x = 0;

