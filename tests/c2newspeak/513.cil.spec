Newspeak output
---------------
513.c
void main(void) {
  int32 tmp;
  (513.c:29#1076)^choose {
    | ! (x_int32 ==_int32 0) -->
      (513.c:29#1076)^0- =(int32) 1;
    | (x_int32 ==_int32 0) -->
      (513.c:29#1076)^0- =(int32) 0;
  }
  (513.c:29#1076)^choose {
    | ! (0-_int32 ==_int32 0) -->
    | (0-_int32 ==_int32 0) -->
  }
}

int32 x = 0;

