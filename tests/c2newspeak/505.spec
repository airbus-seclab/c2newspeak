Newspeak output
---------------
505.c
void main(void) {
  (505.c:29#2)^do {
    (505.c:29#2)^choose {
      | ! (a_int32 ==_int32 0) -->
        (505.c:29#2)^choose {
          | ! (b_int32 ==_int32 0) -->
            (505.c:30#4)^x =(int32) 0;
            (505.c:31#4)^x =(int32) 3;
          | (b_int32 ==_int32 0) -->
            (505.c:29#2)^goto lbl1;
        }
      | (a_int32 ==_int32 0) -->
        (505.c:29#2)^goto lbl1;
    }
  } with lbl1: {
    (505.c:29#2)^choose {
      | ! (c_int32 ==_int32 0) -->
        (505.c:29#2)^choose {
          | ! (d_int32 ==_int32 0) -->
            (505.c:30#4)^x =(int32) 0;
            (505.c:31#4)^x =(int32) 3;
          | (d_int32 ==_int32 0) -->
            (505.c:33#4)^x =(int32) 1;
            (505.c:34#4)^x =(int32) 2;
        }
      | (c_int32 ==_int32 0) -->
        (505.c:33#4)^x =(int32) 1;
        (505.c:34#4)^x =(int32) 2;
    }
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 x = 0;

