006.c
int32 getIndex(ptr) {
  (006.c:30#6)^int32 x;
  (006.c:29#6)^int32 found;
  (006.c:32#2)^0- =(int32) 0;
  (006.c:33#2)^1- =(int32) 0;
  (006.c:35#4)^choose {
    | ! (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
      (006.c:36#6)^[2-_ptr]32 =(int32) 0;
      (006.c:37#6)^0- =(int32) 1;
    | (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
  }
  (006.c:34#2)^do {
    (006.c:34#2)^while (1) {
      (006.c:34#2)^choose {
        | (10 > 1-_int32) -->
          (006.c:34#2)^choose {
            | (0 ==_int32 0-_int32) -->
            | ! (0 ==_int32 0-_int32) -->
              (006.c:34#2)^goto lbl1;
          }
        | ! (10 > 1-_int32) -->
          (006.c:34#2)^goto lbl1;
      }
      (006.c:35#4)^choose {
        | ! (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
          (006.c:36#6)^[2-_ptr]32 =(int32) 1-_int32;
          (006.c:37#6)^0- =(int32) 1;
        | (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
      }
    }
  } with lbl1: {
  }
  (006.c:28#4)^do {
    (006.c:41#2)^choose {
      | ! (0 ==_int32 0-_int32) -->
        (006.c:42#4)^3- =(int32) 1;
        (006.c:42#4)^goto lbl0;
      | (0 ==_int32 0-_int32) -->
        (006.c:44#4)^3- =(int32) 0;
        (006.c:44#4)^goto lbl0;
    }
  } with lbl0: {
  }
}

void main(void) {
  (006.c:49#6)^int32 i;
  (006.c:50#6)^int32 c;
  (006.c:52#2)^{
    ptr getIndex.arg1;
    (006.c:52#2)^0- =(ptr) &_32(2-);
    (006.c:30#6)^{
      int32 x;
      (006.c:29#6)^int32 found;
      (006.c:32#2)^0- =(int32) 0;
      (006.c:33#2)^1- =(int32) 0;
      (006.c:35#4)^choose {
        | ! (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
          (006.c:36#6)^[2-_ptr]32 =(int32) 0;
          (006.c:37#6)^0- =(int32) 1;
        | (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
      }
      (006.c:34#2)^do {
        (006.c:34#2)^while (1) {
          (006.c:34#2)^choose {
            | (10 > 1-_int32) -->
              (006.c:34#2)^choose {
                | (0 ==_int32 0-_int32) -->
                | ! (0 ==_int32 0-_int32) -->
                  (006.c:34#2)^goto lbl1;
              }
            | ! (10 > 1-_int32) -->
              (006.c:34#2)^goto lbl1;
          }
          (006.c:35#4)^choose {
            | ! (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
              (006.c:36#6)^[2-_ptr]32 =(int32) 1-_int32;
              (006.c:37#6)^0- =(int32) 1;
            | (0 ==_int32 t + (belongs[0,9] 1-_int32 * 8)_int8) -->
          }
        }
      } with lbl1: {
      }
      (006.c:28#4)^do {
        (006.c:41#2)^choose {
          | ! (0 ==_int32 0-_int32) -->
            (006.c:42#4)^3- =(int32) 1;
            (006.c:42#4)^goto lbl0;
          | (0 ==_int32 0-_int32) -->
            (006.c:44#4)^3- =(int32) 0;
            (006.c:44#4)^goto lbl0;
        }
      } with lbl0: {
      }
    }
  }
  (006.c:54#2)^choose {
    | ! (0 ==_int32 0-_int32) -->
      (006.c:55#4)^t + (belongs[0,9] 1-_int32 * 8) =(int8) 0;
    | (0 ==_int32 0-_int32) -->
  }
}

int8[10] t = 0;
