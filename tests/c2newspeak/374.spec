Newspeak output
---------------
374.c
main() {
  (374.c:29#1)^int32 tmp1;
  (374.c:29#1)^do {
    (374.c:29#1)^choose {
      | ! (y_int32 ==_int32 0) -->
        (374.c:29#1)^choose {
          | ! (z_int32 ==_int32 0) -->
            (374.c:29#1)^0- =(int32) 1;
          | (z_int32 ==_int32 0) -->
            (374.c:29#1)^goto lbl1;
        }
      | (y_int32 ==_int32 0) -->
        (374.c:29#1)^goto lbl1;
    }
  } with lbl1: {
    (374.c:29#1)^int32 tmp2;
    (374.c:29#1)^choose {
      | ! (t_int32 ==_int32 0) -->
        (374.c:29#1)^0- =(int32) u_int32;
      | (t_int32 ==_int32 0) -->
        (374.c:29#1)^0- =(int32) 0;
    }
    (374.c:29#1)^1- =(int32) 0-_int32;
  }
  (374.c:29#1)^x =(int32) 0-_int32;
}

int32 t = 0;
int32 u = 0;
int32 x = 0;
int32 y = 0;
int32 z = 0;

