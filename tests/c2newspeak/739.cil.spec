Newspeak output
---------------
739.c
void main(void) {
  int32 tmp;
  int32 tmp___0;
  (739.c:29#1063)^choose {
   -->
    (739.c:29#1063)^guard(! (~ x_int32 ==_int32 0));
    (739.c:29#1063)^choose {
     -->
      (739.c:29#1063)^guard(! (y_int32 ==_int32 0));
      (739.c:29#1063)^tmp =(int32) 1;
     -->
      (739.c:29#1063)^guard((y_int32 ==_int32 0));
      (739.c:29#1063)^tmp =(int32) 0;
    }
   -->
    (739.c:29#1063)^guard((~ x_int32 ==_int32 0));
    (739.c:29#1063)^tmp =(int32) 0;
  }
  (739.c:29#1063)^z =(int32) tmp_int32;
  (739.c:31#1119)^choose {
   -->
    (739.c:31#1119)^guard(! (~ x_int32 ==_int32 0));
    (739.c:31#1119)^choose {
     -->
      (739.c:31#1119)^guard(! (y_int32 ==_int32 0));
      (739.c:31#1119)^tmp___0 =(int32) 1;
     -->
      (739.c:31#1119)^guard((y_int32 ==_int32 0));
      (739.c:31#1119)^tmp___0 =(int32) 0;
    }
   -->
    (739.c:31#1119)^guard((~ x_int32 ==_int32 0));
    (739.c:31#1119)^tmp___0 =(int32) 0;
  }
  (739.c:31#1119)^z =(int32) tmp___0_int32;
}

int32 x;
int32 y;
int32 z;

