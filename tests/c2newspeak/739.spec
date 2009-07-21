Newspeak output
---------------
739.c
void main(void) {
  (739.c:29#2)^choose {
   -->
    (739.c:29#2)^guard(~ x_int32);
    (739.c:29#2)^z =(int32) ! (y_int32 ==_int32 0);
   -->
    (739.c:29#2)^guard(! ~ x_int32);
    (739.c:29#2)^z =(int32) 0;
  }
  (739.c:31#0)^choose {
   -->
    (739.c:31#0)^guard(~ x_int32);
    (739.c:31#0)^z =(int32) ! (y_int32 ==_int32 0);
   -->
    (739.c:31#0)^guard(! ~ x_int32);
    (739.c:31#0)^z =(int32) 0;
  }
}

int32 x;
int32 y;
int32 z;

