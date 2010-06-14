Newspeak output
---------------
738.c
void main(void) {
  int32 tmp;
  (738.c:29#1063)^choose {
   -->
    (738.c:29#1063)^guard((x_int32 ==_int32 0));
    (738.c:29#1063)^choose {
     -->
      (738.c:29#1063)^guard(! (y_int32 ==_int32 0));
      (738.c:29#1063)^tmp =(int32) 1;
     -->
      (738.c:29#1063)^guard((y_int32 ==_int32 0));
      (738.c:29#1063)^tmp =(int32) 0;
    }
   -->
    (738.c:29#1063)^guard(! (x_int32 ==_int32 0));
    (738.c:29#1063)^tmp =(int32) 0;
  }
  (738.c:29#1063)^z =(int32) tmp_int32;
}

int32 x;
int32 y;
int32 z;

