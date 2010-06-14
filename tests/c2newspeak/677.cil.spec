Newspeak output
---------------
677.c
void main(void) {
  int32 tmp;
  (677.c:31#1101)^tmp <- f();
  (677.c:31#1101)^choose {
   -->
    (677.c:31#1101)^guard(! (tmp_int32 ==_int32 0));
    (677.c:31#1101)^choose {
     -->
      (677.c:31#1101)^guard(! (x_int32 ==_int32 0));
      (677.c:32#1121)^x =(int32) 1;
     -->
      (677.c:31#1101)^guard((x_int32 ==_int32 0));
    }
   -->
    (677.c:31#1101)^guard((tmp_int32 ==_int32 0));
  }
}

int32 x;

