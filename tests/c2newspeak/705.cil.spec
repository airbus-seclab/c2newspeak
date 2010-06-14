Newspeak output
---------------
705.c
void main(void) {
  (705.c:29#1073)^int32 x;
  int32 tmp;
  (705.c:31#1079)^tmp <- f(x_int32);
  (705.c:31#1079)^choose {
   -->
    (705.c:31#1079)^guard(! (tmp_int32 ==_int32 0));
    (705.c:32#1159)^x =(int32) 1;
   -->
    (705.c:31#1079)^guard((tmp_int32 ==_int32 0));
  }
}


