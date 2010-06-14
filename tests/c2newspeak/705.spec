Newspeak output
---------------
705.c
void main(void) {
  (705.c:29#6)^int32 x;
  (705.c:31#2)^int32 !tmp0;
  (705.c:31#2)^!tmp0 <- f(x_int32);
  (705.c:31#2)^choose {
   -->
    (705.c:31#2)^guard(! (!tmp0_int32 ==_int32 0));
    (705.c:32#4)^x =(int32) 1;
   -->
    (705.c:31#2)^guard((!tmp0_int32 ==_int32 0));
  }
}


