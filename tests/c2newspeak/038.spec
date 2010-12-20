Newspeak output
---------------
void main(void) {
  (038.c:30#6)^int32 a;
  (038.c:30#9)^int32 b;
  (038.c:31#2)^choose {
   -->
    (038.c:31#2)^guard(! (a_int32 ==_int32 0));
    (038.c:32#4)^a =(int32) 1;
   -->
    (038.c:31#2)^guard((a_int32 ==_int32 0));
    (038.c:33#9)^choose {
     -->
      (038.c:33#9)^guard(! (b_int32 ==_int32 0));
      (038.c:34#4)^b =(int32) 2;
     -->
      (038.c:33#9)^guard((b_int32 ==_int32 0));
    }
  }
}


