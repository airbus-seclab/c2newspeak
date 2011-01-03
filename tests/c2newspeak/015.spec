Newspeak output
---------------
void (015.c:29#5)^main(void) {
  (015.c:30#6)^int32 i;
  (015.c:30#6)^i =(int32) 0;
  (015.c:32#2)^do {
    (015.c:32#2)^while (1) {
      (015.c:33#4)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
      (015.c:32#2)^choose {
       -->
        (015.c:32#2)^guard((100 > i_int32));
       -->
        (015.c:32#2)^guard(! (100 > i_int32));
        (015.c:32#2)^goto lbl1;
      }
    }
  } with lbl1:
}


