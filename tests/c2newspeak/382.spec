Newspeak output
---------------
void (382.c:28#5)^main(void) {
  (382.c:29#6)^int32 i;
  (382.c:31#2)^i =(int32) 0;
  (382.c:33#2)^do {
    (382.c:33#2)^do {
      (382.c:32#2)^choose {
       -->
        (382.c:32#2)^guard((0 ==_int32 0));
        (382.c:33#2)^goto lbl2;
       -->
        (382.c:32#2)^guard(! (0 ==_int32 0));
        (382.c:32#2)^goto lbl1;
      }
    } with lbl2:
    (382.c:34#4)^i =(int32) 1;
    (382.c:35#4)^i =(int32) 2;
  } with lbl1:
}


