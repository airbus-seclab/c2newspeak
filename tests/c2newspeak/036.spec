Newspeak output
---------------
void main(void) {
  (036.c:30#6)^int32 x;
  (036.c:31#6)^int32 y;
  (036.c:32#2)^do {
    (036.c:33#2)^do {
      (036.c:32#2)^choose {
       -->
        (036.c:32#2)^guard((x_int32 ==_int32 1));
        (036.c:33#2)^goto lbl2;
       -->
        (036.c:32#2)^guard(! (x_int32 ==_int32 1));
        (036.c:32#2)^goto lbl1;
      }
    } with lbl2:
    (036.c:36#6)^y =(int32) 4;
  } with lbl1:
}


