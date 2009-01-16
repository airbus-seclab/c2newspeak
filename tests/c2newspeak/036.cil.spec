Newspeak output
---------------
036.c
void main(void) {
  (036.c:30#1132)^int32 x;
  (036.c:31#1141)^int32 y;
  (036.c:29#1112)^do {
    (036.c:33#1161)^do {
      (036.c:32#1146)^choose {
       -->
        (036.c:32#1146)^guard(! (1-_int32 ==_int32 1));
        (036.c:32#1146)^goto lbl0;
       -->
        (036.c:32#1146)^guard((1-_int32 ==_int32 1));
        (036.c:32#1146)^goto lbl1;
      }
    } with lbl1: {
    }
    (036.c:36#1206)^0- =(int32) 4;
  } with lbl0: {
  }
}


