Warning: 622.c:33#1114: goto statement accepted
Newspeak output
---------------
622.c
void main(void) {
  (622.c:28#2)^uint32 goto.fail;
  (622.c:28#2)^0- =(uint32) 0;
  (622.c:32#4)^do {
    (622.c:32#4)^while (1) {
      (622.c:29#11)^choose {
       -->
        (622.c:29#11)^guard(! (0-_uint32 ==_uint32 0));
       -->
        (622.c:29#11)^guard((0-_uint32 ==_uint32 0));
      }
      (622.c:32#4)^0- =(uint32) 1;
      (622.c:32#4)^choose {
       -->
        (622.c:32#4)^guard(0-_int32);
       -->
        (622.c:32#4)^guard(! 0-_int32);
        (622.c:32#4)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


