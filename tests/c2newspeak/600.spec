Warning: 600.c:29#1089: goto statement accepted
Newspeak output
---------------
600.c
void main(void) {
  (600.c:27#6)^uint32 goto.lbl;
  (600.c:27#6)^0- =(uint32) 0;
  (600.c:27#6)^{
    int32 x;
    (600.c:28#2)^do {
      (600.c:28#2)^while (1) {
        (600.c:28#2)^choose {
         -->
          (600.c:28#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (600.c:28#2)^guard((0-_int32 ==_int32 0));
          (600.c:28#2)^goto lbl1;
        }
        (600.c:31#2)^choose {
         -->
          (600.c:31#2)^guard(! 1);
          (600.c:30#4)^0- =(int32) 0;
         -->
        }
      }
    } with lbl1: {
    }
  }
}


