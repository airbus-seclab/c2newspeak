Warning: 609.c:30#1089: goto statement accepted
Newspeak output
---------------
609.c
void main(void) {
  (609.c:27#6)^uint32 goto.lbl;
  (609.c:27#6)^0- =(uint32) 0;
  (609.c:27#6)^{
    int32 i;
    (609.c:30#6)^1- =(uint32) 1;
    (609.c:30#6)^choose {
     -->
      (609.c:30#6)^guard(! 1-_int32);
      (609.c:31#6)^0- =(int32) 2;
     -->
      (609.c:30#6)^guard(1-_int32);
    }
    (609.c:30#6)^choose {
     -->
      (609.c:30#6)^guard(! 1-_int32);
      (609.c:33#4)^0- =(int32) 1;
     -->
      (609.c:30#6)^guard(1-_int32);
    }
    (609.c:35#2)^while (1) {
      (609.c:35#2)^choose {
       -->
        (609.c:35#2)^guard(! (1-_uint32 ==_uint32 0));
       -->
        (609.c:35#2)^guard((1-_uint32 ==_uint32 0));
      }
      (609.c:36#2)^1- =(uint32) 0;
    }
  }
}


