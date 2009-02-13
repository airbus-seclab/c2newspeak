Warning: 589.c:30#1091: goto statement accepted
Newspeak output
---------------
589.c
void main(void) {
  (589.c:27#6)^uint32 goto.lbl;
  (589.c:27#6)^0- =(uint32) 0;
  (589.c:27#6)^{
    int32 i;
    (589.c:29#2)^0- =(int32) 1;
    (589.c:29#2)^choose {
     -->
      (589.c:29#2)^guard(! (0-_int32 ==_int32 0));
      (589.c:30#4)^1- =(uint32) 1;
      (589.c:30#4)^choose {
       -->
        (589.c:30#4)^guard(coerce[0,4294967295] (0 - 1-_uint32));
        (589.c:31#4)^0- =(int32) 0;
       -->
        (589.c:30#4)^guard(coerce[0,4294967295] ! (0 - 1-_uint32));
      }
     -->
      (589.c:29#2)^guard((0-_int32 ==_int32 0));
    }
    (589.c:28#1)^do {
      (589.c:28#1)^while (1) {
        (589.c:28#1)^choose {
         -->
          (589.c:28#1)^guard(1-_uint32);
         -->
          (589.c:28#1)^guard(! 1-_uint32);
          (589.c:28#1)^goto lbl1;
        }
        (589.c:29#2)^0- =(int32) 1;
        (589.c:29#2)^choose {
         -->
          (589.c:29#2)^guard(! (0-_int32 ==_int32 0));
          (589.c:30#4)^1- =(uint32) 1;
          (589.c:30#4)^choose {
           -->
            (589.c:30#4)^guard(coerce[0,4294967295] (0 - 1-_uint32));
            (589.c:31#4)^0- =(int32) 0;
           -->
            (589.c:30#4)^guard(coerce[0,4294967295] ! (0 - 1-_uint32));
          }
         -->
          (589.c:29#2)^guard((0-_int32 ==_int32 0));
        }
      }
    } with lbl1: {
    }
  }
}


