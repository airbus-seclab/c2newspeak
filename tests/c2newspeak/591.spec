Warning: 591.c:32#1104: goto statement accepted
Newspeak output
---------------
591.c
void main(void) {
  (591.c:27#6)^uint32 goto.lbl;
  (591.c:27#6)^0- =(uint32) 0;
  (591.c:27#6)^{
    int32 i;
    (591.c:29#2)^0- =(int32) 1;
    (591.c:29#2)^choose {
     -->
      (591.c:29#2)^guard(! (0-_int32 ==_int32 0));
     -->
      (591.c:29#2)^guard((0-_int32 ==_int32 0));
      (591.c:32#4)^1- =(uint32) 1;
      (591.c:32#4)^choose {
       -->
        (591.c:32#4)^guard(coerce[0,4294967295] (0 - 1-_uint32));
        (591.c:33#4)^0- =(int32) 0;
       -->
        (591.c:32#4)^guard(coerce[0,4294967295] ! (0 - 1-_uint32));
      }
    }
    (591.c:28#1)^do {
      (591.c:28#1)^while (1) {
        (591.c:28#1)^choose {
         -->
          (591.c:28#1)^guard(1-_uint32);
         -->
          (591.c:28#1)^guard(! 1-_uint32);
          (591.c:28#1)^goto lbl1;
        }
        (591.c:29#2)^0- =(int32) 1;
        (591.c:29#2)^choose {
         -->
          (591.c:29#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (591.c:29#2)^guard((0-_int32 ==_int32 0));
          (591.c:32#4)^1- =(uint32) 1;
          (591.c:32#4)^choose {
           -->
            (591.c:32#4)^guard(coerce[0,4294967295] (0 - 1-_uint32));
            (591.c:33#4)^0- =(int32) 0;
           -->
            (591.c:32#4)^guard(coerce[0,4294967295] ! (0 - 1-_uint32));
          }
        }
      }
    } with lbl1: {
    }
  }
}

