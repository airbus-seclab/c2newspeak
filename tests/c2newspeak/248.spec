Newspeak output
---------------
void main(void) {
  (248.c:27#6)^int32[22] x;
  (248.c:28#6)^int32 y;
  (248.c:29#2)^choose {
   -->
    (248.c:29#2)^choose {
     -->
      (248.c:29#2)^guard(! (x_int32 ==_int32 0));
     -->
      (248.c:29#2)^guard((x_int32 ==_int32 0));
      (248.c:29#2)^choose {
       -->
        (248.c:29#2)^guard(! (x + 32_int32 ==_int32 0));
       -->
        (248.c:29#2)^guard((x + 32_int32 ==_int32 0));
        (248.c:29#2)^guard(! (x + 64_int32 ==_int32 0));
      }
    }
    (248.c:30#4)^y =(int32) 1;
   -->
    (248.c:29#2)^guard((x_int32 ==_int32 0));
    (248.c:29#2)^guard((x + 32_int32 ==_int32 0));
    (248.c:29#2)^guard((x + 64_int32 ==_int32 0));
  }
}


