Newspeak output
---------------
194.c
void main(void) {
  (194.c:27#6)^int32 x;
  (194.c:29#2)^choose {
   -->
    (194.c:29#2)^guard(! (0-_int32 ==_int32 0));
    (194.c:29#2)^guard(! (0-_int32 ==_int32 0));
    (194.c:29#2)^0- =(int32) ! (0-_int32 ==_int32 0);
   -->
    (194.c:29#2)^choose {
     -->
      (194.c:29#2)^guard(! (0-_int32 ==_int32 0));
      (194.c:29#2)^guard((0-_int32 ==_int32 0));
     -->
      (194.c:29#2)^guard((0-_int32 ==_int32 0));
    }
    (194.c:29#2)^0- =(int32) 0;
  }
}


