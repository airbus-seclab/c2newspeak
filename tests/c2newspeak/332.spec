Warning: 332.c:29#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (332.c:27#6)^uint32 goto!end;
  (332.c:27#6)^goto!end =(uint32) 0;
  (332.c:27#6)^{
    int32 x;
    (332.c:28#2)^choose {
     -->
      (332.c:28#2)^guard(! (x_int32 ==_int32 0));
      (332.c:29#4)^goto!end =(uint32) 1;
     -->
      (332.c:28#2)^guard((x_int32 ==_int32 0));
    }
    (332.c:29#4)^choose {
     -->
      (332.c:29#4)^guard(! goto!end_int32);
      (332.c:31#2)^x =(int32) 3;
     -->
      (332.c:29#4)^guard(goto!end_int32);
    }
    (332.c:33#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  }
}


