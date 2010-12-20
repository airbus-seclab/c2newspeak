Warning: 581.c:29#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (581.c:27#6)^uint32 goto!end;
  (581.c:27#6)^goto!end =(uint32) 0;
  (581.c:27#6)^{
    int32 x;
    (581.c:28#2)^choose {
     -->
      (581.c:28#2)^guard(! (x_int32 ==_int32 0));
      (581.c:29#4)^goto!end =(uint32) 1;
     -->
      (581.c:28#2)^guard((x_int32 ==_int32 0));
    }
    (581.c:29#4)^choose {
     -->
      (581.c:29#4)^guard(! goto!end_int32);
      (581.c:31#2)^x =(int32) 3;
     -->
      (581.c:29#4)^guard(goto!end_int32);
    }
    (581.c:33#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  }
}


