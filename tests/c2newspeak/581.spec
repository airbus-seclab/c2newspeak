Warning: 581.c:29#1086: goto statement accepted
Newspeak output
---------------
581.c
void main(void) {
  (581.c:27#6)^uint32 goto.end;
  (581.c:27#6)^0- =(uint32) 0;
  (581.c:27#6)^{
    int32 x;
    (581.c:28#2)^choose {
     -->
      (581.c:28#2)^guard(! (0-_int32 ==_int32 0));
      (581.c:29#4)^1- =(uint32) 1;
     -->
      (581.c:28#2)^guard((0-_int32 ==_int32 0));
    }
    (581.c:29#4)^choose {
     -->
      (581.c:29#4)^guard(! 1-_int32);
      (581.c:31#2)^0- =(int32) 3;
     -->
      (581.c:29#4)^guard(1-_int32);
    }
    (581.c:33#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  }
}


