Newspeak output
---------------
520.c
void main(void) {
  (520.c:27#1072)^int32 x;
  (520.c:28#1081)^int32 y;
  (520.c:29#1090)^int32 z;
  int32 tmp;
  (520.c:31#1096)^choose {
   -->
    (520.c:31#1096)^guard(! (3-_int32 ==_int32 0));
    (520.c:31#1096)^choose {
     -->
      (520.c:31#1096)^guard(! (2-_int32 ==_int32 0));
      (520.c:31#1096)^0- =(int32) 1;
     -->
      (520.c:31#1096)^guard((2-_int32 ==_int32 0));
      (520.c:31#1096)^0- =(int32) 0;
    }
   -->
    (520.c:31#1096)^guard((3-_int32 ==_int32 0));
    (520.c:31#1096)^0- =(int32) 0;
  }
  (520.c:31#1096)^1- =(int32) 0-_int32;
}


