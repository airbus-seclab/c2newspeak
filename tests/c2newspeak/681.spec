Newspeak output
---------------
681.c
void main(void) {
  (681.c:27#6)^int32 x;
  (681.c:28#2)^choose {
   -->
    (681.c:28#2)^guard(! (0-_int32 ==_int32 0));
    (681.c:28#15)^0- =(int32) 0;
   -->
    (681.c:28#2)^guard((0-_int32 ==_int32 0));
  }
}


