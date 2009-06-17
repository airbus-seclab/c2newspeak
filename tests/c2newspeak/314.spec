Warning: 314.c:29#2: assignment within expression accepted
Newspeak output
---------------
314.c
void main(void) {
  (314.c:27#6)^int32 x;
  (314.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (314.c:29#2)^choose {
   -->
    (314.c:29#2)^guard(! (0-_int32 ==_int32 0));
   -->
    (314.c:29#2)^guard((0-_int32 ==_int32 0));
  }
}


