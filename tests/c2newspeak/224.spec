Newspeak output
---------------
void main(void) {
  (224.c:27#6)^int32 x;
  (224.c:29#2)^choose {
   -->
    (224.c:29#2)^guard(! (x_int32 ==_int32 0));
    (224.c:29#2)^x =(int32) ! (x_int32 ==_int32 0);
   -->
    (224.c:29#2)^guard((x_int32 ==_int32 0));
    (224.c:29#2)^x =(int32) 0;
  }
}


