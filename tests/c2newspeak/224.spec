Newspeak output
---------------
224.c
void main(void) {
  (224.c:27#6)^int32 x;
  (224.c:29#2)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (224.c:29#2)^0- =(int32) ! (0-_int32 ==_int32 0);
    | (0-_int32 ==_int32 0) -->
      (224.c:29#2)^0- =(int32) 0;
  }
}


