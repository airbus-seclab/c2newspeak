Newspeak output
---------------
038.c
void main(void) {
  (038.c:30#1132)^int32 a;
  (038.c:30#1135)^int32 b;
  (038.c:31#1140)^choose {
   -->
    (038.c:31#1140)^guard(! (1-_int32 ==_int32 0));
    (038.c:32#1153)^1- =(int32) 1;
   -->
    (038.c:31#1140)^guard((1-_int32 ==_int32 0));
    (038.c:33#1169)^choose {
     -->
      (038.c:33#1169)^guard(! (0-_int32 ==_int32 0));
      (038.c:34#1182)^0- =(int32) 2;
     -->
      (038.c:33#1169)^guard((0-_int32 ==_int32 0));
    }
  }
}


