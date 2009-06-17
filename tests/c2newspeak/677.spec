Newspeak output
---------------
677.c
void main(void) {
  (677.c:31#2)^int32 !tmp0;
  (677.c:31#2)^f();
  (677.c:31#2)^choose {
   -->
    (677.c:31#2)^guard(! (0-_int32 ==_int32 0));
    (677.c:31#2)^guard(! (x_int32 ==_int32 0));
    (677.c:32#4)^x =(int32) 1;
   -->
    (677.c:31#2)^choose {
     -->
      (677.c:31#2)^guard(! (0-_int32 ==_int32 0));
      (677.c:31#2)^guard((x_int32 ==_int32 0));
     -->
      (677.c:31#2)^guard((0-_int32 ==_int32 0));
    }
  }
}

int32 x = 0;

