Newspeak output
---------------
195.c
void main(void) {
  (195.c:27#1073)^int32 x;
  (195.c:28#1082)^int32 y;
  int32 tmp;
  (195.c:30#1090)^y =(int32) 1;
  (195.c:31#1099)^choose {
   -->
    (195.c:31#1099)^guard(! (x_int32 ==_int32 0));
    (195.c:31#1099)^choose {
     -->
      (195.c:31#1099)^guard(! (x_int32 ==_int32 0));
      (195.c:31#1099)^tmp =(int32) 1;
     -->
      (195.c:31#1099)^guard((x_int32 ==_int32 0));
      (195.c:31#1099)^tmp =(int32) 0;
    }
   -->
    (195.c:31#1099)^guard((x_int32 ==_int32 0));
    (195.c:31#1099)^tmp =(int32) 0;
  }
  (195.c:31#1099)^x =(int32) tmp_int32;
}


