Newspeak output
---------------
void main(void) {
  (738.c:29#2)^choose {
   -->
    (738.c:29#2)^guard((x_int32 ==_int32 0));
    (738.c:29#2)^z =(int32) ! (y_int32 ==_int32 0);
   -->
    (738.c:29#2)^guard(! (x_int32 ==_int32 0));
    (738.c:29#2)^z =(int32) 0;
  }
}

int32 x;
int32 y;
int32 z;

