Newspeak output
---------------
211.c
int32 f(int32) {
  (211.c:29#2)^1- =(int32) x_int32;
}

void main(void) {
  (211.c:33#6)^int32 y;
  (211.c:35#2)^int32 !tmp0;
  (211.c:35#2)^0- =(int32) x_int32;
  (211.c:35#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (211.c:35#2)^{
    int32 value_of_f;
    (211.c:35#2)^{
      int32 f.arg1;
      (211.c:35#2)^0- =(int32) 2-_int32;
      (211.c:35#2)^f();
    }
    (211.c:35#2)^2- =(int32) 0-_int32;
  }
}

int32 x = 0;

