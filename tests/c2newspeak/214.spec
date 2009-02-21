Warning: 214.c:32#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
214.c
void f(int32, int32) {
}

void main(void) {
  (214.c:31#6)^int32 x;
  (214.c:31#6)^0- =(int32) 0;
  (214.c:32#2)^{
    int32 !tmp0;
    (214.c:32#2)^0- =(int32) 1-_int32;
    (214.c:32#2)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
    (214.c:32#2)^{
      int32 !tmp1;
      (214.c:32#2)^0- =(int32) 2-_int32;
      (214.c:32#2)^2- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1);
      (214.c:32#2)^{
        int32 f.arg1;
        (214.c:32#2)^0- =(int32) 1-_int32;
        (214.c:32#2)^{
          int32 f.arg2;
          (214.c:32#2)^0- =(int32) 3-_int32;
          (214.c:32#2)^f();
        }
      }
    }
  }
}


