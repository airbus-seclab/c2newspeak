Newspeak output
---------------
210.c
void f(int32) {
}

void main(void) {
  (210.c:30#6)^int32 x;
  (210.c:32#2)^int32 !tmp-1073741820;
  (210.c:32#2)^0- =(int32) 1-_int32;
  (210.c:32#2)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
  (210.c:32#2)^{
    int32 f.arg1;
    (210.c:32#2)^0- =(int32) 1-_int32;
    (210.c:32#2)^f();
  }
}


