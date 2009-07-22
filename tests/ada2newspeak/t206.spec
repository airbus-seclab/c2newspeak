Newspeak output
---------------
t206.adb
int32 t206.id(int32) {
  (t206.adb:6#9)^1- =(int32) 0-_int32;
}

void t206.main(void) {
  (t206.adb:10#7)^int32 xn;
  (t206.adb:12#8)^{
    int32 x;
    (t206.adb:12#8)^0- =(int32) 2147483647;
    (t206.adb:12#8)^t206.id();
  }
  (t206.adb:14#8)^{
    int32 x;
    (t206.adb:14#8)^0- =(int32) belongs[-2147483648,2147483647] 2147483648;
    (t206.adb:14#8)^t206.id();
  }
}


