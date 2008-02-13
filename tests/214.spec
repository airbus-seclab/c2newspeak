Warning: the order of execution of side-effects in expressions not specified, picking a random one, be careful in 214.c line 32
Newspeak output
---------------
214.c
f() {
}

main() {
  (214.c:31#1126)^int32;
  (214.c:31#1126)^0- =(int32) 0;
  (214.c:32#1135)^{
    int32;
    (214.c:32#1135)^0- =(int32) 1-_int32;
    (214.c:32#1135)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
    (214.c:32#1135)^{
      int32;
      (214.c:32#1135)^0- =(int32) 2-_int32;
      (214.c:32#1135)^2- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1);
      (214.c:32#1135)^{
        int32;
        (214.c:32#1135)^0- =(int32) 1-_int32;
        (214.c:32#1135)^{
          int32;
          (214.c:32#1135)^0- =(int32) 3-_int32;
          (214.c:32#1135)^f();
        }
      }
    }
  }
}


