Newspeak output
---------------
018.c
f1() {
  (018.c:32#1146)^int32 value_of_f2;
  (018.c:32#1146)^{
    int32 f2.arg1;
    (018.c:32#1146)^0- =(int32) 2-_int32;
    (018.c:32#1146)^f2();
  }
  (018.c:32#1146)^2- =(int32) 0-_int32;
}

f2() {
  (018.c:36#1184)^int32 value_of_f1;
  (018.c:36#1184)^{
    int32 f1.arg1;
    (018.c:36#1184)^0- =(int32) 2-_int32;
    (018.c:36#1184)^f1();
  }
  (018.c:36#1184)^2- =(int32) 0-_int32;
}


