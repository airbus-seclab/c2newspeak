f() {
  (27)^2- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 0-_int4);
}

g() {
  (31)^int4;
  (32)^{
    int4;
    (32)^{
      int4;
      (32)^int4;
      (32)^1- =(int4) 4-_int4;
      (32)^0- =(int4) 4-_int4;
      (27)^2- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 0-_int4);
    }
    (32)^1- =(int4) 0-_int4;
  }
  (33)^2- =(int4) coerce[-2147483648,2147483647] (2 * 0-_int4);
}

main() {
  (37)^int4;
  (37)^int4;
  (39)^int4;
  (39)^{
    int4;
    (39)^0- =(int4) 3-_int4;
    (31)^{
      int4;
      (32)^{
        int4;
        (32)^{
          int4;
          (32)^int4;
          (32)^1- =(int4) 4-_int4;
          (32)^0- =(int4) 4-_int4;
          (32)^f();
        }
        (32)^1- =(int4) 0-_int4;
      }
      (33)^2- =(int4) coerce[-2147483648,2147483647] (2 * 0-_int4);
    }
  }
  (39)^1- =(int4) 0-_int4;
}

