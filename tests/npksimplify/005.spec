f() {
  (27)^2- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 0-_int4);
}

g() {
  (27)^int4 tmp;
  (32)^{
    int4 value_of_f;
    (32)^{
      int4 x;
      (32)^int4 y;
      (32)^1- =(int4) 4-_int4;
      (32)^0- =(int4) 4-_int4;
      (27)^2- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 0-_int4);
    }
    (32)^1- =(int4) 0-_int4;
  }
  (33)^2- =(int4) coerce[-2147483648,2147483647] (2 * 0-_int4);
}

main() {
  (33)^int4 a;
  (33)^int4 b;
  (39)^int4 value_of_g;
  (39)^{
    int4 x;
    (39)^0- =(int4) 3-_int4;
    (27)^{
      int4 tmp;
      (32)^{
        int4 value_of_f;
        (32)^{
          int4 x;
          (32)^int4 y;
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
