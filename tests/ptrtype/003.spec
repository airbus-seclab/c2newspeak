void (003.c:1#5)^f(void) {
  (003.c:3#8)^Int x;
  (003.c:3#8)^x =(int32) (1 : Int);
  (003.c:4#8)^{
    Int y;
    (003.c:4#8)^y =(int32) (1 : Int);
    (003.c:5#8)^{
      Int a;
      (003.c:5#8)^a =(int32) (coerce[-2147483648,2147483647] (((x_Int : Int) + (y_Int : Int)) : Int) : Int);
      (003.c:6#8)^{
        Int b;
        (003.c:6#8)^b =(int32) (coerce[-2147483648,2147483647] (((x_Int : Int) - (y_Int : Int)) : Int) : Int);
        (003.c:7#8)^{
          Int c;
          (003.c:7#8)^c =(int32) (coerce[-2147483648,2147483647] (((x_Int : Int) * (y_Int : Int)) : Int) : Int);
          (003.c:8#8)^{
            Int d;
            (003.c:8#8)^d =(int32) (coerce[-2147483648,2147483647] (((x_Int : Int) / (y_Int : Int)) : Int) : Int);
            (003.c:9#8)^{
              Int e;
              (003.c:9#8)^e =(int32) (coerce[-2147483648,2147483647] (((0 : Int) - (x_Int : Int)) : Int) : Int);
              (003.c:10#8)^{
                Int f;
                (003.c:10#8)^f =(int32) (~ (x_Int : Int) : Int);
                (003.c:11#8)^{
                  Int g;
                  (003.c:11#8)^g =(int32) (! (y_Int : Int) : Int);
                  (003.c:12#8)^{
                    Int h;
                    (003.c:12#8)^{
                      Int tmp_firstpass!0;
                      (003.c:12#8)^choose {
                       -->
                        (003.c:12#8)^guard((! (((x_Int : Int) ==_int32 (0 : Int)) : Int) : Int));
                        (003.c:12#8)^tmp_firstpass!0 =(int32) (! (((y_Int : Int) ==_int32 (0 : Int)) : Int) : Int);
                       -->
                        (003.c:12#8)^guard((((x_Int : Int) ==_int32 (0 : Int)) : Int));
                        (003.c:12#8)^tmp_firstpass!0 =(int32) (0 : Int);
                      }
                      (003.c:12#8)^h =(int32) (tmp_firstpass!0_Int : Int);
                    }
                    (003.c:13#8)^{
                      Int i;
                      (003.c:13#8)^{
                        Int tmp_firstpass!1;
                        (003.c:13#8)^choose {
                         -->
                          (003.c:13#8)^guard((! (((x_Int : Int) ==_int32 (0 : Int)) : Int) : Int));
                          (003.c:13#8)^tmp_firstpass!1 =(int32) (1 : Int);
                         -->
                          (003.c:13#8)^guard((((x_Int : Int) ==_int32 (0 : Int)) : Int));
                          (003.c:13#8)^tmp_firstpass!1 =(int32) (! (((y_Int : Int) ==_int32 (0 : Int)) : Int) : Int);
                        }
                        (003.c:13#8)^i =(int32) (tmp_firstpass!1_Int : Int);
                      }
                      (003.c:14#8)^{
                        Int j;
                        (003.c:14#8)^j =(int32) (((x_Int : Int) ^ (y_Int : Int)) : Int);
                        (003.c:15#8)^{
                          Int k;
                          (003.c:15#8)^k =(int32) (((x_Int : Int) & (y_Int : Int)) : Int);
                          (003.c:16#8)^{
                            Int l;
                            (003.c:16#8)^l =(int32) (((x_Int : Int) | (y_Int : Int)) : Int);
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

