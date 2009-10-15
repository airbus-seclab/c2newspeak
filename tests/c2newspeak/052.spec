Warning: 052.c:33#7: extra initializer for array accepted
Warning: 052.c:35#7: not enough initializers for array
Warning: 052.c:38#7: extra initializer for array accepted
Warning: 052.c:39#7: extra initializer for array accepted
Warning: 052.c:41#7: not enough initializers for array
Newspeak output
---------------
052.c
void main(void) {
  (052.c:31#8)^ptr x;
  (052.c:31#8)^0- =(ptr) focus48 &(!052.c.const_str_Hello);
  (052.c:32#7)^{
    int8[3] a1;
    (052.c:32#7)^0- =(int8) 1;
    (052.c:32#7)^0- + 8 =(int8) 2;
    (052.c:32#7)^0- + 16 =(int8) 3;
    (052.c:33#7)^{
      int8[2] a2;
      (052.c:33#7)^0- =(int8) 1;
      (052.c:33#7)^0- + 8 =(int8) 2;
      (052.c:34#7)^{
        int8[3] a3;
        (052.c:34#7)^0- =(int8) 1;
        (052.c:34#7)^0- + 8 =(int8) 2;
        (052.c:34#7)^0- + 16 =(int8) 3;
        (052.c:35#7)^{
          int8[4] a4;
          (052.c:35#7)^0- =(int8) 1;
          (052.c:35#7)^0- + 8 =(int8) 2;
          (052.c:35#7)^0- + 16 =(int8) 3;
          (052.c:35#7)^0- + 24 =(int8) 0;
          (052.c:37#7)^{
            int8[4] b1;
            (052.c:37#7)^0- =(int8) 97;
            (052.c:37#7)^0- + 8 =(int8) 98;
            (052.c:37#7)^0- + 16 =(int8) 99;
            (052.c:37#7)^0- + 24 =(int8) 0;
            (052.c:38#7)^{
              int8[2] b2;
              (052.c:38#7)^0- =(int8) 97;
              (052.c:38#7)^0- + 8 =(int8) 98;
              (052.c:39#7)^{
                int8[3] b3;
                (052.c:39#7)^0- =(int8) 97;
                (052.c:39#7)^0- + 8 =(int8) 98;
                (052.c:39#7)^0- + 16 =(int8) 99;
                (052.c:40#7)^{
                  int8[4] b4;
                  (052.c:40#7)^0- =(int8) 97;
                  (052.c:40#7)^0- + 8 =(int8) 98;
                  (052.c:40#7)^0- + 16 =(int8) 99;
                  (052.c:40#7)^0- + 24 =(int8) 0;
                  (052.c:41#7)^{
                    int8[5] b5;
                    (052.c:41#7)^0- =(int8) 97;
                    (052.c:41#7)^0- + 8 =(int8) 98;
                    (052.c:41#7)^0- + 16 =(int8) 99;
                    (052.c:41#7)^0- + 24 =(int8) 0;
                    (052.c:41#7)^0- + 32 =(int8) 0;
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

int8[6] !052.c.const_str_Hello;
(052.c:31#8)^!052.c.const_str_Hello =(int8) 72;
(052.c:31#8)^!052.c.const_str_Hello + 8 =(int8) 101;
(052.c:31#8)^!052.c.const_str_Hello + 16 =(int8) 108;
(052.c:31#8)^!052.c.const_str_Hello + 24 =(int8) 108;
(052.c:31#8)^!052.c.const_str_Hello + 32 =(int8) 111;
(052.c:31#8)^!052.c.const_str_Hello + 40 =(int8) 0;

