Warning: 052.c:33#7: extra initializer for array accepted
Warning: 052.c:35#7: not enough initializers for array
Warning: 052.c:38#7: extra initializer for array accepted
Warning: 052.c:39#7: extra initializer for array accepted
Warning: 052.c:41#7: not enough initializers for array
Newspeak output
---------------
void main(void) {
  (052.c:31#8)^ptr x;
  (052.c:31#8)^x =(ptr) focus48 &(cstr!1!052.c.Hello);
  (052.c:32#7)^{
    int8[3] a1;
    (052.c:32#7)^a1 =(int8) 1;
    (052.c:32#7)^a1 + 8 =(int8) 2;
    (052.c:32#7)^a1 + 16 =(int8) 3;
    (052.c:33#7)^{
      int8[2] a2;
      (052.c:33#7)^a2 =(int8) 1;
      (052.c:33#7)^a2 + 8 =(int8) 2;
      (052.c:34#7)^{
        int8[3] a3;
        (052.c:34#7)^a3 =(int8) 1;
        (052.c:34#7)^a3 + 8 =(int8) 2;
        (052.c:34#7)^a3 + 16 =(int8) 3;
        (052.c:35#7)^{
          int8[4] a4;
          (052.c:35#7)^a4 =(int8) 1;
          (052.c:35#7)^a4 + 8 =(int8) 2;
          (052.c:35#7)^a4 + 16 =(int8) 3;
          (052.c:35#7)^a4 + 24 =(int8) 0;
          (052.c:37#7)^{
            int8[4] b1;
            (052.c:37#7)^b1 =(int8) 97;
            (052.c:37#7)^b1 + 8 =(int8) 98;
            (052.c:37#7)^b1 + 16 =(int8) 99;
            (052.c:37#7)^b1 + 24 =(int8) 0;
            (052.c:38#7)^{
              int8[2] b2;
              (052.c:38#7)^b2 =(int8) 97;
              (052.c:38#7)^b2 + 8 =(int8) 98;
              (052.c:39#7)^{
                int8[3] b3;
                (052.c:39#7)^b3 =(int8) 97;
                (052.c:39#7)^b3 + 8 =(int8) 98;
                (052.c:39#7)^b3 + 16 =(int8) 99;
                (052.c:40#7)^{
                  int8[4] b4;
                  (052.c:40#7)^b4 =(int8) 97;
                  (052.c:40#7)^b4 + 8 =(int8) 98;
                  (052.c:40#7)^b4 + 16 =(int8) 99;
                  (052.c:40#7)^b4 + 24 =(int8) 0;
                  (052.c:41#7)^{
                    int8[5] b5;
                    (052.c:41#7)^b5 =(int8) 97;
                    (052.c:41#7)^b5 + 8 =(int8) 98;
                    (052.c:41#7)^b5 + 16 =(int8) 99;
                    (052.c:41#7)^b5 + 24 =(int8) 0;
                    (052.c:41#7)^b5 + 32 =(int8) 0;
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

int8[6] cstr!1!052.c.Hello;
(052.c:31#8)^cstr!1!052.c.Hello =(int8) 72;
(052.c:31#8)^cstr!1!052.c.Hello + 8 =(int8) 101;
(052.c:31#8)^cstr!1!052.c.Hello + 16 =(int8) 108;
(052.c:31#8)^cstr!1!052.c.Hello + 24 =(int8) 108;
(052.c:31#8)^cstr!1!052.c.Hello + 32 =(int8) 111;
(052.c:31#8)^cstr!1!052.c.Hello + 40 =(int8) 0;

