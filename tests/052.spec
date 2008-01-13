Warning: Too many initializers for array in 052.c line 33
Warning: Too many initializers for array in 052.c line 38
Warning: Too many initializers for array in 052.c line 39
Newspeak output
---------------
main() {
  (31)^ptr;
  (32)^int8[3];
  (33)^int8[2];
  (34)^int8[3];
  (35)^int8[4];
  (37)^int8[4];
  (38)^int8[2];
  (39)^int8[3];
  (40)^int8[4];
  (41)^int8[5];
  (31)^9- =(ptr) &_48(!052.c.const_str_Hello);
  (32)^8- =(int8) 1;
  (32)^8- + 8 =(int8) 2;
  (32)^8- + 16 =(int8) 3;
  (33)^7- =(int8) 1;
  (33)^7- + 8 =(int8) 2;
  (34)^6- =(int8) 1;
  (34)^6- + 8 =(int8) 2;
  (34)^6- + 16 =(int8) 3;
  (35)^5- =(int8) 1;
  (35)^5- + 8 =(int8) 2;
  (35)^5- + 16 =(int8) 3;
  (35)^5- + 24 =(int8) 0;
  (37)^4- =(int8) 97;
  (37)^4- + 8 =(int8) 98;
  (37)^4- + 16 =(int8) 99;
  (37)^4- + 24 =(int8) 0;
  (38)^3- =(int8) 97;
  (38)^3- + 8 =(int8) 98;
  (39)^2- =(int8) 97;
  (39)^2- + 8 =(int8) 98;
  (39)^2- + 16 =(int8) 99;
  (40)^1- =(int8) 97;
  (40)^1- + 8 =(int8) 98;
  (40)^1- + 16 =(int8) 99;
  (40)^1- + 24 =(int8) 0;
  (41)^0- =(int8) 97;
  (41)^0- + 8 =(int8) 98;
  (41)^0- + 16 =(int8) 99;
  (41)^0- + 24 =(int8) 0;
  (41)^0- + 32 =(int8) 0;
}

int8[6] !052.c.const_str_Hello = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};

