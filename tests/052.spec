Warning: Too many initializers for array in 052.c line 33
Warning: Too many initializers for array in 052.c line 38
Warning: Too many initializers for array in 052.c line 39
Newspeak output
---------------
main() {
  (31)^ptr;
  (32)^int1[3];
  (33)^int1[2];
  (34)^int1[3];
  (35)^int1[4];
  (37)^int1[4];
  (38)^int1[2];
  (39)^int1[3];
  (40)^int1[4];
  (41)^int1[5];
  (31)^9- =(ptr) &_6(!052.c.const_str_Hello);
  (32)^8- =(int1) 1;
  (32)^8- + 1 =(int1) 2;
  (32)^8- + 2 =(int1) 3;
  (33)^7- =(int1) 1;
  (33)^7- + 1 =(int1) 2;
  (34)^6- =(int1) 1;
  (34)^6- + 1 =(int1) 2;
  (34)^6- + 2 =(int1) 3;
  (35)^5- =(int1) 1;
  (35)^5- + 1 =(int1) 2;
  (35)^5- + 2 =(int1) 3;
  (35)^5- + 3 =(int1) 0;
  (37)^4- =(int1) 97;
  (37)^4- + 1 =(int1) 98;
  (37)^4- + 2 =(int1) 99;
  (37)^4- + 3 =(int1) 0;
  (38)^3- =(int1) 97;
  (38)^3- + 1 =(int1) 98;
  (39)^2- =(int1) 97;
  (39)^2- + 1 =(int1) 98;
  (39)^2- + 2 =(int1) 99;
  (40)^1- =(int1) 97;
  (40)^1- + 1 =(int1) 98;
  (40)^1- + 2 =(int1) 99;
  (40)^1- + 3 =(int1) 0;
  (41)^0- =(int1) 97;
  (41)^0- + 1 =(int1) 98;
  (41)^0- + 2 =(int1) 99;
  (41)^0- + 3 =(int1) 0;
  (41)^0- + 4 =(int1) 0;
}

const int1[6] !052.c.const_str_Hello = {0: int1 72;1: int1 101;2: int1 108;3: int1 108;4: int1 111;5: int1 0};

