Newspeak output
---------------
052.c
void main(void) {
  (052.c:31#1134)^ptr x;
  (052.c:32#1155)^int8[3] a1;
  (052.c:33#1180)^int8[2] a2;
  (052.c:34#1206)^int8[3] a3;
  (052.c:35#1232)^int8[4] a4;
  (052.c:37#1259)^int8[4] b1;
  (052.c:38#1280)^int8[2] b2;
  (052.c:39#1302)^int8[3] b3;
  (052.c:40#1324)^int8[4] b4;
  (052.c:41#1346)^int8[5] b5;
  (052.c:31#1129)^9- =(ptr) &_48(!052.c.const_str_Hello);
  (052.c:32#1150)^8- =(int8) 1;
  (052.c:32#1150)^8- + 8 =(int8) 2;
  (052.c:32#1150)^8- + 16 =(int8) 3;
  (052.c:33#1175)^7- =(int8) 1;
  (052.c:33#1175)^7- + 8 =(int8) 2;
  (052.c:34#1201)^6- =(int8) 1;
  (052.c:34#1201)^6- + 8 =(int8) 2;
  (052.c:34#1201)^6- + 16 =(int8) 3;
  (052.c:35#1227)^5- =(int8) 1;
  (052.c:35#1227)^5- + 8 =(int8) 2;
  (052.c:35#1227)^5- + 16 =(int8) 3;
  (052.c:35#1227)^5- + 24 =(int8) 0;
  (052.c:37#1254)^4- =(int8) 97;
  (052.c:37#1254)^4- + 8 =(int8) 98;
  (052.c:37#1254)^4- + 16 =(int8) 99;
  (052.c:37#1254)^4- + 24 =(int8) 0;
  (052.c:38#1275)^3- =(int8) 97;
  (052.c:38#1275)^3- + 8 =(int8) 98;
  (052.c:39#1297)^2- =(int8) 97;
  (052.c:39#1297)^2- + 8 =(int8) 98;
  (052.c:39#1297)^2- + 16 =(int8) 99;
  (052.c:40#1319)^1- =(int8) 97;
  (052.c:40#1319)^1- + 8 =(int8) 98;
  (052.c:40#1319)^1- + 16 =(int8) 99;
  (052.c:40#1319)^1- + 24 =(int8) 0;
  (052.c:41#1341)^0- =(int8) 97;
  (052.c:41#1341)^0- + 8 =(int8) 98;
  (052.c:41#1341)^0- + 16 =(int8) 99;
  (052.c:41#1341)^0- + 24 =(int8) 0;
  (052.c:41#1341)^0- + 32 =(int8) 0;
}

int8[6] !052.c.const_str_Hello;


052.c:33: Warning: Too many initializers for array a2

052.c:38: Warning: Too many initializers for character array b2
