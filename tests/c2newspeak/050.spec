Warning: 050.c:30#5: extra initializer for array accepted
Warning: 050.c:32#5: not enough initializers for array
Warning: 050.c:35#5: extra initializer for array accepted
Warning: 050.c:36#5: extra initializer for array accepted
Warning: 050.c:38#5: not enough initializers for array
Newspeak output
---------------
050.c
void main(void) {
}

int8[3] a1;
int8[2] a2;
int8[3] a3;
int8[4] a4;
int8[4] b1;
int8[2] b2;
int8[3] b3;
int8[4] b4;
int8[5] b5;
(050.c:38#5)^b5 =(int8) 97;
(050.c:38#5)^b5 + 8 =(int8) 98;
(050.c:38#5)^b5 + 16 =(int8) 99;
(050.c:38#5)^b5 + 24 =(int8) 0;
(050.c:38#5)^b5 + 32 =(int8) 0;
(050.c:37#5)^b4 =(int8) 97;
(050.c:37#5)^b4 + 8 =(int8) 98;
(050.c:37#5)^b4 + 16 =(int8) 99;
(050.c:37#5)^b4 + 24 =(int8) 0;
(050.c:36#5)^b3 =(int8) 97;
(050.c:36#5)^b3 + 8 =(int8) 98;
(050.c:36#5)^b3 + 16 =(int8) 99;
(050.c:35#5)^b2 =(int8) 97;
(050.c:35#5)^b2 + 8 =(int8) 98;
(050.c:34#5)^b1 =(int8) 97;
(050.c:34#5)^b1 + 8 =(int8) 98;
(050.c:34#5)^b1 + 16 =(int8) 99;
(050.c:34#5)^b1 + 24 =(int8) 0;
(050.c:32#5)^a4 =(int8) 1;
(050.c:32#5)^a4 + 8 =(int8) 2;
(050.c:32#5)^a4 + 16 =(int8) 3;
(050.c:32#5)^a4 + 24 =(int8) 0;
(050.c:31#5)^a3 =(int8) 1;
(050.c:31#5)^a3 + 8 =(int8) 2;
(050.c:31#5)^a3 + 16 =(int8) 3;
(050.c:30#5)^a2 =(int8) 1;
(050.c:30#5)^a2 + 8 =(int8) 2;
(050.c:29#5)^a1 =(int8) 1;
(050.c:29#5)^a1 + 8 =(int8) 2;
(050.c:29#5)^a1 + 16 =(int8) 3;

