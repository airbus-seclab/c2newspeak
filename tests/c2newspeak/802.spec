Warning: 802.c:11#10: missing initializers for structure accepted
Warning: 802.c:11#10: dirty cast from integer to pointer accepted
Newspeak output
---------------
{ ptr 0; int32 32; int32 64; ptr 96; ptr 128; { int32 0; }32 160; }192 x;
(802.c:11#10)^x + 96 =(ptr) nil;
(802.c:11#10)^x + 128 =(ptr) (ptr) ~ 0;
(802.c:11#10)^x + 160 =(int32) 1;
(802.c:11#10)^x =(ptr) nil;
(802.c:11#10)^x + 32 =(int32) 0;
(802.c:11#10)^x + 64 =(int32) 0;

