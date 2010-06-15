Warning: 509.c:32#0: block within expression accepted
Newspeak output
---------------
509.c
void main(void) {
  (509.c:27#6)^int32 y;
  (509.c:29#10)^int32 x;
  (509.c:30#6)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (509.c:28#2)^y =(int32) x_int32;
}


