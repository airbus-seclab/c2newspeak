Newspeak output
---------------
void main(void) {
  (481.c:27#15)^uint32 x;
  (481.c:28#15)^uint32 y;
  (481.c:29#6)^int32 z;
  (481.c:31#2)^y =(uint32) coerce[0,4294967295] (1 << 31);
  (481.c:32#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << 31);
  (481.c:33#2)^x =(uint32) 31;
  (481.c:34#2)^y =(uint32) coerce[0,4294967295] (1 << x_uint32);
  (481.c:35#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << x_uint32);
  (481.c:37#2)^y =(uint32) coerce[0,4294967295] (1 << 32);
  (481.c:38#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << 32);
  (481.c:39#2)^x =(uint32) 32;
  (481.c:40#2)^y =(uint32) coerce[0,4294967295] (1 << x_uint32);
  (481.c:41#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << x_uint32);
}


