Newspeak output
---------------
002-a.c
002-b.c
void main(void) {
  (002-a.c:29#6)^int32 x;
  (002-a.c:30#6)^int32 i;
  (002-a.c:31#7)^ptr ptr;
  (002-a.c:32#2)^2- =(int32) v + (belongs[0,9] 1-_int32 * 32)_int32;
  (002-a.c:33#2)^0- =(ptr) &_320(v);
  (002-a.c:34#2)^2- =(int32) t + (belongs[0,9] 1-_int32 * 32)_int32;
  (002-a.c:35#2)^0- =(ptr) &_320(t);
}

int32[10] t = 0;
int32[10] v = 0;

