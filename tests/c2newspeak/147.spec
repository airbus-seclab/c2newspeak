Newspeak output
---------------
147.c
void main(void) {
  (147.c:27#8)^float32 x;
  (147.c:28#2)^choose {
   -->
    (147.c:28#2)^guard((0-_float32 > (float32 <= int32) 10));
   -->
    (147.c:28#2)^guard(! (0-_float32 > (float32 <= int32) 10));
  }
}


