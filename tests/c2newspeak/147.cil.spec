Newspeak output
---------------
147.c
void main(void) {
  (147.c:27#1074)^float32 x;
  (147.c:28#1079)^choose {
   -->
    (147.c:28#1079)^guard((x_float32 > (float32 <= int32) 10));
   -->
    (147.c:28#1079)^guard(! (x_float32 > (float32 <= int32) 10));
  }
}


