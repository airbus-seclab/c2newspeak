Warning: 158.c:29#2: dirty cast from pointer to integer accepted
Warning: 158.c:29#2: dirty cast from pointer to integer accepted
Newspeak output
---------------
158.c
void main(void) {
  (158.c:27#6)^int32 x;
  (158.c:28#7)^int8[10] t;
  (158.c:29#2)^choose {
   -->
    (158.c:29#2)^guard((x_int32 > (int32) focus80 &(t)));
   -->
    (158.c:29#2)^guard(! (x_int32 > (int32) focus80 &(t)));
  }
}


