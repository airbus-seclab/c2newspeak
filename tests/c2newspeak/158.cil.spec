Warning: 158.c:29#1091: dirty cast from pointer to integer accepted
Newspeak output
---------------
158.c
void main(void) {
  (158.c:27#1072)^int32 x;
  (158.c:28#1082)^int8[10] t;
  (158.c:29#1091)^choose {
   -->
    (158.c:29#1091)^guard((x_int32 > (int32) focus80 &(t)));
   -->
    (158.c:29#1091)^guard(! (x_int32 > (int32) focus80 &(t)));
  }
}


