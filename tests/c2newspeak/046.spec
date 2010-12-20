Newspeak output
---------------
void main(void) {
  (046.c:30#6)^int32 x;
  (046.c:32#2)^choose {
   -->
    (046.c:32#2)^guard((x_int32 ==_int32 1));
   -->
    (046.c:32#2)^guard(! (x_int32 ==_int32 1));
  }
}


