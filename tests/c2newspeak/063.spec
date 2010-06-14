Newspeak output
---------------
063.c
int32 f(int32 a) {
  (063.c:27#2)^!return =(int32) a_int32;
}

void main(void) {
  (063.c:31#6)^int32 x;
  (063.c:32#2)^{
    int32 !tmp0;
    (063.c:32#2)^!tmp0 <- f(1);
    (063.c:32#2)^x <- f(!tmp0_int32);
  }
  (063.c:34#8)^{
    int32 y;
    (063.c:35#4)^x =(int32) y_int32;
  }
}


