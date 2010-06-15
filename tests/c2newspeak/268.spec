Newspeak output
---------------
268.c
void main(void) {
  (268.c:29#29)^{ int32 0; int32 32; }64 x;
  (268.c:29#32)^{ int32 0; int32 32; }64 y;
  (268.c:30#12)^ptr z;
  (268.c:31#2)^int8[4] tmp_firstpass!0;
  (268.c:31#2)^tmp_firstpass!0 =(int32) 2;
  (268.c:31#2)^f(1, focus32 &(tmp_firstpass!0));
  (268.c:32#2)^{
    int8[12] tmp_firstpass!1;
    (268.c:32#2)^tmp_firstpass!1 =(int32) 2;
    (268.c:32#2)^tmp_firstpass!1 + 32 =(float64) 1.0;
    (268.c:32#2)^f(1, focus96 &(tmp_firstpass!1));
    (268.c:33#2)^{
      int8[20] tmp_firstpass!2;
      (268.c:33#2)^tmp_firstpass!2 =64 x;
      (268.c:33#2)^tmp_firstpass!2 + 64 =64 y;
      (268.c:33#2)^tmp_firstpass!2 + 128 =(ptr) z_ptr;
      (268.c:33#2)^f(1, focus160 &(tmp_firstpass!2));
    }
  }
}


