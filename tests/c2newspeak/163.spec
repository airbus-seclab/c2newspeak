Warning: 163.c:26#6: not enough initializers for array
Newspeak output
---------------
void (163.c:28#5)^main(void) {
  (163.c:29#2)^x =(float32) (float32 <= float64) 1.0;
}

float32[3] x;
(163.c:26#6)^x =(float32) (float32 <= float64) 0.4;
(163.c:26#6)^x + 32 =(float32) (float32 <= float64) 9.4;
(163.c:26#6)^x + 64 =(float32) 0.;

