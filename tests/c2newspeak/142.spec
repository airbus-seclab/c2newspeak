Newspeak output
---------------
void (142.c:29#5)^main(void) {
  (142.c:30#8)^float32 x;
  (142.c:31#9)^float64 y;
  (142.c:32#6)^int32 z;
  (142.c:34#2)^x =(float32) (x_float32 *. x_float32);
  (142.c:35#2)^x =(float32) (float32 <= float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:36#2)^x =(float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:37#2)^x =(float32) (float32 <= float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:38#2)^x =(float32) (float32 <= float64) (y_float64 *. y_float64);
  (142.c:39#2)^x =(float32) (float32 <= float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:40#2)^x =(float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:41#2)^x =(float32) (float32 <= float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:42#2)^x =(float32) (float32 <= int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
  (142.c:44#2)^y =(float64) (float64 <= float32) (x_float32 *. x_float32);
  (142.c:45#2)^y =(float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:46#2)^y =(float64) (float64 <= float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:47#2)^y =(float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:48#2)^y =(float64) (y_float64 *. y_float64);
  (142.c:49#2)^y =(float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:50#2)^y =(float64) (float64 <= float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:51#2)^y =(float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:52#2)^y =(float64) (float64 <= int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
  (142.c:54#2)^z =(int32) (int32 <= float32) (x_float32 *. x_float32);
  (142.c:55#2)^z =(int32) (int32 <= float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:56#2)^z =(int32) (int32 <= float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:57#2)^z =(int32) (int32 <= float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:58#2)^z =(int32) (int32 <= float64) (y_float64 *. y_float64);
  (142.c:59#2)^z =(int32) (int32 <= float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:60#2)^z =(int32) (int32 <= float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:61#2)^z =(int32) (int32 <= float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:62#2)^z =(int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
}


