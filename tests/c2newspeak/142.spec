Newspeak output
---------------
142.c
void main(void) {
  (142.c:30#8)^float32 x;
  (142.c:31#9)^float64 y;
  (142.c:32#6)^int32 z;
  (142.c:34#2)^2- =(float32) (2-_float32 *. 2-_float32);
  (142.c:35#2)^2- =(float32) (float32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:36#2)^2- =(float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:37#2)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:38#2)^2- =(float32) (float32 <= float64) (1-_float64 *. 1-_float64);
  (142.c:39#2)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:40#2)^2- =(float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:41#2)^2- =(float32) (float32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:42#2)^2- =(float32) (float32 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (142.c:44#2)^1- =(float64) (float64 <= float32) (2-_float32 *. 2-_float32);
  (142.c:45#2)^1- =(float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:46#2)^1- =(float64) (float64 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:47#2)^1- =(float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:48#2)^1- =(float64) (1-_float64 *. 1-_float64);
  (142.c:49#2)^1- =(float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:50#2)^1- =(float64) (float64 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:51#2)^1- =(float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:52#2)^1- =(float64) (float64 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (142.c:54#2)^0- =(int32) (int32 <= float32) (2-_float32 *. 2-_float32);
  (142.c:55#2)^0- =(int32) (int32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:56#2)^0- =(int32) (int32 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:57#2)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:58#2)^0- =(int32) (int32 <= float64) (1-_float64 *. 1-_float64);
  (142.c:59#2)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:60#2)^0- =(int32) (int32 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:61#2)^0- =(int32) (int32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:62#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
}


