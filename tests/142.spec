Newspeak output
---------------
142.c
main() {
  (142.c:30#1134)^float32;
  (142.c:31#1146)^float64;
  (142.c:32#1155)^int32;
  (142.c:34#1163)^2- =(float32) (2-_float32 *. 2-_float32);
  (142.c:35#1176)^2- =(float32) (float32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:36#1189)^2- =(float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:37#1202)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:38#1215)^2- =(float32) (float32 <= float64) (1-_float64 *. 1-_float64);
  (142.c:39#1228)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:40#1241)^2- =(float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:41#1254)^2- =(float32) (float32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:42#1267)^2- =(float32) (float32 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (142.c:44#1281)^1- =(float64) (float64 <= float32) (2-_float32 *. 2-_float32);
  (142.c:45#1294)^1- =(float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:46#1307)^1- =(float64) (float64 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:47#1320)^1- =(float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:48#1333)^1- =(float64) (1-_float64 *. 1-_float64);
  (142.c:49#1346)^1- =(float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:50#1359)^1- =(float64) (float64 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:51#1372)^1- =(float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:52#1385)^1- =(float64) (float64 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (142.c:54#1399)^0- =(int32) (int32 <= float32) (2-_float32 *. 2-_float32);
  (142.c:55#1412)^0- =(int32) (int32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (142.c:56#1425)^0- =(int32) (int32 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (142.c:57#1438)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (142.c:58#1451)^0- =(int32) (int32 <= float64) (1-_float64 *. 1-_float64);
  (142.c:59#1464)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (142.c:60#1477)^0- =(int32) (int32 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (142.c:61#1490)^0- =(int32) (int32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (142.c:62#1503)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
}


