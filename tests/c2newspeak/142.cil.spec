Newspeak output
---------------
142.c
void main(void) {
  (142.c:30#1134)^float32 x;
  (142.c:31#1146)^float64 y;
  (142.c:32#1155)^int32 z;
  (142.c:34#1163)^x =(float32) (x_float32 *. x_float32);
  (142.c:35#1176)^x =(float32) (float32 <= float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:36#1189)^x =(float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:37#1202)^x =(float32) (float32 <= float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:38#1215)^x =(float32) (float32 <= float64) (y_float64 *. y_float64);
  (142.c:39#1228)^x =(float32) (float32 <= float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:40#1241)^x =(float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:41#1254)^x =(float32) (float32 <= float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:42#1267)^x =(float32) (float32 <= int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
  (142.c:44#1281)^y =(float64) (float64 <= float32) (x_float32 *. x_float32);
  (142.c:45#1294)^y =(float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:46#1307)^y =(float64) (float64 <= float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:47#1320)^y =(float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:48#1333)^y =(float64) (y_float64 *. y_float64);
  (142.c:49#1346)^y =(float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:50#1359)^y =(float64) (float64 <= float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:51#1372)^y =(float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:52#1385)^y =(float64) (float64 <= int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
  (142.c:54#1399)^z =(int32) (int32 <= float32) (x_float32 *. x_float32);
  (142.c:55#1412)^z =(int32) (int32 <= float64) ((float64 <= float32) x_float32 *. y_float64);
  (142.c:56#1425)^z =(int32) (int32 <= float32) (x_float32 *. (float32 <= int32) z_int32);
  (142.c:57#1438)^z =(int32) (int32 <= float64) (y_float64 *. (float64 <= float32) x_float32);
  (142.c:58#1451)^z =(int32) (int32 <= float64) (y_float64 *. y_float64);
  (142.c:59#1464)^z =(int32) (int32 <= float64) (y_float64 *. (float64 <= int32) z_int32);
  (142.c:60#1477)^z =(int32) (int32 <= float32) ((float32 <= int32) z_int32 *. x_float32);
  (142.c:61#1490)^z =(int32) (int32 <= float64) ((float64 <= int32) z_int32 *. y_float64);
  (142.c:62#1503)^z =(int32) coerce[-2147483648,2147483647] (z_int32 * z_int32);
}


