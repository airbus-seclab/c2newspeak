Newspeak output
---------------
main() {
  (30)^float32;
  (31)^float64;
  (32)^int32;
  (34)^2- =(float32) (2-_float32 *. 2-_float32);
  (35)^2- =(float32) (float32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (36)^2- =(float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (37)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (38)^2- =(float32) (float32 <= float64) (1-_float64 *. 1-_float64);
  (39)^2- =(float32) (float32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (40)^2- =(float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (41)^2- =(float32) (float32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (42)^2- =(float32) (float32 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (44)^1- =(float64) (float64 <= float32) (2-_float32 *. 2-_float32);
  (45)^1- =(float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (46)^1- =(float64) (float64 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (47)^1- =(float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (48)^1- =(float64) (1-_float64 *. 1-_float64);
  (49)^1- =(float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (50)^1- =(float64) (float64 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (51)^1- =(float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (52)^1- =(float64) (float64 <= int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
  (54)^0- =(int32) (int32 <= float32) (2-_float32 *. 2-_float32);
  (55)^0- =(int32) (int32 <= float64) ((float64 <= float32) 2-_float32 *. 1-_float64);
  (56)^0- =(int32) (int32 <= float32) (2-_float32 *. (float32 <= int32) 0-_int32);
  (57)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= float32) 2-_float32);
  (58)^0- =(int32) (int32 <= float64) (1-_float64 *. 1-_float64);
  (59)^0- =(int32) (int32 <= float64) (1-_float64 *. (float64 <= int32) 0-_int32);
  (60)^0- =(int32) (int32 <= float32) ((float32 <= int32) 0-_int32 *. 2-_float32);
  (61)^0- =(int32) (int32 <= float64) ((float64 <= int32) 0-_int32 *. 1-_float64);
  (62)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 * 0-_int32);
}


