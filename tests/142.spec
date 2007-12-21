Newspeak output
---------------
main() {
  (30)^float4;
  (31)^float8;
  (32)^int4;
  (34)^2- =(float4) (2-_float4 *. 2-_float4);
  (35)^2- =(float4) (float4 <= float8) ((float8 <= float4) 2-_float4 *. 1-_float8);
  (36)^2- =(float4) (2-_float4 *. (float4 <= int4) 0-_int4);
  (37)^2- =(float4) (float4 <= float8) (1-_float8 *. (float8 <= float4) 2-_float4);
  (38)^2- =(float4) (float4 <= float8) (1-_float8 *. 1-_float8);
  (39)^2- =(float4) (float4 <= float8) (1-_float8 *. (float8 <= int4) 0-_int4);
  (40)^2- =(float4) ((float4 <= int4) 0-_int4 *. 2-_float4);
  (41)^2- =(float4) (float4 <= float8) ((float8 <= int4) 0-_int4 *. 1-_float8);
  (42)^2- =(float4) (float4 <= int4) coerce[-2147483648,2147483647] (0-_int4 * 0-_int4);
  (44)^1- =(float8) (float8 <= float4) (2-_float4 *. 2-_float4);
  (45)^1- =(float8) ((float8 <= float4) 2-_float4 *. 1-_float8);
  (46)^1- =(float8) (float8 <= float4) (2-_float4 *. (float4 <= int4) 0-_int4);
  (47)^1- =(float8) (1-_float8 *. (float8 <= float4) 2-_float4);
  (48)^1- =(float8) (1-_float8 *. 1-_float8);
  (49)^1- =(float8) (1-_float8 *. (float8 <= int4) 0-_int4);
  (50)^1- =(float8) (float8 <= float4) ((float4 <= int4) 0-_int4 *. 2-_float4);
  (51)^1- =(float8) ((float8 <= int4) 0-_int4 *. 1-_float8);
  (52)^1- =(float8) (float8 <= int4) coerce[-2147483648,2147483647] (0-_int4 * 0-_int4);
  (54)^0- =(int4) (int4 <= float4) (2-_float4 *. 2-_float4);
  (55)^0- =(int4) (int4 <= float8) ((float8 <= float4) 2-_float4 *. 1-_float8);
  (56)^0- =(int4) (int4 <= float4) (2-_float4 *. (float4 <= int4) 0-_int4);
  (57)^0- =(int4) (int4 <= float8) (1-_float8 *. (float8 <= float4) 2-_float4);
  (58)^0- =(int4) (int4 <= float8) (1-_float8 *. 1-_float8);
  (59)^0- =(int4) (int4 <= float8) (1-_float8 *. (float8 <= int4) 0-_int4);
  (60)^0- =(int4) (int4 <= float4) ((float4 <= int4) 0-_int4 *. 2-_float4);
  (61)^0- =(int4) (int4 <= float8) ((float8 <= int4) 0-_int4 *. 1-_float8);
  (62)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 * 0-_int4);
}


