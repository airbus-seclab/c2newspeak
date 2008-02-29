int8[6] !param_str1 = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};
int8[6] !param_str2 = {0: int8 87;8: int8 111;16: int8 114;24: int8 108;32: int8 100;40: int8 0};
ptr[2] !ptr_array = {32: ptr &_48(!param_str2);0: ptr &_48(!param_str1)};
(:-1#-1)^int32;
(:-1#-1)^int32;
(:-1#-1)^ptr;
(:-1#-1)^0- =(ptr) &_64(!ptr_array);
(:-1#-1)^1- =(int32) 2;
(:-1#-1)^main();

