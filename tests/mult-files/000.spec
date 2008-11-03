Newspeak output
---------------
000-a.c
000-b.c
f() {
  (000-b.c:28#2)^[y_ptr]8 =(int8) 1;
}

main() {
  (000-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] !000-a.c.const_str_Hello = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};
int8[6] !000-b.c.const_str_Hello = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};
ptr x = {0: ptr &_48(!000-a.c.const_str_Hello)};
ptr y = {0: ptr &_48(!000-b.c.const_str_Hello)};

