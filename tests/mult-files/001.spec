Newspeak output
---------------
001-a.c
001-b.c
main() {
  (001-a.c:28#1084)^[x_ptr]8 =(int8) 0;
}

int8[6] !001-b.c.const_str_Hello = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};
ptr x = {0: ptr &_48(!001-b.c.const_str_Hello)};

