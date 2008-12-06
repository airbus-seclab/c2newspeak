Warning: 140.c:32#1149: dirty cast from pointer to integer accepted
Newspeak output
---------------
140.c
void main(void) {
  (140.c:30#1132)^int32 x;
  (140.c:31#1141)^ptr ptr;
  (140.c:32#1149)^choose {
    | (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
    | ! (coerce[0,4294967295] 1-_int32 ==_uint32 (uint32) 0-_ptr) -->
  }
}


