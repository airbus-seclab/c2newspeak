Warning: 845.c:72#0: assembly directive '' '=r' '0' ignored
Warning: 845.c:72#0: block within expression accepted
Warning: 845.c:72#0: block within expression accepted
Warning: 845.c:72#81: cast to void accepted
Warning: 845.c:72#131: expression of type signed integer used as an array index accepted
Warning: 845.c:72#131: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Warning: 845.c:72#1: dirty cast from integer to pointer accepted
Warning: 845.c:66#99: extern global variable __per_cpu_offset accepted
Newspeak output
---------------
int32 (845.c:69#18)^!845.c.0.hlt_works(int32 cpu) {
  (845.c:72#30)^{
    ptr __vpp_verify;
    (845.c:72#30)^__vpp_verify =(ptr) nil;
  }
  (845.c:72#131)^{
    uint32 __ptr;
    (845.c:72#1)^!return =(int32) [(ptr) coerce[0,4294967295] (__ptr_uint32 + __per_cpu_offset + (belongs[0,31] cpu_int32 * 32)_uint32)]1408 + 40_int8;
  }
}

uint32[32] __per_cpu_offset;

