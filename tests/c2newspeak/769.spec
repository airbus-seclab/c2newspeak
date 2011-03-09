Warning: 769.c:45#0: assembly directive 'pushf
popl %%eax
xorl $0x200000, %%eax
movl %%eax, %%ecx
andl $0x200000, %%ecx
pushl %%eax
popf
pushf
popl %%eax
andl $0x200000, %%eax
xorl %%eax, %%ecx
movl %%ecx, %0
' '=r' 'eax' 'ecx' ignored
Newspeak output
---------------
void (769.c:26#5)^main(void) {
  (769.c:28#5)^int32 result;
}


