Warning: 844.c:20#0: assembly directive '1: jmp %l[t_no]
2:
.section .altinstructions,"a"
 .balign 4 
 .long 1b
 .long 0
 .byte %P0
 .byte 2b - 1b
 .byte 0
 .byte 0xff + 0 - (2b-1b)
.previous
' 'i' 't_no' ignored
Newspeak output
---------------
int32 (844.c:6#72)^!844.c.0.__static_cpu_has(uint8 bit) {
  (844.c:6#72)^do {
    (844.c:21#2)^!return =(int32) 1;
    (844.c:21#2)^goto lbl0;
    (844.c:23#2)^!return =(int32) 0;
  } with lbl0:
}


