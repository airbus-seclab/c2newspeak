// from linux : arch/x86/include/asm/cpufeature.h

typedef enum { false = 0, true = 1 } bool;
typedef unsigned char u8;

static inline __attribute__((always_inline)) __attribute__((pure)) bool __static_cpu_has(u8 bit)
{

  asm goto("1: jmp %l[t_no]\n"
    "2:\n"
    ".section .altinstructions,\"a\"\n"
    " " ".balign 4" " " "\n"
    " " ".long" " " "1b\n"
    " " ".long" " " "0\n"
    " .byte %P0\n"
    " .byte 2b - 1b\n"
    " .byte 0\n"
    " .byte 0xff + 0 - (2b-1b)\n"
    ".previous\n"
    : : "i" (bit) : : t_no);
  return true;
 t_no:
  return false;
}
