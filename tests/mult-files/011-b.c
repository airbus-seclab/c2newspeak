extern int TmIoE[];

# 4 "nctmf2.c"
static int *TmD= (&TmIoE[1]);

int *f()
{
 return TmD;
}






