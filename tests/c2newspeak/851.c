// First class labels.
// http://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html

void f (void)
{
lbl:;
    void *p;
    p = &&lbl;
}
