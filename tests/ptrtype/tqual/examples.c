static int maybe(void)
{
    int x;
    return x;
}

void f0_deref(void)
{
    int * user x;
    int y = *x; // error
}

void f1_set(void)
{
    int * user x;
    int * kernel y;
    y = x; // error with no subtyping
}

void f2_union_flow_insensitive(void)
{
    int * user x;
    int * kernel y;
    int *z;
    if (maybe()) {
        z = x; // qual(z) = qual(x)
    } else {
        z = y; // error : z has type user
    }
}

void f3_struct_rule(void)
{
    struct s {
        int *mp;
    };
    struct s * user p;
    int x = *(p->mp); // error : qual(p->mp) = user
}

void f4_path_qual(void)
{
    int * user * pp;
    int * user p = *pp; // safe
    int x = *p; // unsafe
}

void f5_memcpy_set(void)
{
    int * user p;
    int *q;
    memcpy(&q,&p,sizeof(int*)); // should be compiled to Set(q,p,abi.ptr_size)
    int x = *q; // error
}
