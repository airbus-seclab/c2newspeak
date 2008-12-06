Newspeak output
---------------
050.c
void main(void) {
}

int8[3] a1 = {0: int8 1;8: int8 2;16: int8 3};
int8[2] a2 = {0: int8 1;8: int8 2};
int8[3] a3 = {0: int8 1;8: int8 2;16: int8 3};
int8[4] a4 = {0: int8 1;8: int8 2;16: int8 3;24: int8 0};
int8[4] b1 = {0: int8 97;8: int8 98;16: int8 99;24: int8 0};
int8[2] b2 = {0: int8 97;8: int8 98};
int8[3] b3 = {0: int8 97;8: int8 98;16: int8 99};
int8[4] b4 = {0: int8 97;8: int8 98;16: int8 99;24: int8 0};
int8[5] b5 = {0: int8 97;8: int8 98;16: int8 99;24: int8 0;32: int8 0};


050.c:30: Warning: Too many initializers for array a2

050.c:35: Warning: Too many initializers for character array b2
