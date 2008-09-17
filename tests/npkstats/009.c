int var_int;
int var_int2;

char tab[10];
int tab2[30];
 
int plus(int x, int y){
  return x+y;
}

int neg(int x){
  char tab[10];
  return -x;
}

void main(void){
  var_int = 0;
  var_int2 = 5;
  plus(var_int, var_int);
  neg(var_int2);
  tab[3] = 'a';
  tab2[0] = 1;
}
