void main() {
  int stop;
  int i;
  
  i = 0;
  do {
    i++;
    stop = (i >= 10);
  } while (!stop);
  
  /*
    do {
    i++;
    } while (!(i >= 10));
   */
}
