int x, c;
void main(void)
{
  x = 1;
  c = 0;
  do {
    if (x != 1) {
      /*!npk assert false */
      x = 2;
    }
    c += x;
  } while (c < 10);
  /*!npk assert eq x 1 */
}
