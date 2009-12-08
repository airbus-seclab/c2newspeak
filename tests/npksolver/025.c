int i;
void main(void)
{
  i = 0;
  if (i == 1) {
    /*!npk assert false */
    i = 2;
  }
  /*!npk assert eq i 0 */
}
