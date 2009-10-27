void main(void)
{
  int i;
  i = 3;
  /*!npk assert eq i 3 */
  {
    int j;
    j = 5;
    /*!npk assert eq j 5 */
    /*!npk assert eq i 3 */
    i = 4;
  }
  /*!npk assert eq i 4 */
  i = 1;
  /*!npk assert eq i 1 */
}
