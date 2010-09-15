/* mutation of 036 */
/*!npk widen*/

int i;
int rb_data[100];
int *rb_in;

void main(void)
{
  rb_in = rb_data;
  for (i = 0 ; i < 100; i++) {
    rb_in += 3;                 // ptr OOB
  }
}
