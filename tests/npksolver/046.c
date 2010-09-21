/* Derived from 036 */

/*!npk widen */

int i;                           // when rb_in -> (rb_data, n),
int rb_data[100];                //         pointer arithmetics ok
int *rb_in;                      // <=>     0 <= n < 100

void main(void)
{
                                 // Loop invariant :
                                 // exists n in [0;99]. rb_in -> (rb_data, n)
  rb_in = &rb_data[0];               // base case : n = 0
  while (1) {   // induction :
    rb_in += 3;
  }                              // induction ok
}
