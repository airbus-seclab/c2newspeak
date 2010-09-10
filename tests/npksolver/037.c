
int rb_data[100];
int *rb_in;

void main(void)
{
  rb_in = rb_data;
  if (rb_in < rb_data + 97) {
    rb_in += 3;
  } else {
    rb_in -= 97;
  }
}
