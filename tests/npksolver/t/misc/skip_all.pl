use strict;
use warnings;
BEGIN {
  unless ($ENV{'NPK_RUN_TESTSUITE'}) {
    require Test::More;
    Test::More->import (skip_all =>
      'Set NPK_RUN_TESTSUITE to enable this test suite');
  }
}

1;
