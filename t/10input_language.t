#!/usr/bin/env perl
use strict;
use warnings;
use Test::Command;
use Test::More;

my @tests = <t/input_language/*.c>;

plan tests => (2 * scalar @tests);

foreach (@tests) {
  /\d+_(\w+)\.c/ or die "Bad filename format : $_";
  my $testname = $1;
  my $cmd = Test::Command->new (cmd => ['./solver', $_]);
  $cmd->exit_cmp_ok('>', 0, $testname.' : return value');
  $cmd->stderr_unlike(qr/Fatal error/, $testname.' : no exception');
}

