#!/usr/bin/env perl
use strict;
use warnings;
use Test::Command;
use Test::More;

my @tests = <t/input_language/*/*.c>;

plan tests => (2 * scalar @tests);

foreach (@tests) {
  /(not_ok|ok)\/(\w+)\.c/ or die "Bad filename format : $_";
  my $should_be_ok = ($1 eq 'ok');
  my $testname = $2;
  my $cmd = Test::Command->new (cmd => ['./solver', '--cfg', $_]);
  $cmd->exit_cmp_ok(($should_be_ok ? '==' : '>'), 0,
                     $testname.' : return value');
  $cmd->stderr_unlike(qr/Fatal error/, $testname.' : no exception');
}

