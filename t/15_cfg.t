#!/usr/bin/env perl
use strict;
use warnings;
use YAML::Syck;
use File::Slurp;
use Test::More;

my @tests = <t/cfg/*.c>;

plan tests => scalar @tests;

foreach (@tests) {
TODO: {
  local $TODO = 'Not yet implemented' if 0;
  /\/(\w+)\.c$/ or die "Bad filename format : $_";
  my $test_name = $1;
  my $yml_fname = $_;
  $yml_fname =~ s/c$/yml/;
  die "Cannot find $yml_fname" unless -e $yml_fname;
  my $exp_text = read_file $yml_fname;
  my $exp_yaml = Load ($exp_text);

  my $output = qx#./solver $_#;
  my $got_yaml = Load ($output);

  is_deeply ($got_yaml, $exp_yaml, "CFG (YAML) for $test_name.c");
  }
}
