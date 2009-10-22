#!/usr/bin/env perl
use strict;
use warnings;
BEGIN {require 't/misc/skip_all.pl';}
use YAML::Syck;
use File::Slurp;
use Test::More;

my @src = <t/src/*.c>;

my $cmd = 'npksolver --cfg';

my @tests = <t/cfg/*.yml>;

plan tests => scalar @tests;

foreach (@tests) {
TODO: {
  local $TODO = 'Not yet implemented' if 0;
  /\/(\d+)\.yml$/ or die "Bad filename format : $_";
  my $test_num = $1;
  my ($src_fname) = grep /$test_num/, @src;

  die "Cannot find $src_fname" unless -e $src_fname;

  my $exp_text = read_file $_;
  my $exp_yaml = Load ($exp_text);

  my $output = qx#$cmd $src_fname#;
  my $got_yaml = Load ($output);

  is_deeply ($got_yaml, $exp_yaml, "CFG (YAML) for $test_num");
  }
}