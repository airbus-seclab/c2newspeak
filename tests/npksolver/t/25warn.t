#!/usr/bin/env perl
use strict;
use warnings;
BEGIN {require 't/misc/skip_all.pl';}
use YAML::Syck;
use File::Slurp;
use Test::More;

my @src = <t/src/*.c>;

my @tests = <t/warnings/*.spec>;

my $cmd = 'npksolver';

plan tests => scalar @tests;

foreach (@tests) {
TODO: {
  local $TODO = 'Not yet implemented' if /23/;
  /\/(\d+)\.spec$/ or die "Bad filename format : $_";
  my $test_num = $1;
  my ($src_fname) = grep /$test_num/, @src;

  die "Cannot find $src_fname" unless -e $src_fname;

  my $exp_text = read_file $_;

  my $output = qx#$cmd $src_fname#;

  is ($output, $exp_text, "Warnings for $test_num");
  }
}
