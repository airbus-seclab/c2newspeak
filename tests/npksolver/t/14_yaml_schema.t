#!/usr/bin/env perl
use strict;
use warnings;
BEGIN {require 't/misc/skip_all.pl';}
use Test::More skip_all => 'kwalify tests disabled';
use Test::Command;
use File::Slurp;
use lib 't/lib';
use YAML::Syck;
use Kwalify qw/validate/;

sub kwalify_ok {
  my ($schema, $fname) = @_;
  my $sch_text = read_file $schema;
  my $sch_yaml = Load ($sch_text);
  my $yml_text = read_file $fname;
  my $yml_yaml = Load ($yml_text);
  ok(validate ($sch_yaml, $yml_yaml), "$fname against $schema");
}

my @cfg    = <t/cfg/*.yml>;
my @solver = <t/solver/*.yml>;

plan tests => scalar @cfg + scalar @solver;

&kwalify_ok ('t/misc/cfg.yml',    $_) foreach (@cfg) ;
&kwalify_ok ('t/misc/solver.yml', $_) foreach (@solver) ;
