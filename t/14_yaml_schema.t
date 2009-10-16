#!/usr/bin/env perl
use strict;
use warnings;
BEGIN {require 't/misc/skip_all.pl';}
use Test::More;
use Test::Command;

sub kwalify_schema_ok {
  my $fname = shift;
  ok(-e $fname, "Schema $fname exists");
  stdout_unlike (['kwalify', '-m', $fname], qr/INVALID/, "Schema : $fname");
}

sub kwalify_ok {
  my ($schema, $fname) = @_;
  stdout_unlike (['kwalify', '-f', $schema, $fname], qr/INVALID/,
                  "YAML : $fname against $schema");
}

my @cfg    = <t/cfg/*.yml>;
my @solver = <t/solver/*.yml>;

my $schemas = 2;

plan tests => 2 * $schemas + scalar @cfg + scalar @solver;

&kwalify_schema_ok ('t/misc/cfg.yml');
&kwalify_schema_ok ('t/misc/solver.yml');

&kwalify_ok ('t/misc/cfg.yml',    $_) foreach (@cfg) ;
&kwalify_ok ('t/misc/solver.yml', $_) foreach (@solver) ;
