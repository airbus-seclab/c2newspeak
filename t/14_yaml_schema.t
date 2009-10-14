#!/usr/bin/env perl
use strict;
use warnings;
use Test::More;
use Test::Command;

sub kwalify_schema_ok {
  my $fname = shift;
  stdout_unlike (['kwalify', '-m', $fname], qr/INVALID/, "Schema : $fname");
}

sub kwalify_ok {
  my ($schema, $fname) = @_;
  stdout_unlike (['kwalify', '-f', $schema, $fname], qr/INVALID/,
                  "YAML : $fname against $schema");
}

my @cfg = <t/cfg/*.yml>;

plan tests => 1 + scalar @cfg;

&kwalify_schema_ok ('t/misc/cfg.yml');

foreach (@cfg) {
  &kwalify_ok ('t/misc/cfg.yml', $_);
}
