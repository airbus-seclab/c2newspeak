#!/usr/bin/env perl
use strict;
use warnings;
BEGIN {require 't/misc/skip_all.pl';}
use Test::Command tests => 13;

my $cmd = './solver';

exit_is_num ($cmd, 0, 'Silent fail is not an error');
stdout_like ($cmd, qr/^Usage/, 'Help message without args');

foreach (qw/-h --help/) {
  stdout_like ([$cmd, $_], qr/^Usage/, 'Help message with '.$_);
}

foreach (qw/-V --version/) {
  stdout_like ([$cmd, $_], qr/^Version/,                           'Version with '.$_);
  stdout_like ([$cmd, $_], qr/LGPLv2/,                             'License with '.$_);
  stdout_like ([$cmd, $_], qr/Etienne Millon/,                     'Author with ' .$_);
  stdout_like ([$cmd, $_], qr/etienne DOT millon AT eads DOT net/, 'Email with '  .$_);
}

exit_is_num ([$cmd, 't/misc/empty.c'], 0, 'With a .c file');
