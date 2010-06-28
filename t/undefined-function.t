use strict;
use LispTest;
use Test::More tests => 1;

eval {
  lisp('(this-function-is-not-defined)');
};

is($@ && $@ =~ /^undefined function/, 1, "error on undefined function");

