use strict;
use LispTest;
use Test::More tests => 1;

eval {
  lisp(<<'EOF');
((this-is-an-invalid-list-function))
EOF
};

is($@ && $@ =~ /^invalid list function/, 1, "error on invalid list function");

