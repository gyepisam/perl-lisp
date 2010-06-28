use strict;
use LispTest;
use Test::More tests => 2;

lisp_is(<<'EOT', '660', 'lambda');
((lambda (x y) (* x y)) 12 55)
EOT

lisp_is(<<'EOT', '660', 'fset lambda');
(fset 'foo '(lambda (x y) (* x y)))
(foo 12 55)
EOT

