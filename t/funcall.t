use strict;
use LispTest;
use Test::More tests => 3;


lisp_is(<<'EOT', '6', 'funcall -- single arg');
(funcall  #'+ 6)
EOT

lisp_is(<<'EOT', '6', 'funcall -- multiple args');
(funcall  #'+ 1 2 3)
EOT

lisp_is(<<'EOT', '6', 'funcall -- multiple args');
(funcall  #'+ (+ 1 2) 3)
EOT
