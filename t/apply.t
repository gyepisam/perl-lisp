use strict;
use LispTest;
use Test::More tests => 2;


lisp_is(<<'EOT', '6', 'apply -- single arg');
(apply  #'+ (quote (1 2 3))) ;'
EOT

lisp_is(<<'EOT', '9', 'apply -- multiple args');
(apply  #'+ 3 '(1 2 3)) ;''
EOT

