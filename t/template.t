use strict;
use LispTest;
use Test::More tests => 1;

lisp_is(<<'EOT', 'nil', 'simple test');
;;foo
EOT

