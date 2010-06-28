use strict;
use LispTest;
use Test::More tests => 1;

lisp_is(<<'EOT', '3', 'set symbol value indirectly');
(let ((a nil)
      (b (quote a)))
      (set b 3)
      a) ;;returns 3, not nil
EOT

