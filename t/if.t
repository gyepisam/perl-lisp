use strict;
use LispTest;
use Test::More tests => 5;

lisp_is(<<'EOT', 'NIL', 'if form -- true condition, empty body');
(if t)
EOT

lisp_is(<<'EOT', 'NIL', 'if form -- false condition, empty body');
(if nil)
EOT

lisp_is(<<'EOT', 1, 'if form -- true condition, then');
(if t 
    1
    (not-reached "part 2"))
EOT

lisp_is(<<'EOT', 1, 'if form -- false condition, else');
(if nil 
    (not-reached "part 1")
    1)
EOT

lisp_err(<<'EOT', 'if form has too many parts');
(if nil
    (not-reached "part 1")
    (not-reached "part 2")
    (not-reached "part 3"))
EOT

