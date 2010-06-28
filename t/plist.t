use strict;
use LispTest;
use Test::More tests => 3;

lisp_is(<<'EOT', 'nil', 'get non-existent');
(setq foo nil)
(get foo "string")
EOT


lisp_is(<<'EOT', '"bar"', 'one item put & get');
(setq foo nil)
(put foo "string" "bar")
(get foo "string")
EOT


lisp_is(<<'EOT', q[("bar" "dos")], 'two item put & get');
(setq foo nil)
(put foo "string" "bar")
(put foo 2 "dos")
(list (get foo "string") (get foo 2))
EOT
