use strict;
use LispTest;
use Test::More tests => 9;

lisp_is(<<'EOT', '()', 'hash-keys on empty hash');
(setq hash (hash))
(hash-keys hash)
EOT

lisp_is(<<'EOT', '()', 'hash-values on empty hash');
(setq hash (hash))
(hash-values hash)
EOT


lisp_is(<<'EOT', q[("foo")], 'hash-keys on single hash entry');
(setq hash (hash "foo" 1))
(hash-keys hash)
EOT


lisp_is(<<'EOT', q[(1)], 'hash-values on single hash entry');
(setq hash (hash "foo" 1))
(hash-values hash)
EOT

lisp_is(<<'EOT', q[("abel" "baker" "charlie")], 'hash-keys on multiple hash entries');
(setq *hash* (hash "abel" 1 "baker" 2 "charlie" 3))
(sort-ascii (hash-keys *hash*))
EOT


lisp_is(<<'EOT', q[(1 2 3)], 'hash-values on multiple hash entries');
(setq *hash* (hash "abel" 1 "baker" 2 "charlie" 3))
(sort-numeric (hash-values *hash*))
EOT

lisp_is(<<'EOT', 'NIL', 'retrieve non-existent item');
(let ((*hash* (hash "abel" 1 "baker" 2 "charlie" 3)))
  (gethash *hash* "dollar"))
EOT

lisp_is(<<'EOT', '2', 'retrieve existing item');
(let ((*hash* (hash "abel" 1 "baker" 2 "charlie" 3)))
  (gethash *hash* "baker"))
EOT

lisp_is(<<'EOT', q[()], 'no key/value leakage');
(setq hash (hash))
(sethash hash "key" "value")
(sethash hash 3 "value3")
(remhash hash "key")
(remhash hash 3)
(hash-values hash)
EOT
