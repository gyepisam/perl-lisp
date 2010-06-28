use strict;
use LispTest;
use Test::More tests => 9;

lisp_is(<<'EOT', '0', 'decf by default value');
(setq a 1)
(decf a)
EOT

lisp_is(<<'EOT', '2', 'incf by default value');
(setq a 1)
(incf a)
EOT

lisp_is(<<'EOT', '1', 'decf by 0');
(setq a 1)
(decf a 0)
EOT

lisp_is(<<'EOT', '1', 'incf by 0');
(setq a 1)
(incf a 0)
EOT

lisp_is(<<'EOT', '1', 'decf by 1');
(setq a 2)
(decf a 1)
EOT

lisp_is(<<'EOT', '2', 'incf by 1');
(setq a 1)
(incf a 1)
EOT

lisp_is(<<'EOT', '2', 'decf by 3');
(setq a 5)
(decf a 3)
EOT

lisp_is(<<'EOT', '9', 'incf by 7');
(setq a 2)
(incf a 7)
EOT

lisp_is(<<'EOT', '9', 'incf by 7 in a lexical environment');
(let ((a 2))
  (incf a 7))
EOT

