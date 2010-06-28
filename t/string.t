use strict;
use LispTest;
use Test::More tests => 8;

lisp_is(<<'EOT', '"foo"', 'simple string');
"foo"
EOT

lisp_is(<<'EOT', '3', 'string length');
(length "foo")
EOT

lisp_is(<<'EOT', '"aps"', 'substring extraction 1');
(substr "flaps" 2)
EOT

lisp_is(<<'EOT', '"a"', 'substring extraction 2');
(substr "flaps" 2 1)
EOT


lisp_is(<<'EOT', '"apple"', 'lower case string');
(lc "APPLE")
EOT


lisp_is(<<'EOT', '"APPLE"', 'upper case string');
(uc "apple")
EOT

lisp_is(<<'EOT', '"first-second-third"', 'string join');
(setq foo "first")
(setq bar "second")
(setq str (join "-" foo bar "third"))
(join "-" foo bar "third")
EOT


lisp_is(<<'EOT', '"foo123"', "string concatenation");
(concat "foo" 123)
EOT
