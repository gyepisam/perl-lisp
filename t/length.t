use strict;
use LispTest;
use Test::More tests => 4;


cmp_ok(lisp(<<EOT), '==', 0, 'empty list length');
(length ())
EOT

cmp_ok(lisp(<<EOT), '==', 3, 'non-empty list length');
(setq list-item (list 'Frogs 'love 'water))
(length list-item)
EOT

cmp_ok(lisp(<<EOT), '==', 0, 'empty string length');
(length "")
EOT

cmp_ok(lisp(<<EOT), '==', 16, 'non-empty string length');
(setq string-item "Frogs love water")
(length string-item)
EOT

=pod

cmp_ok(lisp(<<EOT), '==', 0, 'nil length');
(length nil)
EOT

=cut
