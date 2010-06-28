use strict;
use LispTest;
use Test::More tests => 18;

lisp_is(<<EOF, '()', 'constructed empty list');
(list)
EOF

lisp_is(<<EOF, '(NIL)', 'constructed list containing single nil value');
(list nil)
EOF

lisp_is(<<EOF, '(1)', 'constructed list containing single non-nil value');
(list 1)
EOF

lisp_is(<<EOF, '("a" 1 5)', 'quoted list with multiple items');
(list "a" 1 5)
EOF

lisp_is(<<EOF, '("a" 1 5 3)', "push into a constructed list");
(push (list "a" 1 5) 3)
EOF

lisp_is(<<EOF, '("a" 1 5 3)', "push into a quoted list");
(push '("a" 1 5) 3) ;'
EOF

lisp_is(<<EOF, '5', "pop from a constructed list");
(pop (list "a" 1 5))
EOF

lisp_is(<<EOF, '5', "pop from a quoted list");
(pop '("a" 1 5));'
EOF

lisp_is(<<EOF, '("a" 1 5 3)', "push into a symbol containing a list #1");
(push (setq a (list "a" 1 5)) 3)
EOF

lisp_is(<<EOF, '("a" 1 5 3)', "push into a symbol containing a list #2");
(setq a '("a" 1 5)) ;'
(push a 3)
a
EOF

lisp_is(<<EOF, '5', "pop from a symbol containing a list");
(let ((a (list "a" 1 5)))
  (pop a))
EOF

lisp_is(<<EOF, '5', "pop from a quoted list");
(let ((a (list "a" 1 5)))
  (pop a))
EOF

lisp_is(<<EOF, '5', "list op in lexical env");
(let ((a '(5))) ;'
  (let ((a '(79)))  ;'
    (pop a))
  (elt a 0))
EOF

lisp_is(<<EOF, '"a"', "shift from a quoted list");
(let ((a (list "a" 1 5)))
  (shift a))
EOF

lisp_is(<<EOF, '(3 2 6 "a" 1 5)', "unshift into a symbol containing a list #1");
(unshift (setq a (list "a" 1 5)) 3 2 6)
EOF

lisp_is(<<EOF, '(3 2 6 "a" 1 5)', "unshift into a symbol containing a list #2");
(setq a '("a" 1 5)) ;'
(unshift a 3 2 6)
a
EOF

lisp_is(<<EOF, '"a"', "car list");
(car (list "a" 1 5))
EOF

lisp_is(<<EOF, '(1 5)', "cdr list");
(cdr (list "a" 1 5))
EOF
