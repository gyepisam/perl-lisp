use strict;
use LispTest;
use Test::More tests => 11;

lisp_is(<<EOF, '()', 'empty list, ascii sort');
(sort (list))
EOF

lisp_is(<<EOF, '(9)', 'single item list, ascii sort');
(sort (list 9))
EOF

lisp_is(<<EOF, '(7 9)', 'single digit two item list, ascii sort');
(sort (list 9 7))
EOF

lisp_is(<<EOF, '(199 57)', 'double digit two item list, ascii sort');
(sort (list 57 199))
EOF


lisp_is(<<EOF, '(1 7 9)', 'three item list, ascii sort');
(sort (list 1 9 7))
EOF

lisp_is(<<EOF, '(199 2 57)', 'double digit three item list, ascii sort');
(sort (list 2 199 57))
EOF


#two item list is sorted 
lisp_is(<<EOF, '(7 9)', 'single digit, two item list, numeric sort');
(sort-numeric (list 9 7))
EOF

lisp_is(<<EOF, '(1 7 9)', 'single digit, three item list, numeric sort');
(sort-numeric (list 1 9 7))
EOF

lisp_is(<<EOF, '("abel" "baker" "charlie")', 'string, implicit ascii sort');
(sort '("abel" "charlie" "baker")) ;'help editor syntax colorizer
EOF

lisp_is(<<EOF, '("abel" "baker" "charlie")', 'string, explicit ascii sort');
(sort-ascii '("abel" "charlie" "baker")) ;'help editor syntax colorizer 
EOF

lisp_is(<<EOF, '("abel" "baker" "charlie")', 'sort-by-elt-key-numeric');
(let ((names (sort-by-elt-key-numeric (list
                                    (hash "name" "charlie" "number" 3)
                                    (hash "name" "abel" "number" 1 )
                                    (hash "name" "baker" "number" 2))
                                 "number"))
      (output (list)))
      (dolist (list-elt names)
       (push output (gethash list-elt "name")))
      output)
      

EOF
