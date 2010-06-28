use strict;
use LispTest;
use Test::More tests => 5;

lisp_is(<<'EOT', '55', 'while loop');
(let ((a 10)
      (sum 0))
(while (not (zerop a))
    (setq sum (+ sum a))
    (setq a (1- a)))
sum)
EOT

lisp_is(<<'EOT', "T", 'no arg defun');
(defun no-arg ()
 t)

(no-arg)
EOT

lisp_is(<<'EOT', '46', 'one arg defun');
(defun one-arg (arg)
 arg)

(one-arg 46)
EOT


lisp_is(<<'EOT', 7, 'two arg defun');
(defun sum2 (a b)
    (+ a b))

(sum2 3 4)
EOT

lisp_is(<<'EOT', 21,'&rest as list');
(defun sumn (&rest numbers)
  (let ((total 0))
    (dolist (number numbers total)
      (setq total (+ total number)))))

(sumn 1 2 3 4 5 6)
EOT
