use strict;
use LispTest;
use Test::More tests => 23;

lisp_is(<<'EOT', '55', 'while loop');
(let ((a 10)
      (sum 0))
(while (not (zerop a))
    (setq sum (+ sum a))
    (setq a (1- a)))
sum)
EOT

lisp_is(<<'EOT', "1", 'dolist on empty constructed list');
(let ((res 1))
(dolist (x (list) res)
 (incf res)))
EOT

lisp_is(<<'EOT',"1",'dolist on empty literal');
(let ((res 1))
(dolist (x () res)
 (incf res)))
EOT


lisp_is(<<EOT, "42", 'loop variable localization');
(setq number-list (list 1 2 3 4 5))
(setq x 42)

(dolist (x number-list)
;     (warn "loop number is" x)
)
x
EOT


lisp_is(<<EOT, "53", 'returnform in initform');

(setq number-list (list 1 2 3 4 5))

(dolist (x number-list 53)
     ;(warn "loop number is" x)
)
EOT


lisp_is(<<EOT, "54", 'explicit return');

(setq number-list (list 1 2 3 4 5))

(dolist (x number-list 53)
     ;(warn "dolist loop number is" x)
     (if (eq x 3)
        (return 54)))
EOT


lisp_is(<<EOT, "55", 'explicit return without a value returns resultform');

(setq number-list (list 1 2 3 4 5))

(dolist (x number-list 55)
     ;(warn "dolist loop number is" x)
     (if (eq x 3)
        (return)))
EOT

lisp_is(<<'EOT', 45, 'dolist on empty constructed list with resultform');
(dolist (x (list) 45)
 ;do nothing
)
EOT

lisp_is(<<'EOT', 45,'dolist on empty literal with resultform');
(dolist (x () 45)
 ;do nothing
)
EOT


lisp_is(<<EOT, 4, 'Loop counter as resultform. Should be n - 1.');
(dotimes (x 5 x)
     ;(warn "dotimes loop number is" x)
)
EOT


lisp_is(<<EOT, "101", 'Negative loop count should not run');
(dotimes (x -1 101)
  ;;This should not execute.
  (return "x")
)
EOT


lisp_is(<<EOT, "56", 'returnform');
(dotimes (x 5 56)
     ;(warn "loop number is" x)
     (if (eq x 3)
        (return)))
EOT


lisp_is(<<EOT, "NIL", 'countform as expression');

(dotimes (x (min 5 8 9 3))
     ;(warn "loop number is" x)
     (return))
EOT


lisp_is(<<EOT, "(99 98)", 'return value as list');

(dotimes (x 2)
     ;(warn "loop number is" x)
     (return (list 99 98)))
EOT


lisp_is(<<EOT, "(99 98)", 'returnform as list');

(dotimes (x 2 (list 99 98))
     ;(warn "loop number is" x)
     (return))
EOT

lisp_is(<<EOT, "NIL", 'returnform on empty list');

(dotimes (x 0 x)
;;
)
EOT


lisp_is(<<EOT, "(1 3 5 7 9)", 'Loop continue');
(let ((odds ()))
  (dotimes (x 10)
     (when (= (% x 2) 0)
        (continue))
     (push odds x))
  odds)
EOT

lisp_is(<<EOT, "NIL", 'empty foreach loop');
(let ((res nil))
 (foreach (k v (hash))
  (setq res v))
 res)
EOT

lisp_is(<<EOT, "1", 'single foreach loop');
(let ((res nil))
 (foreach (k v (hash "one" 1))
  (setq res v))
 res)
EOT

lisp_is(<<EOT, "(1 2 3 4)", 'multi-valued foreach loop');
(let ((res (list)))
 (foreach (k v (hash "one" 1 "two" 2 "three" 3 "four" 4))
  (push res v))
 (sort res))
EOT

lisp_is(<<EOT, "4", 'foreach loop with continue');
(let ((res (list)))
 (foreach (k v (hash "one" 1 "two" 2 "three" 3 "four" 4))
  (unless (= v 4) (continue))
  (setq res v))
 res)
EOT

lisp_is(<<EOT, "4", 'foreach loop with return');
(let ((h (hash "one" 1 "two" 2 "three" 3 "four" 4))
      (res (list)))
 (foreach (k v h)
  (when (= v 4)
    (setq res v)
    (return)))
 res)
EOT

lisp_is(<<EOT, "(1 2 3 4)", 'foreach loop on hash symbol');
(let ((h (hash "one" 1 "two" 2 "three" 3 "four" 4))
      (res (list)))
 (foreach (k v h)
  (push res v))
 (sort res))
EOT


