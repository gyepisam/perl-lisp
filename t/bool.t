use strict;
use LispTest;
use Test::More tests => 33;

=pod

#test case template

lisp_is(<<'EOT', "T", '');
EOT

=cut


cmp_ok(lisp("(= 0 0)"), 'eq', "T", '(= 0 0)');

cmp_ok(lisp("(= 7 7)"), 'eq', "T", '(= 7 7)');

cmp_ok(lisp("(< 3 4)"), 'eq', "T", '"(< 3 4)"');

cmp_ok(lisp("(> 3 4)"), 'eq', "NIL", '"(> 3 4)"');

cmp_ok(lisp("(not (< 3 4))"), 'eq', "NIL", '"(not (< 3 4))"');

cmp_ok(lisp("(and (= 3 3) (/= 3 4))"), 'eq', "T", '"(and (= 3 3) (/= 3 4))"');

cmp_ok(lisp("(not (not (null 333)))"), 'eq', "NIL", '"(not (not (null 333)))"');

cmp_ok(lisp("(not ())"), 'eq', "T", '"(not ())"');

cmp_ok(lisp("(not (list))"), 'eq', "T", '"(not (list))"');

cmp_ok(lisp("(bool 0)"), 'eq', "NIL", '"(bool 0)"');

cmp_ok(lisp("(bool 42)"), 'eq', "T", '"(bool 42)"');

lisp_is(<<'EOT', "T", 'complex boolean');
(setq a 1)
(setq b 2)
(setq c 3)
(and (setq a 4) (setq b nil) (setq c 33))
(and (= a 4) (null b) (= c 3))
EOT


lisp_is(<<'EOT', "T", 'if true');
(if (= 3 3) t nil)
EOT

lisp_is(<<'EOT', "T", 'if false');
(if (= 3 4) nil t)
EOT

lisp_is(<<'EOT', "T", 'cond');
(setq a 1)
(setq b 2)
(setq c 3)

(cond ((setq a 100) (setq b 100))
      ((setq c 100)))

(and (= a 100) (= b 100) (= c 3))
EOT

lisp_is(<<'EOT', "T", 'setq and cond');
(setq res
        (cond
                (nil (setq a 50))
                (t 42)))  ; default
(and (/= a 50) (= res 42))
EOT

lisp_is(<<'EOT', "T", 'or when expression');
(or (when (= 1 1) t) nil)
EOT

lisp_is(<<'EOT', "T", 'or unless expression');
(or (unless (= 1 2) t) nil)
EOT

lisp_is(<<'EOT', "T", 'or setq expression');
(setq a 1)
(setq b 2)
(setq c 3)

(setq c (or (setq a nil) (setq b 0)))
(and (null a) (= b 0) (= c 0))
EOT

lisp_is(<<'EOT', '"ok"', 'empty list as boolean');
(if (list) nil "ok")
EOT

lisp_is(<<'EOT', "T", 'non-empty list as boolean');
(let ((foo (list 1 2 3)))
        (if foo t nil))
EOT


lisp_is(<<'EOT', '"ok"', 't as boolean');
(if t "ok" nil)
EOT


lisp_is(<<'EOT', '"ok"', 'nil as boolean');
(if nil nil "ok")
EOT

lisp_is(<<'EOT', "NIL", 'when, no condition, no result');
(when)
EOT

lisp_is(<<'EOT', "NIL", 'when, true, no result');
(when t)
EOT




lisp_is(<<'EOT', '"ok"', 'when, true');
(when t "ok")
EOT

lisp_is(<<'EOT', "NIL", 'when, false');
(when nil t)
EOT

lisp_is(<<'EOT', "NIL", 'when false expression');
(let ((h (hash)))
  (when (gethash h "foot")
    "not ok"))
EOT

lisp_is(<<'EOT', "NIL", 'unless, true');
(unless t "not ok")
EOT


lisp_is(<<'EOT', '"ok"', 'unless, false');
(unless nil "ok")
EOT

lisp_is(<<'EOT', "T", 'not nil is true');
(not nil)
EOT

lisp_is(<<'EOT', "T", 'variable not nil is true');
(setq foo)
(not foo)
EOT

lisp_is(<<'EOT', "T", 'short circuit evaluation');
(setq foo nil)
(when (or (not foo) (= 1 foo)) t)
EOT



