use strict;
use LispTest;
use Test::More tests => 1;

#The results of this test should converge with a delta of the expected values
#after a reasonable number of iterations. It could fail occasionally, of course.
lisp_is(<<'EOT', '(T T T)', 'weighted-rand');
(let ((weights (hash "red" 70 "green" 20 "blue" 10))
      (counts (hash "red" 0 "green" 0 "blue" 0))
      (max 5000)
      (res (list)))

  (dotimes (i max)
    (let ((color (weighted-rand weights)))
      (sethash counts color (1+ (gethash counts color)))))

  (dolist (key (hash-keys counts))
    (let ((computed (* (/ (gethash counts key) max) 100))
          (delta 5)
          (expected (gethash weights key)))
      (push res (if (<= (abs (- expected computed)) delta) T NIL))))
  res           
)
EOT

