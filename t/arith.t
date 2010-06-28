use strict;
use LispTest;
use Test::More tests => 28;

cmp_ok(lisp("'33"), '==', "33",'');

cmp_ok(lisp("(+ 1)"), '==', lisp("(1+)"),'');

cmp_ok(lisp("(+ 1 1)"), '==', lisp("(* 2 1)"),'');

cmp_ok(lisp("(+ 3 3)"), '==', lisp("(* 2 3)"),'');

cmp_ok(lisp("(+ 1 2 3 4)"), 'eq', "10",'');

cmp_ok(lisp("(+ 1 (+ 2 (+ 3 (+ 4))))"), 'eq', "10",'');

cmp_ok(lisp("(1+ 1)"), '==', "2",'');

cmp_ok(lisp("(- 1)"), '==', -1,'');

cmp_ok(lisp("(- -1)"), '==', 1,'');

cmp_ok(lisp("(- 10 5)"), '==', 5,'');

cmp_ok(lisp("(- 10 4 3 2 1)"), '==', 0,'');

cmp_ok(lisp("(1- 10)"), '==', 9,'');

cmp_ok(lisp("(* 3)"), '==', 3,'');

cmp_ok(lisp("(* 3 3)"), '==', 9,'');

cmp_ok(lisp("(* 3 3 3)"), '==', 27,'');

cmp_ok(lisp("(/ 9 3)"), '==', 3,'');

cmp_ok(lisp("(% 9 3)"), '==', 0,'');

cmp_ok(lisp("(max 1 2 3)"), '==', 3,'');

cmp_ok(lisp("(max 2 3 1)"), '==', 3,'');

cmp_ok(lisp("(max 3 1 2)"), '==', 3,'');

cmp_ok(lisp("(min -10 -90 100)"), '==', -90,'');

cmp_ok(lisp("(+ (* (max 3 4 1) (- 10 (1+ 9))) (+ 3 4))"), '==', 7,'');

cmp_ok(lisp("(list (floatp 33.33) (floatp t))"), 'eq', "(T NIL)",'');

cmp_ok(lisp("(list (integerp 42) (integerp 33.3))"), 'eq', "(T NIL)",'');

cmp_ok(lisp("(list (numberp 33.33) (numberp t))"), 'eq', "(T NIL)",'');

cmp_ok(lisp("(list (zerop 0) (zerop 33))"), 'eq', "(T NIL)",'');

cmp_ok(abs(lisp("(cos (sin 10))") - cos(sin(10))), '<', 0.0000001, 'sin & cos');

my $badrand=0;
my $randsum=0;

for (1 .. 100) {
    my $r = lisp("(int (rand 10))");
    $randsum += $r;
    $badrand++ if $r < 0 || $r > 10;
}

$badrand++ if $randsum < 300 || $randsum > 700; # real bad luck if this happens
cmp_ok($badrand, '==', 0, 'random number generation');
