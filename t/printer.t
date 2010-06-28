use strict;
use LispTest;
use Test::More tests => 7;

lisp_is(33, "33",'integer');

lisp_is(33.3, "33.3",'float');

lisp_is('"foo"', '"foo"','string');

lisp_is('"foo\\\\bar\"baz"', '"foo\\\\bar\"baz"','string with embedded quote');

lisp_is('', "nil",'empty script as nil');

lisp_is(undef, "nil",'undef as nil');

lisp_is(q/'[3 4 [5 6]]/, "[3 4 [5 6]]",'vector');
