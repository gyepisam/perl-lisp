package LispTest;

use Exporter 'import';
use Lisp::Interpreter;
use Test::More;

@EXPORT = qw(lisp lisp_err lisp_is);

my $interp = Lisp::Interpreter->new;

if ($ENV{LISPDEBUG}){
  $interp->debug(1);
}

sub lisp(@) { $interp->read_eval_print(@_); }

sub lisp_err($$) {
  my ($lisp, $err) = @_;
  eval { lisp($lisp); };
  is($@ && $@ =~ /^$err/, 1, "normal error: $err");
}

sub lisp_is($$$) {
  my ($lisp, $cond, $description) = @_;
  is(lisp($lisp), $cond, $description);
}

1;

