package Lisp;
use strict;

use vars qw(@EXPORT);
require Exporter;
*import = \&Exporter::import;
@EXPORT = qw(envp symbolp is_symbol);

sub envp {
  return UNIVERSAL::isa($_[0], 'Lisp::Env');
} 

sub symbolp {
    UNIVERSAL::isa($_[0], "Lisp::Symbol");
}

sub is_symbol {
  my ($symbol, $name) = @_;
  return symbolp($symbol) && $symbol->name() eq $name;
}

1;
