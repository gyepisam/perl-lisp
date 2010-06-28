package Lisp::List;

use strict;
use Carp;

use vars qw(@EXPORT_OK);
require Exporter;
*import = \&Exporter::import;
@EXPORT_OK = qw(list listp arrayp null);

sub list
{
    Lisp::List->new(@_);
}

sub new
{
    my $class = CORE::shift;
    my $self = [@_];
    bless $self, $class;
    $self;
}

sub print
{
    my $self = CORE::shift;
    require Lisp::Printer;
    Lisp::Printer::lisp_print($self);
}

sub listp {
  return UNIVERSAL::isa $_[0], 'Lisp::List';
}

sub arrayp {
  return UNIVERSAL::isa $_[0], 'ARRAY';
}

sub null {
  my $self = CORE::shift;
  return $self->length() == 0 ? 1 : 0;
}

sub length {
  my $self = CORE::shift;
  return scalar(@$self);
}

sub push {
  my $self = CORE::shift;
  CORE::push @$self, @_;
}

sub pop {
  my $self = CORE::shift;
  CORE::pop @$self;
}

sub shift {
  my $self = CORE::shift;
  CORE::shift @$self;
}

sub unshift {
  my $self = CORE::shift;
  CORE::unshift @$self, @_;
}

sub car {
  my $self = CORE::shift;
  return $self->[0];
}

sub cdr {
  my $self = CORE::shift;
  my @t = @$self;
  return list(@t[1 .. $#t]);
}

sub elt {
  my $self = CORE::shift;
  my $index = CORE::shift;

  if (@_) {

    my $value = CORE::shift;

    if (@_){
      warn "ignoring excess arguments to elt function\n";
    }

    $self->[$index] = $value;
    return $value;
  }
  else {
    my $len = $self->length;
    if ($index  >= 0 and $index < $len) {
      return $self->[$index];
    }
    else {
      die "Attempt to access invalid index [$index] of list with length $len\n";
    }
  }
}

1;
