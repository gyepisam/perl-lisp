package Lisp::Env;
use strict;
use Lisp::Symbol;
use Carp;

sub new {
  my $self = bless {}, shift;
  return $self;
}

sub has_symbol {
  my ($self, $name) = @_;
  return exists $self->{uc $name};
}

sub symbol {
  my ($self, $name) = @_;

  $name = uc $name;

  if (exists $self->{$name}){
    $self->{$name};
  }
  else {
    $self->{$name} = Lisp::Symbol->new($name);
  }
}

sub install_binding {
    my ($self, $symbol, $value) = @_;
    my $new_sym = $symbol->copy;
    my $name = uc $new_sym->name();
    $new_sym->value($value);
    $self->{$name} = $new_sym;
}


sub install_bindings {
  my $self = shift;
  my $bindings = shift;

  for my $r (@$bindings) {
    $self->install_binding(@$r);
  }
}


sub dump_symbols
{
  my $self = shift;
  print join("", map $self->{$_}->as_string, sort keys %$self);
}

1;
