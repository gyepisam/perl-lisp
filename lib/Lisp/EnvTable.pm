package Lisp::EnvTable;

use strict;

use Lisp;
use Lisp::Env;
use Carp;
use Data::Dumper;

sub new {
  my $self = shift;
  $self = bless {}, ref($self) || $self;

  #create the top-level environment
  $self->push_env();

  #create special, self-evaluating symbols

  for my $name (qw(t nil)) {
    my $symbol = $self->symbol($name);
    $symbol->value($symbol);
  }

  #symbols with no particular value...
  for my $name (qw(lambda macro &optional &rest)) {
    $self->symbol($name);
  }

  return $self; 
}


sub push_env {
  my $self = shift;
  my %args = @_;
 
  my $env = $args{env} || Lisp::Env->new();

  #current env is always at $self->{env}->[0]
  #and we don't have to reverse the list when iterating.
  unshift @{$self->{env}}, $env;

  if (my $bindings = $args{bindings}){
    $env->install_bindings($bindings);
  }

  $env;
}



sub pop_env {
  my $self = shift;

  my $depth = $self->stack_size;
  
  if ($depth == 0){
      Carp::confess "attempt to underflow environment stack";
  }
  elsif ($depth == 1) {
      Carp::confess "attempt to removed top-level environment from stack";
  }

  shift @{$self->{env}};
}


sub get_env {
  my ($self, $index) = @_;

  $index = 0 unless defined $index;

  if ($index !~ /^\d+$/){
      Carp::confess "integer index required";
  }
  elsif ($index < 0 || $index > $self->stack_size) {
      Carp::confess "environment index out of range";
  }

  return $self->{env}->[$index];
}


sub stack_size {
    my $self = shift;
    scalar @{$self->{env}}
}


=pod

if ref is symbol, return its upper cased name.
if ref is name, upper case and return it.

=cut

sub resolve_symbol_ref {
  my ($self, $ref) = @_;

  my $name;

  #$ref must be a symbol or a string.
  if (ref($ref)){
      if (symbolp($ref)) {
          $name = $ref->name;
      }
      else {
          Carp::croak
             "cannot resolve symbol reference for non-symbol object",
             Dumper($ref);
      }
  }
  else {
      $name = $ref;
  }
 
  return uc $name;
}


sub find_symbol_env {
  my ($self, $ref) = @_;

  my $name = $self->resolve_symbol_ref($ref);

  #Look through all the extant environments...
  for (my $i = 0; $i < @{$self->{env}}; $i++ ) {    
    my $r = $self->{env}->[$i];
    if ($r->has_symbol($name)) {
      return ($r, 1) if wantarray;
      return $r;
    }
  }
  
  return ($self->{env}->[0], 0) if wantarray;
  return undef;
}

sub find_symbol {

  my ($self, $ref) = @_;

  my $name = $self->resolve_symbol_ref($ref);
  my $env = $self->find_symbol_env($name);

  if ($env){
    return $env->symbol($name);
  }

  return undef;
}

#lookup a symbol. croaks on failure!
sub get_symbol {
  my ($self, $ref) = @_;

  my $name = $self->resolve_symbol_ref($ref);

  if (my $symbol = $self->find_symbol($name)){
    return $symbol;
  }
  else {
    Carp::croak "cannot get symbol with name [$name]\n";
  }
}

#lookup a symbol. install symbol on failure.
sub symbol {
  my ($self, $ref) = @_;

  my $name = $self->resolve_symbol_ref($ref);
  #create a list context to get default top level env if necessary.
  my ($env, undef) = $self->find_symbol_env($name);
  return $env->symbol($name);
}

1;
