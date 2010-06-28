package Lisp::Symbol;
use strict;

require Carp;

use vars qw(@EXPORT_OK $VERSION);
$VERSION = sprintf("%d.%02d", q$Revision: 1.5 $ =~ /(\d+)\.(\d+)/);
require Exporter;
*import = \&Exporter::import;
@EXPORT_OK = qw(symbolp is_symbol);

sub symbolp
{
    UNIVERSAL::isa($_[0], "Lisp::Symbol");
}

sub is_symbol {
  my ($symbol, $name) = @_;
  return symbolp($symbol) && $symbol->name() eq uc $name;
}

sub new
{
    my($class, $name) = @_;
    bless {'name' => uc $name}, $class;
}

sub copy
{
  my $self = shift;
  my $new = {};
  %$new = %$self;
  bless $new, ref($self);
  $new;
}
 
sub name
{
    $_[0]->{'name'};  # readonly
}

sub value {
    my $self = shift;
    if (wantarray && !exists $self->{'value'}) {
        Carp::croak("Symbol's value as variable is void ($self->{'name'})");
    }
    my $old = $self->{'value'};
    $self->{'value'} = shift if @_;
    $old;
}

sub function {
    my $self = shift;
    if (wantarray && !exists $self->{'function'}) {
        Carp::croak("Symbol's value as function is void ($self->{'name'})");
    }
    my $old = $self->{'function'};
    $self->{'function'} = shift if @_;
    $old;
}

sub plist
{
    my $self = shift;
    my $old = $self->{'plist'};
    $self->{'plist'} = shift if @_;
    $old;
}

sub get
{
    my $self = shift;
    $self->{'plist'}{$_[0]};
}

sub put
{
    my $self = shift;
    $self->{'plist'}{$_[0]} = $_[1];
}

sub as_string {
    my $self = shift;
    my @str;
    require Lisp::Printer; #instead of use to avoid recursive dependency

    push(@str, "$self->{'name'}\n");

    if (exists $self->{'value'}) {
        push(@str, "\tvalue: " .
                Lisp::Printer::lisp_print($self->{'value'}) . "\n");
    }
    if (exists $self->{'function'}) {
        push(@str, "\tfunction: " .
                Lisp::Printer::lisp_print($self->{'function'}) . "\n");
    }
    if (exists $self->{'plist'}) {
        push(@str, "\tplist: " .
                Lisp::Printer::lisp_print($self->{'plist'}) . "\n");
    }
    join("", @str);
}

sub dump 
{
    my $self = shift;
    my @str;
    require Lisp::Printer; #instead of use to avoid recursive dependency

    if (exists $self->{value}) {
      push @str, "(setq $self->{'name'}", Lisp::Printer::lisp_print($self->{'value'}) . ")\n";
    };

    if (exists $self->{'function'}) {
       my $func = Lisp::Printer::lisp_print($self->{'function'});
       unless ($func =~ /^#</) {
        push(@str, "(fset (quote $self->{'name'}) (quote", $func .  "))\n");
       }
    }

    if (exists $self->{'plist'}) {
       push @str,
       "(plist $self->{name}", '(hash *', Lisp::Printer::lisp_print($self->{'plist'}) . "*))\n";
    }
    return unless @str;
    join(" ", @str);
}

1;
