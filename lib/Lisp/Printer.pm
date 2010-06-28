package Lisp::Printer;

use strict;

use vars qw(@EXPORT_OK $VERSION);
$VERSION = sprintf("%d.%02d", q$Revision: 1.5.2.1 $ =~ /(\d+)\.(\d+)/);
require Exporter;
*import = \&Exporter::import;
@EXPORT_OK = qw(lisp_print);

use Lisp::Symbol qw(symbolp);
use Lisp::Vector qw(vectorp);
use Lisp::Cons   qw(consp);
use Lisp::List   qw(listp);

sub dump
{
    require Data::Dumper;
    Data::Dumper::Dumper($_[0]);
}

sub lisp_print
{
    my $obj = shift;
    my $str = "";
    if (my $ref = ref($obj)) {
        if (symbolp($obj)) {
            $str = $obj->name;
        } elsif (vectorp($obj)) {
            $str = "[" . join(" ", map lisp_print($_), @$obj) . "]";
        } elsif ($ref eq "Lisp::Cons") {
            $str = "(" .join(" . ", map lisp_print($_), @$obj). ")";
        } elsif (listp($obj)) {
            $str = "(" . join(" ", map lisp_print($_), @$obj) . ')';
        } elsif ($ref eq "ARRAY") {
            $str = "(" . join(" ", map lisp_print($_), @$obj) . ")";
        } elsif ($ref eq "HASH") {
            # make it into an alist
            $str = "(" . join("",
                        map {"(" . lisp_print($_) .
                        " . " .
                        lisp_print($obj->{$_}) .
                        ")"
                        } sort keys %$obj) .  ")";
        } elsif ($obj =~ /^(.+)=/){
            $str = "#<object $1>";
        } else {
            $str = "#<$obj>";
        }
    } else {
# XXX: need real number/string type info
        if (!defined($obj)) {
            $str = "nil";
        } elsif ($obj =~ /^[+-]?\d+(?:\.\d*)?$/) {
# number
            $str = $obj + 0;
        } else {
# string
            $obj =~ s/([\"\\])/\\$1/g;  # quote special chars
                $str = qq("$obj");
        }
    }
    $str;
    }

1;
