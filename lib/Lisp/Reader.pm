package Lisp::Reader;

use strict;

use vars qw(@EXPORT_OK $DEBUG $VERSION);
$VERSION = sprintf("%d.%02d", q$Revision: 1.8.2.1 $ =~ /(\d+)\.(\d+)/);
require Exporter;
*import = \&Exporter::import;
@EXPORT_OK = qw(lisp_read);

$DEBUG = 0;

sub lisp_read {
    my $symtab = shift;
    local($_) = shift;
    my $reader_state = shift || { one_form => 0, level => 0, quote_stack => [] };

    my ($one, $level, $quote_stack) = @$reader_state{qw(one_form level quote_stack)};
    my $indent = "  " x $level;

    my $my_symbol = sub { $symtab->symbol($_[0]) };

    my @stack;
    my $form = [];

    my $make_x_form = sub {
        my $handler = shift;
        return sub {
            $handler->(@_);
            if ($one && !@stack){
                no warnings; #about exiting subroutine via last!
                last;
            }
        };
    };

    my $pop_form = $make_x_form->(sub { $form = pop(@stack) });
    my $push_form = $make_x_form->(sub { push(@$form, shift) });

    my $quoted_form = sub {
      my $quote_type = shift;
      push @$quote_stack, $quote_type;

      my $old_pos = pos($_);
      local $reader_state->{level} = $level + 1;
      local $reader_state->{one_form} = 1;

      my($subform, $pos) = lisp_read($symtab, substr($_, $old_pos), $reader_state);
      pos($_) = $old_pos + $pos;
      pop @$quote_stack;
      #needs to be the final statement. It could exit the routine.
      $push_form->([$my_symbol->($quote_type), $subform]);
    };

    my $in_quote = sub {
      my $quote_type = shift;
      for (my $i = $#$quote_stack; $i != -1; $i--) {
        return 1 if $quote_type eq $quote_stack->[$i];
      }
      return 0;
    };


    if ($DEBUG) {
      print "${indent}Parse";
      print "-one" if $one;
      print ": $_\n";
      print("quote stack: ", join(' ', @$quote_stack), "\n") if @$quote_stack;
    }

    while (1) {
      if (/\G\s*;+([^\n]*)/gc) {
        print "${indent}COMMENT $1\n" if $DEBUG;
      } elsif (/\G\s*([()\[\]])/gc) {
        print "${indent}PARA $1\n" if $DEBUG;
        if ($1 eq "(" or $1 eq "[") {
          my $prev = $form;
          push(@stack, $prev);
          push(@$prev, $form = []);
          bless $form, "Lisp::Vector" if $1 eq "[";
        } else {
          die "Excessive parentheses\n" unless @stack;
          $pop_form->();
        }
      } elsif (/\G\s*(
                [-+]?                  # optional sign
                (?:\d+(\.\d*)?         # 0 0. 0.0
                |
                \.\d+)                # .0
                ([eE][-+]?\d+)?        # optional exponent
               )
                (?![^\s()\[\];])              # not followed by plain chars
                /gcx) {
        print "${indent}NUMBER $1\n" if $DEBUG;
        $push_form->($1+0);
      } elsif (/\G\s*\?((?:\\[A-Z]-)*(?:\\\^.|\\[0-7]{1,3}|\\.|.))/sgc) {
        print "${indent}CHAR $1\n" if $DEBUG;
        $push_form->(parse_char($1));
      } elsif (/\G\s*
                \"(                           # start quote
                [^\"\\]*                   # unescaped
                (?:\\.[^\"\\]*)*           # (escaped char + unescaped)*
               )\"/gcxs)                     # end quote
                 {
                   my $str = $1;

                   # Unescape
                   $str =~ s/\\\n//g; # escaped newlines disappear
                   $str =~ s/((?:\\[A-Z]-)+.)/chr(parse_char($1,1))/ge;
                   $str =~ s/((?:\\[A-Z]-)*\\(?:\^.|[0-7]{1,3}|.))/
                     chr(parse_char($1,1))/ge;
                   print "${indent}STRING $str\n" if $DEBUG;
                   $push_form->($str);
                } elsif (/\G\s*\'/gc) {
                   print "${indent}QUOTE\n" if $DEBUG;
                   $quoted_form->('quote');
                 } elsif (/\G\s*\`/gc) {
                   print "${indent}BACKQUOTE\n" if $DEBUG;
                   $quoted_form->('backquote');
                 } elsif (/\G\s*\,\@/gc) {
                   print "${indent}SPLICE\n" if $DEBUG;
                   die "found splice form outside a backquote [$_]\n"
                     unless $in_quote->('backquote');
                   $quoted_form->('splice');
                 } elsif (/\G\s*\,\./gc) {
                   print "${indent}NSPLICE\n" if $DEBUG;
                   die "found nsplice form outside a backquote [$_]\n"
                     unless $in_quote->('backquote');
                   $quoted_form->('nsplice');
                 } elsif (/\G\s*\,/gc) {
                   print "${indent}UNQUOTE\n" if $DEBUG;
                   die "found unquote form outside a backquote [$_]\n"
                     unless $in_quote->('backquote');
                   $quoted_form->('unquote');
                 } elsif (/\G\s*\./gc) {
                   print "${indent}DOT\n" if $DEBUG;
                   #XXX Should handle (a b . c) correctly and (a . b c) as error
                   bless $form, "Lisp::Cons";
                 } elsif (/\G\s*\#'/gc) {
                    $quoted_form->('function');
                 } elsif (/\G\s*\#/gc) {
                   die qq(invalid-read-syntax: "\#");
                 } elsif (/\G\s*
                           (  [^\s()\[\];\\]*          # unescaped plain chars
                           (?:\\.[^\s()\[\];\\]*)*  # (escaped char + unescaped)*
                          )/gcsx
                          && length($1)) {
                   # symbols can have space and parenthesis embedded if they are
                   # escaped.
                   my $sym = $1;
                   $sym =~ s/\\(.)/$1/g; # unescape
                   print "${indent}SYMBOL $sym\n" if $DEBUG;
                   $push_form->($my_symbol->($sym));
                 } elsif (/\G\s*(.)/gc) {
                   print "${indent}? $1\n";
                   die qq(invalid-read-syntax: "$1");
                 } else {
                   last;
                 }
    }

    if (@stack) {
      die "Form terminated early";
      $form = $stack[0];
    }

    if ($one) {
      die "More than one form parsed, this should never happen"
        if @$form > 1;
      $form = $form->[0];
    }

    wantarray ? ($form, pos($_)) : $form;
  }


sub parse_char {
    my($char, $instring) = @_;
    my $ord = 0;
    my @mod;
    while ($char =~ s/^\\([A-Z])-//) {
      push(@mod, $1);
    }

    if (length($char) == 1) {
      $ord = ord($char);        # a plain one
    } elsif ($char =~ /^\\([0-7]+)$/) {
      $ord = oct($1);
    } elsif ($char =~ /^\\\^(.)$/) {
      $ord = ord(uc($1)) - ord("@");
      $ord += 128 if $ord < 0;
    } elsif ($char eq "\\t") {
      $ord = ord("\t");
    } elsif ($char eq "\\n") {
      $ord = ord("\n");
    } elsif ($char eq "\\a") {
      $ord = ord("\a");
    } elsif ($char eq "\\f") {
      $ord = ord("\f");
    } elsif ($char eq "\\r") {
      $ord = ord("\r");
    } elsif ($char eq "\\e") {
      $ord = ord("\e");
    } elsif ($char =~ /^\\(.)$/) {
      $ord = ord($1);
    } else {
      warn "Don't know how to handle character ($char)";
    }

    for (@mod) {
      if ($_ eq "C") {
        $ord = ord(uc(chr($ord))) - ord("@");
        $ord += 128 if $ord < 0;
      } elsif ($_ eq "M") {
        $ord += $instring ? 2**7 : 2**27;
      } elsif ($_ eq "H") {
        $ord += 2**24;
      } elsif ($_ eq "S") {
        $ord += 2**23;
      } elsif ($_ eq "A") {
        $ord += 2**22;
      } else {
        warn "Unknown character modified ($_)";
      }
    }

    $ord;
  }
1;
