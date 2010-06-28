package Lisp::Interpreter;

use strict;
use vars qw($VERSION);

=pod

=name Lisp::Interpreter

A perl based interpreter for Lisp programs.

=cut

$VERSION = sprintf("%d.%02d", q$Revision: 1.25.2.2 $ =~ /(\d+)\.(\d+)/);

use Lisp::EnvTable;
use Lisp::Symbol  qw(symbolp is_symbol);
use Lisp::Printer qw(lisp_print);
use Lisp::Special qw(make_special specialp);
use Lisp::Reader qw(lisp_read);
use Lisp::Cons qw(consp);
use Lisp::List qw(list listp arrayp);
use POSIX;

use Data::Dumper;
use Carp;

sub new {
  my $class = shift;
  my $self = bless { __evalno => 0, __depth => 0,
                     __symtab => Lisp::EnvTable->new, @_ }, $class;

  $self->init;
 
  return $self;
}

sub symtab { return $_[0]->{__symtab} }

sub lisp_eval_impl {
  my ($self, $form) = @_;

  my $symtab = $self->symtab;
  my $debug = $self->debug;
  
  local $self->{__evalno} = $self->{__evalno} + 1;
  local $self->{__depth} = $self->{__depth} + 1 ;

  if ($debug) {
    warn " " x $self->{__depth}, $self->{__evalno}, " ", lisp_print($form),"\n";

    if ($debug > 1) {
      warn 'ref:', ref($form), "\n";
      warn 'symbol:', symbolp($form), "\n";
      if ($debug > 2) {
        warn 'dump:', Dumper($form);
      }
    }
  }

  #return scalars
  return get_symbol("nil") if not defined($form); #undef to nil
  return $form unless ref($form); # a string or a number

  #lookup symbol in lexical scope
  if (symbolp($form)){
    return get_symbol($form)->value;
  }

  return $form if listp($form);
  return $form if ref($form) eq 'ARRAY' && @$form == 0; #empty list stays that way

  my $fatal_err = sub {
    my $msg = join(' ', @_);
    if ($msg =~ /\%f/){
       $msg =~ s/(\%f)/lisp_print($form)/e;
    }
    die $msg,"\n";
  };

  my @args = @$form;

  my $func = shift(@args);

  while (symbolp($func)) {
      if (is_symbol($func, "macro")) {
          shift(@args);
          last;
      } elsif (is_symbol($func, "lambda")) {
          last;
      } else {
        my $func_definition = get_symbol($func)->function;
        if (lisp_true($func_definition)){
          $func = $func_definition;
        }
        else {
          $fatal_err->('undefined function: %f');
        }
      }
  }

  unless (specialp($func) || (is_symbol($func, "macro"))) {
    # evaluate all arguments
    for (@args) {
      if (ref($_)) {
        if (symbolp($_)) {
          $_ = get_symbol($_)->value;
        } elsif (ref($_) eq "ARRAY") {
          $_ = lisp_eval($_);
        } else {
          # leave it as it is
        }
      }
    }
  }

  my $res;

  #need this to avoid error with message:
  #Attempt to free temp prematurely: SV 0x8268308.
  #Attempt to free unreferenced scalar: SV 0x8268308.
  #perlbug?

  eval {

    if (UNIVERSAL::isa($func, "CODE")) {
      $res = $func->(@args);
    } elsif (ref($func) eq "ARRAY") {
      if (is_symbol($func->[0], "lambda")) {
        $res = $self->lambda($func, \@args)
      } else {
        $fatal_err->('invalid list function: %f. Must begin with lambda.');
      }
    } else {
        #Can't produce a test case for this. Don't think it's possible.
        $fatal_err->('invalid function: %f. Unknown function type.');
    }
  };

  die $@ if $@;

  if ($debug) {
    warn " " x $self->{__depth}, $self->{__evalno}, " ==> @{[lisp_print($res)]}\n";
  }

  $res;
}

sub lisp_true {
  my $item = shift;
  if (ref($item)) {

    if (listp($item)){
      return not $item->null;
    }
    
    if (symbolp($item)) {
      return 0 if is_symbol($item, 'nil');
      my $v = $item->{value};
      if (ref($v) eq 'ARRAY') {
        return @$v != 0;
      }
      return 1; 
    } else {
      if (ref($item) eq 'ARRAY') {
        return @$item != 0;
      }
      return 1;
    }
  } else {
    return defined($item) ? 1 : 0;
  }
}

# calling a lambda expression
sub lambda {
  my($self, $lambda, $args) = @_;

  my $symtab = $self->symtab;

  # set local variables
  my $localvar = $lambda->[1];

  #convert nil to empty array.
  $localvar = [] unless lisp_true($localvar);

  my $do_opt;
  my $do_rest;
  my $i = 0;

  # symbols in the argument list
  my $opt = get_symbol("&optional");
  my $rest = get_symbol("&rest");

  my $env = $symtab->push_env;

  for my $sym (@$localvar) {
    if ($sym == $opt) {
      $do_opt++;
    } elsif ($sym == $rest) {
      $do_rest++;
    } elsif ($do_rest) {
      $env->install_binding($sym, [@{$args}[$i .. @$args-1]]);
      last;
    } elsif ($i < @$args || $do_opt) {
      $env->install_binding($sym, $args->[$i]);
      $i++;
    } else {
      die "lambda: too-few-arguments", Dumper($lambda, $args);
    }
  }

  if (!$do_rest && @$args > $i) {
    die "lambda: too-many-arguments", Dumper($lambda, $args);
  }

  # execute the function body
  my $res = get_symbol("nil");
  my $pc = 2;                   # starting here (0=lambda, 1=local variables)
  while ($pc < @$lambda) {
    $res = lisp_eval($lambda->[$pc]);
    $pc++;
  }

  $symtab->pop_env;
  $res;
}


sub read_eval_print {
  my $self = shift;

  my $symtab = $self->symtab;

  my $form = Lisp::Reader::lisp_read($symtab, join(" ", @_));
  unshift(@$form, get_symbol("progn"));
  lisp_print(lisp_eval($form));
}

#forward declarations are necessary for the parser.
sub lisp_function($$);
sub lisp_special($$);
sub lisp_eval($);
sub get_symbol($);


#install convenience functions to reduce typing...
sub init_setup_functions {
  my $self = shift;
  my $package = shift;

  
  my $symtab = $self->symtab;

  my $install_func = sub {
      my ($method, $sub) = @_;
      my $fq_method = $package . '::' . $method;
      no strict 'refs';
      *{$fq_method} = $sub;
  };

  $install_func->('lisp_eval', sub($) { $self->lisp_eval_impl($_[0]) });

  #lookup a symbol. dies on failure
  $install_func->('get_symbol', sub($) { $symtab->get_symbol($_[0]) });

  #lookup a symbol. installs symbol, if new.
  $install_func->('symbol', sub($) { $symtab->symbol($_[0]) });

  $install_func->('lisp_function', sub ($$) {
                      my ($name, $func) = @_;
                      $symtab->symbol($name)->function($func);
                  });

  $install_func->('lisp_special', sub ($$) {
                      my ($name, $func)= @_;
                      lisp_function $name, make_special($func);
                  });

}

sub debug { 
  my $self = shift;
  my $debug = $self->{_debug};
  $self->{_debug} = shift if @_;
  return $debug;
}

sub init {
  my $self = shift;

  #define convenience functions
  $self->init_setup_functions(__PACKAGE__);

  my $symtab = $self->symtab;

  #convenience variables
  my $lambda = get_symbol("lambda");
  my $macro = get_symbol("macro");
  my $nil = get_symbol("nil");
  my $t = get_symbol("t");
  
=pod

=head3 debug

Enables system tracing to stderr.

(debug n)

n = 0 turns off tracing.
n between 1 and 3 turn on increasingly more verbose tracing.
n greater than 3 is just like n == 3.

=head3 debug-on

(debug-on)

Synonym for (debug 1)

=head3 debug-off

(debug-off)

Synonym for (debug 0)

=cut

  lisp_function "debug", sub { $self->debug(@_) };
  lisp_function "debug-on", sub { $self->debug(1) };
  lisp_function "debug-off", sub { $self->debug(0) };


=pod

=head3 list

(list ...)

Returns a list containing function arguments, if any.
The return value is actually a perl array reference, but that could
change so don't depend on it.

=head3 car

(car LIST)

Returns first item from LIST.
Returns NIL (undef) if LIST is empty.
It is a fatal error is LIST is not a list.

=head3 cdr

(cdr LIST)

Returns a list of all but the first item in LIST.
Note that returned list is a new list and not a sub list of the original.
The list contents, however, are the same.

=cut

  #FIXME: bless arguments into List class
  lisp_function "list", sub { list(@_) };

  lisp_function "car", sub {
    my $obj = shift;

    if (UNIVERSAL::can($obj, 'car')){
      return $obj->car;
    }
    else {
      die "wrong-argument-type -- cannot get car ", Dumper($obj); 
    }
  };

  lisp_function "cdr", sub {
    my $obj = shift;

    if (UNIVERSAL::can($obj, 'cdr')){
      return $obj->cdr;
    }
    else {
      die "wrong-argument-type. cannot get cdr ", Dumper($obj);
    }
  };

  {

    #create lexical scope for loop operations 

    #use array as a stack to handle nested loops
    my @loop_ctrl;

    #set up loop control block on loop control stack.
    #Every running loop needs a block
    #Current loop block is on top of stack.
    my $enter_loop = sub {
        unshift @loop_ctrl, {

          #flag. turned on with (continue)
          loop_continue => 0,

          #flag. turned on with (return)
          loop_return => 0,

          #what does the loop return?
          loop_value => undef,

          #every loop creates a lexical environment for loop variables
          env => $symtab->push_env
        };
    };

    #tear down loop control block
    my $exit_loop = sub {
        shift @loop_ctrl;
        $symtab->pop_env;
    };

    #Iterates over and evaluates loop body statements.
    my $eval_body = sub {
      my ($init_sub, $list) = (shift, shift);

    MAIN:
      for my $item (@$list) {

        $init_sub->($item);

        for my $statement (@_) {

          lisp_eval($statement);

          if ($loop_ctrl[0]->{loop_continue}) {
            $loop_ctrl[0]->{loop_continue} = 0;
            next MAIN;
          }
          elsif ($loop_ctrl[0]->{loop_return}){
            last MAIN;
          }
        }
      }
    };


    #Generic loop meta meta evaluator for all loops
    my $make_looper = sub {
      my ($list_expander, $loop_body, $loop_tail) = @_;

      return sub {
        my $initform = shift;
      
        $enter_loop->();

        $loop_body->($initform, $list_expander, @_);

        $loop_tail->($initform) if $loop_tail;

        my $loop_value = $loop_ctrl[0]->{loop_value};

        $exit_loop->();

        return $loop_value;
      };
    };


    #Evaluates the body of dotimes and dolist loops
    my $do_body = sub {
      my ($initform, $list_expander) = (shift, shift);

      my $env = $loop_ctrl[0]->{env};      
      my $loopvar = $env->install_binding($initform->[0], $nil);

      $eval_body->(sub {
                     my $cur_value = shift;
                     $loopvar->value($cur_value);
                   },$list_expander->($initform->[1]), @_);
      
    };


    #Evaluates the tail end of dotimes and dolist loops to determine loop value.
    #Necessary for dotimes and dolist resultforms

    my $do_tail = sub {
      my $initform = shift;
      #loop value is either an explicit value
      #set with a return,a  resultform, or nil
      unless (defined($loop_ctrl[0]->{loop_value})) {
        if (defined($initform->[2])) {
          $loop_ctrl[0]->{loop_value} = lisp_eval($initform->[2]);
        } else {
          $loop_ctrl[0]->{loop_value} = $nil;
        }
      }
    };

=pod

=head3 dotimes

(dotimes (INDEX N) ...)

(dotimes (INDEX N RETURNFORM) ...)

Loops over INDEX with successive values between zero (0) and N - 1.

If RETURNFORM is provided, it is evaluated at the end of the loop
iteration, and its value is returned as the loop value.

Note that the loop does not iterate if N is less than or equal to zero.
However, RETURNFORM, if it exists, is always evaluated.

L<return> and L<contine> can be used as necessary within the loop. 

=cut

    
    #dotimes and dolist only differ in how the list is generated...
    lisp_special "dotimes", $make_looper->(sub {
                                           my $countform = shift;
                                           my $max = lisp_eval($countform);
                                           return [] if $max < 1;
                                           return [0 .. $max - 1];
                                         }, $do_body, $do_tail);


=pod

=head3 dolist

(dolist (INDEX LIST) ..)

(dolist (INDEX LIST RETURNFORM) ..)

Loops over INDEX for each element in LIST.

If RETURNFORM is provided, it is evaluated at the end of the loop
iteration, and its value is returned as the loop value.

L<return> and L<contine> can be used as necessary within the loop. 

=cut

    lisp_special "dolist", $make_looper->(sub {
                                          my $listform = shift;
                                          return lisp_eval($listform);
                                        }, $do_body, $do_tail);


=pod

=head3 foreach

(foreach (KEY VALUE (hash "1" "one" "2" "two"))
   ...)

(let ((h (hash "1" "one" "2" "two")))
  (foreach (KEY VALUE h)
    ...))

Iterates over key value pairs of a hash, successively
setting the values of the KEY and VALUE symbols to the
values of the hash keys and values.

L<return> and L<contine> can be used as necessary within the loop. 

=cut

    #Evaluates body of foreach loops

    my $foreach_body = sub {
      my ($initform, $list_expander) = (shift, shift);

      my $env = $loop_ctrl[0]->{env};

      my $h_key = $env->install_binding($initform->[0], $nil);
      my $h_value = $env->install_binding($initform->[1], $nil);
      my $hash = lisp_eval($initform->[2]);

      $eval_body->(sub {
                     my $k = shift;
                     $h_key->value($k);
                     $h_value->value($hash->{$k});
                   }, $list_expander->($hash), @_);

    };

    #foreach generates the list differently and also does not have a result form...
    lisp_special "foreach", $make_looper->(sub {
                                             my $hash = shift;
                                             return [keys %$hash];
                                           }, $foreach_body, undef);


=pod

=head3 return

(return)

(return FORM)

"Breaks" out of a loop.

The second form also allows a return value from the loop.
A return with a value overrides a return form specified in the loop definition.

=cut


    lisp_function "return", sub {
      die "return called outside a loop" unless @loop_ctrl;
      $loop_ctrl[0]->{loop_return} = 1;
      $loop_ctrl[0]->{loop_value} = shift if @_;
      $loop_ctrl[0]->{loop_value};      
    };


=pod

=head3 continue

(continue)

Starts the next iteration of a loop.

=cut

    lisp_function "continue", sub { 
      die "continue called outside a loop" unless @loop_ctrl;
      $loop_ctrl[0]->{loop_continue} = 1; undef;
    }; 

  }


  {

    my $get_list_value = sub {
      my $list = shift;
      my $value = symbolp($list) ? $list->value : $list;
      $value;
    };

    my $set_list_value = sub {
      my ($list, $value) = @_;
      if (symbolp($list)) {
        $list->value($value); 
        return $list;
      }
      $value;
    };

=pod

=head3 pop

(pop LIST)

Removes and returns the last item from a list.

=head3 shift

(shift LIST)

Removes and returns the first item from a list.

=cut


    for my $op (qw(pop shift)) {
      lisp_special $op, sub {
        my $list = shift;
        my $value = $get_list_value->(lisp_eval($list));
        my $head;
        
        if (UNIVERSAL::can($value, $op)) {
          $head = $value->$op();
        }
        else {
          #deprecated
          unless (ref($value) eq 'ARRAY') {
            die "attempt to call op $op on non list", Dumper($list);
          }
          #This bit could be marginally optimized
          if ($op eq 'pop') {
            $head = pop @$value;
          } elsif ($op eq 'shift') {
            $head = shift @$value;
          }
        }

        $set_list_value->($list, $value);
        $head;
      };
    }


=pod

=head3 push

(push LIST item1 item2 item3 ... itemn)

Appends any number of items to a list.

=head3 unshift

(unshift LIST item1 item2 item3 ... itemn)

Prepends any number of items to a list.

=cut

    {

      my $list_adder = sub {
        my $op = shift;
        lisp_special $op, sub {
          my $list = shift;
          my $value = $get_list_value->(lisp_eval($list));
          my @new_items = map { lisp_eval($_) } @_;

          if (UNIVERSAL::can($value, $op)) {
            $value->$op(@new_items);
          }
          else {
            ##deprecate.
            unless (ref($value) eq 'ARRAY') {
              die "attempt to call op $op on non list", Dumper($list);
            }
            eval "$op \@\$value, \@new_items";
          }
          $set_list_value->($list, $value);
        };
      };

      $list_adder->("push");
      $list_adder->("unshift");
    }
  }

=pod

=head3 sort

(sort LIST)

Returns a copy of LIST sorted with perl's cmp based sort routine.

=cut
  
  lisp_function "sort", sub {
    my $list = shift;
    [sort @$list];
  };


=pod

=head3 sort-ascii

(sort-ascii LIST)

Same as sort, but explicitly using the cmp comparison function.

=cut

  lisp_function "sort-ascii", sub {
    my $list = shift;
    [sort { $a cmp $b } @$list];
  };



=pod

=head3 sort-numeric

(sort-numeric LIST)

Returns a copy of LIST, sorted numerically, using the '<=>' comparison function.

=cut

  lisp_function "sort-numeric", sub {
    my $list = shift;
    [sort { $a <=> $b } @$list];
  };
  

=pod

=head3 sort-by-elt-key-numeric

(sort-by-elt-key-numeric LIST KEY)

Returns a copy of LIST, sorted based on the value KEY of each hash element,
compared using the '<=>' function.

=cut

  lisp_function "sort-by-elt-key-numeric", sub {
    my ($list, $key) = @_;
    [sort { $a->{$key} <=> $b->{$key} } @$list];
  };

=pod

=head3 reverse

(reverse LIST)

Returns a reversed copy of LIST

=cut


  lisp_function "reverse", sub {
    my $list = shift;
    [reverse @$list];
  };


=pod

=head3 hash

(hash LIST)

(hash [key value]*)

Constructs and returns a hash, optionally primed
from a list or with any number of key-value pairs.

=cut

  lisp_function "hash", sub {
    
    @_ = @{$_[0]} if @_ == 1 && ref($_[0]) eq 'ARRAY';
    return { @_ };
  };


=pod

=head3 gethash

(gethash HASH KEY)

Returns the value associated with KEY in HASH. NIL (undef) otherwise.

=cut

  lisp_function "gethash", sub {
    my ($hash, $key, $default) = @_;
    return $hash->{$key} if exists $hash->{$key};
    return $default if @_ > 2;
    return $nil;
  };


=pod

=head3 remhash

(remhash HASH KEY)

Removes KEY from HASH

=cut

  lisp_function "remhash", sub {
    my ($hash, $key) = @_;
    delete $hash->{$key} ? $t : $nil;
  };


=pod

=head3 sethash

(sethash HASH KEY VALUE)

Associates KEY with VALUE in HASH

=cut

  lisp_function "sethash", sub {
    my ($hash, $key, $value) = @_;
    $hash->{$key} = $value;
  };


=pod

=head3 inhash

(inhash HASH KEY)

Returns T if KEY exists in HASH. NIL otherwise.

=cut

  lisp_function "inhash", sub {
    my ($hash, $key) = @_;
    return exists($hash->{$key}) ? $t : $nil;
  };


=pod

=head3 hash-keys

(hash-keys HASH)

returns a list of the keys in HASH.

=cut

  lisp_function "hash-keys", sub { return [keys %{$_[0]}] };


=pod

=head3 hash-values

(hash-values HASH)

returns a list of the values in HASH.

=cut

  lisp_function "hash-values", sub { return [values %{$_[0]}] };


=pod

=head3 defmacro

(defmacro MACRO BODY)

Associates MACRO with BODY.

Not fully implemented. Do not use.

=cut

#This really should not be a special routine as such.
#It really needs to be an 'expand-macro' routine that the
#reader can call when it encounters a 'defmacro'

  lisp_special "defmacro", sub {
    my $sym = shift;
    $sym->function([$macro, @_]);
    $sym;
  };


=pod

=head3 quote

(quote ARG)

Returns ARG, unevaluated. Short for 'ARG.

=cut

  lisp_special "quote", sub { $_[0] };


=pod

=head3 set

(set SYMBOL VALUE)

Sets the I<value> of SYMBOL to VALUE.

(let ((a nil)
      (b 'a))
      (set b 3)
      (warn a)) ;;prints 3, not nil

=cut

  lisp_function "set", sub { symbol($_[0])->value($_[1]); $_[1]};


=pod

=head3 setq

(setq SYMBOL VALUE)

Sets SYMBOL to VALUE.

=cut

  lisp_special "setq", sub {
    my $val = lisp_eval($_[1]);
    symbol($_[0])->value($val);
    $val;
  };


=pod

=head3 backquote

(backquote BODY)

Expands backquoted expressions.

Not fully implemented. Do not use.

=cut

  lisp_special "backquote", 
    sub {
      my $ret = [];
      for (@{$_[0]}) {
        if (symbolp($_)) {
          if ($_->{name} eq 'unquote') {
            push @$ret, lisp_eval($_);
            next;
          } elsif ($_->{name} eq 'unquote-splice') {
            my $value = lisp_eval($_);
            #FIXME -- should the result be a list?
            push @$ret, $value;
            next;
          }
        }
        push @$ret, $_;
      }
      $ret;
    };


=pod

=head3 unquote-splice

(unquote-splice)

Not fully implemented. Do not use.

=cut

  lisp_special "unquote-splice", sub { lisp_eval($_[0])};

=pod

=head3 unquote

(unquote)

Not fully implemented. Do not use.

=cut

  lisp_special "unquote", sub { lisp_eval($_[0])};


=pod

=head3 function

(function SYMBOL)

Returns the function associated with SYMBOL, if any.
Shorthand is #': (function +) === #'+

Return the lambda function:
  
  (function '(lambda (x) (...)))

Return built-in 'length' function
 
  (function length)

Result can be I<funcalled>

(funcall (function length) (list 1 2 3))
(funcall #'length (list 1 2 3))

=cut

  lisp_special "function", sub {
    my $function = shift;
    if (symbolp($function)) {
      return $function;
    } elsif (ref($function) eq 'ARRAY') {
      unless (symbolp($function->[0]) && $function->[0]->name() eq 'lambda') {
        die "function: expecting lambda expression argument";
      }
      return $function;
    } else {
      die  "function: called with unknown object", Dumper($_[0]), "\n";
    }
  };


=pod

=head3 apply

(apply FUNC ARG1 ARG2 .. ARGN LIST)

Applies FUNC to all arguments and returns result.
The last argument B<must> be a LIST.

=cut

  lisp_special "apply", sub {
    my ($func, @args) = map { lisp_eval($_) } @_;

    my $e = pop @args;

    unless (arrayp($e)) {
      die "apply: last argument must be a list - ", lisp_print($e), "\n";
    }

    lisp_eval([$func, @args, @$e]);
  };

=pod

=head3 funcall

(funcall FUNC ARG1 ARG2 ARG3)

Applies FUNC to all arguments and returns result.
Last argument does not need to be a list.

=cut

#  lisp_special "funcall", sub { lisp_eval([map { lisp_eval($_) } @_]) };

  my $funcall_imp = sub { 
    my $func = shift;
    lisp_eval([$func, @_]);
  };

  lisp_function "funcall", $funcall_imp;


=pod

=head3 mapcar

(mapcar #'func list1 list2 list3 ...)

Calls FUNC, which must accept as many arguments as there are lists,
with sequential elements of each list as arguments and returns a list
of the results. Processing stops when the shortest input list ends
therefore the length of the result list is the same as that of the
shortest input list.

=cut

  lisp_function "mapcar", sub {
    my $func = shift;
    my @lists = @_;
    my @res;

    my $max_index = $#{$lists[0]};

    for (my $i = 1; $i < @lists; $i++){
      $max_index = $#{$lists[$i]} if $#{$lists[$i]} < $max_index;
    }

    for (my $i = 0; $i <= $max_index; $i++){
      my @args;
      for my $list (@lists) {
        push @args, $list->[$i];
      }
      push @res, $funcall_imp->($func, @args);
    }
    return \@res;
  };



=head3 print

(print ARG)

Prints Lisp representation of ARG 

=cut

  lisp_function "print", sub {lisp_print($_[0])};

=pod

=head3 read

(read ARG)

invoke Lisp Reader on ARG.

=cut

  lisp_function "read", sub {lisp_read($symtab, $_[0])};

=pod

=head3 eval

(eval ARG)

Eval ARG, which is presumably a valid Lisp form.

=cut

  lisp_function "eval", sub {lisp_eval($_[0])};

=pod

=head3 write

(write [ARG]*)

Prints Lisp representation of args to stdout.

=cut

  lisp_function "write", sub {print join(" ", (map lisp_print($_), @_), "\n")};

=pod

=head3 warn

(warn)

Prints Lisp representation of args to stderr.

=cut

  lisp_function "warn", sub {print STDERR join(" ", (map lisp_print($_), @_)), "\n"};

  # control structures

=pod

=head3 progn

(progn)

=cut

  lisp_function "progn", sub {$_[-1]};

=pod

=head3 prog1

(prog1)


=cut

  lisp_function "prog1", sub {$_[0]};

=pod

=head3 prog2

(prog2)


=cut

  lisp_function "prog2", sub {$_[1]};

  {
    my $eval_list = sub {
      my $res = $nil;
      for (@_) {
        $res = lisp_eval($_);
      }      
      return $res;
    };


=pod

=head3 if

(if)
(if CONDITION)
(if CONDITION TRUE-PART)
(if CONDITION TRUE-PART FALSE-PART)

Evaluates CONDITION
ands return TRUE-PART if true,
otherwise returns FALSE-PART or nil.

This short-circuits.

=cut

    lisp_special "if", sub {

      return get_symbol('nil') if (@_ <= 1); #empty test or empty body
      
      if (@_ > 3){
        die "if form has too many parts. At most 3 expected.\n";  
      }

      my $cond = lisp_eval($_[0]);  #if CONDITION
      
      if (lisp_true($cond)) {
        return lisp_eval($_[1]); #then
      }
      elsif (@_ > 2) {
        return lisp_eval($_[2]); #else
      }
    };

    my $make_bool_eval = sub {
      my $expr_eval = shift;
      return sub {
        my $cond = shift;

        $cond = $expr_eval->(lisp_eval($cond));

        if ($cond) {
          return $eval_list->(@_);
        }

        return $nil;
      };
    };



=pod

=head3 when

(when)
(when CONDITION)
(when CONDITION RESULT-FORM)

if condition evaluates to t, evaluates RESULT-FORM

=cut

    lisp_special "when", $make_bool_eval->(\&lisp_true);

=pod

=head3 unless

(unless)
(unless CONDITION)
(unless CONDITION RESULT-FORM)

if condition evaluates to nil, evaluates RESULT-FORM

=cut

    lisp_special "unless", $make_bool_eval->(sub { not lisp_true($_[0]) });
  }


=pod

=head3 cond

(cond)


=cut

  lisp_special "cond", sub {
    my $res;
    my $clause;
    for $clause (@_) {
      $res = lisp_eval($clause->[0]);
      next unless lisp_true($res);
      my $pc;
      for ($pc = 1; $pc < @$clause; $pc++) {
        $res = lisp_eval($clause->[$pc]);
      }
      return $res;
    }
    undef;
  };


  my $lisp_not = sub { lisp_true($_[0]) ? $nil : $t };


=pod

=head3 not

(not)


=cut

  lisp_function "not", $lisp_not;

=pod

=head3 null

(null)


=cut

  lisp_function "null", $lisp_not;

  #same as (not (not x)). This is no longer required, but must be maintained
  #for old programs

=pod

=head3 is

(is)


=cut

  lisp_function "is", sub { lisp_true($_[0]) ? $t : $nil };

  #converts from perl boolean to lisp boolean

=pod

=head3 bool

(bool)


=cut

  lisp_function "bool", sub { $_[0] ? $t : $nil };


=pod

=head3 and

(and)


=cut

  lisp_special "and", sub {
    my $res;
    for (@_) {
      $res = lisp_eval($_);
      return $res unless lisp_true($res);
    }
    $res;
  };


=pod

=head3 or

(or)


=cut

  lisp_special "or", sub {
    my $res = $nil;
    for (@_) {
      $res = lisp_eval($_);
      return $res if lisp_true($res);
    }
    $res;
  };


=pod

=head3 while

(while)


=cut

  lisp_special "while", 
    sub {
      my $condition = shift;
      while (lisp_true(lisp_eval($condition))) {
        # evaluate body
        for (@_) {
          lisp_eval($_);
        }
      }
      undef;
    };

# numeric functions
sub is_float {
  $_[0] =~ /^[-+]?(?:\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?$/;
}

sub is_number {
  my $r = shift;
  return !ref($r) && is_float($r);
}

sub find_non_number {
  for my $r (@_) {
    unless (is_number($r)) {
      return $r;
    }
  }
  return undef;
}

sub die_number_required {
  my ($op, $item) = @_;
  die join(' ', $op, ':', lisp_print($item), "is not a number\n");
}

sub err_unless_number {
  my ($op, $item) = @_;
  return if is_number($item);
  die_number_required($op, $item);
}

sub lisp_numeric_func($$) {
  my ($func, $sub) = @_;

  lisp_function $func, sub {
    if (my $err = find_non_number(@_)) {
      die_number_required($func, $err);
    }
    else {
      $sub->(@_);
    }};
}



  my $floatp = sub { is_float($_[0]) ? $t : $nil };


=pod

=head3 floatp

(floatp SYMBOL)

Returns T if SYMBOL is a floating point number, NIL otherwise.

=cut

  lisp_function "floatp", $floatp;

=pod

=head3 numberp

(numberp SYMBOL)

Returns T if SYMBOL is a number, NIL otherwise.

=cut

  lisp_function "numberp", $floatp;


=pod

=head3 integerp

(integerp)


=cut

  lisp_function "integerp", sub {$_[0] =~ /^\d+$/ ? $t : $nil };

  my $require_num = sub { require_num $_[0] };



=pod

=head3 zerop

(zerop SYMBOL)

Returns T if SYMBOL has a zero value, NIL otherwise. 

=cut

  lisp_function "zerop", sub { 
    return $nil unless $floatp->($_[0]);
    $_[0] == 0 ? $t : $nil;
  };

=pod

=head3 numeric operators

 (=)
 (/=)
 (<)
 (<=)
 (>)
 (>=)
 (1+)
 (+)
 (1-)
 (-)
 (*)
 (/)
 (%)
 (max)
 (min)

=cut

  lisp_numeric_func "=", sub {  $_[0] == $_[1] ? $t : $nil };
  lisp_numeric_func "/=", sub {$_[0] != $_[1] ? $t : $nil };
  lisp_numeric_func "<", sub {$_[0] <  $_[1] ? $t : $nil };
  lisp_numeric_func "<=", sub {$_[0] <= $_[1] ? $t : $nil };
  lisp_numeric_func ">", sub {$_[0] >  $_[1] ? $t : $nil };
  lisp_numeric_func ">=", sub {$_[0] >= $_[1] ? $t : $nil };

  lisp_numeric_func "1+", sub {($_[0] || 0) + 1};

  lisp_numeric_func "+", sub { 

    my $sum = shift;

    for (@_) {
      $sum += $_;
    }
  
    $sum || 0;
  };

  lisp_numeric_func "1-", sub {($_[0] || 0) - 1};

  lisp_numeric_func "-", sub {
    return 0 if $_ == 0;
    return -$_[0] if @_ == 1;
    my $sum = shift; for(@_) {$sum-=$_}
    $sum || 0;
  };

  lisp_numeric_func "*", sub {
    my $prod=1;
    for (@_) {
      $prod *= $_;
    }
    $prod
  };

  lisp_numeric_func "/", sub {
    my $div = shift;

    #invert if single arg
    unless (@_){
      unshift @_,$div;
      $div = 1;
    }

    for (@_) {
      $div /= $_;
    }
    $div;
  };

  lisp_numeric_func "%", sub {  $_[0] % $_[1]};
  lisp_numeric_func "max", sub { my $max=shift;for(@_){$max=$_ if $_ > $max}$max};
  lisp_numeric_func "min", sub { my $min=shift;for(@_){$min=$_ if $_ < $min}$min};


=pod

=head3 incf, decf

(incf SYMBOL)
(incf SYMBOL DELTA)

Increment the value of SYMBOL, which must be numeric, by DELTA and store the results
back in SYMBOL.

DELTA is optional and defaults to 1.

(decf SYMBOL)
(decf SYMBOL DELTA)

Decrement the value of SYMBOL, which must be numeric, by DELTA and store the results
back in SYMBOL.

=cut

  {
    #incf and decf
    my $make_xf = sub {
      my ($op_name, $op) = @_;
      lisp_special $op_name, sub {
        my ($place, $delta) = @_;
        my $value = lisp_eval($place);
        err_unless_number($op_name, $value);
        if (defined $delta) {
          $delta = lisp_eval($delta);
          err_unless_number($op_name, $delta);
        } else {
          $delta = 1;
        }
        $value = $op->($value, $delta);
        get_symbol($place)->value($value);
        $value;
      }};

    $make_xf->('incf', sub { $_[0] + $_[1] });
    $make_xf->('decf', sub { $_[0] - $_[1] });
  }


# functions
=pod

=head3 fset

(fset)


=cut

  lisp_function "fset", sub {symbol($_[0])->function($_[1]); $_[1]};

=pod

=head3 symbol-function

(symbol-function)

=cut

  lisp_function "symbol-function", sub {
    my $v;

    eval {
      if (my $x = get_symbol($_[0])) {
        $v = $x->function();
      }
    };

    return ($@ || !defined($v)) ? $nil : $v;
  };
   

=pod

=head3 symbol

(symbol)


=cut

  #lisp_function "symbol", sub { $make_symbol->($_[0])};


=pod

=head3 defun

(defun)


=cut

  lisp_special "defun",
    sub {
      my $sym = shift;
      $sym->function([$lambda, @_]);
      $sym;
    };


=pod

=head3 put

(put)


=cut

  lisp_special "put", sub { $_[0]->put(lisp_eval($_[1]),
                                       lisp_eval($_[2]))};


=pod

=head3 get

(get SYMBOL NAME)

Returns the contents of the property list item NAME associated with SYMBOL.

=cut

  lisp_special "get", sub { $_[0]->get(lisp_eval($_[1]))};
  

=pod

=head3 plist

(plist SYMBOL HASH)

Sets the property list associated with SYMBOL to HASH.
Note: HASH is not type checked.

=cut

  lisp_special "plist", sub { $_[0]->plist(lisp_eval($_[1]))};


  # dynamic scoping

=pod

=head3 let


(let ((SYMBOL VALUE)*) BODY)

Creates a lexical environment with any number of SYMBOL, VALUE bindings
and evaluates BODY within the environment.

The bindings are performed in parallel, so that later bindings
may not refer to earlier ones.

=cut

  lisp_special "let", sub {
    my $bindings = shift;
    my @bindings = @$bindings;  # make a copy

    # First evaluate all bindings
    for my $b (@bindings) {
      if (symbolp($b)) {
        $b = [$b, $nil];
      } else {
        my($sym, $val) = @$b;
        $b = [$sym, lisp_eval($val)];
      }
    }

    # Then localize
    $symtab->push_env(bindings => \@bindings);

    my $res;
    for (@_) {
      $res = lisp_eval($_);
    }

    $symtab->pop_env;

    $res;
  };



=pod

=head3 let*

(let* ((SYMBOL VALUE)*) BODY)

Creates a lexical environment with any number of SYMBOL, VALUE bindings
and evaluates BODY within the environment.

The bindings are performed sequentially, so that later bindings may refer
to earlier ones.

=cut

  lisp_special "let*", sub {
    my $bindings = shift;
    my $env = $symtab->push_env; 

    # Evaluate and localize in the order given
    for my $b (@$bindings) {
      if (symbolp($b)) {
        $env->install_binding($b, $nil);
      } else {
        my($sym, $val) = @$b;
        $env->install_binding($sym, lisp_eval($val));
      }
    }

    my $res;

    for (@_) {
      $res = lisp_eval($_);
    }

    $symtab->pop_env;

    $res;
  };


=pod

=head3 dump

(dump SYMBOL)

Calls Data::Dumper on Symbol.

(dump SYMBOL NAME)

Calls Data::Dumper on Symbol. Prints NAME as the name of SYMBOL, instead
of default 'VARn' name.

=cut

  lisp_function "dump", sub { 
      local $Data::Dumper::Terse = 1;
      print STDERR @_ == 2 ? Data::Dumper->Dump([$_[0]], [$_[1]]) : Dumper(\@_)
  };


=pod

=head3 dump-symbols

(dump-symbols)

Prints contents of symbol table to STDERR. Useful for debugging.

=cut

  lisp_function "dump-symbols", sub {
    my $symtab = $self->symtab;
    my @d;
    while (my ($k, $v) = each %$symtab) {
      push @d, $v->dump();
    }
    return \@d;
  };


=pod

=head3 eq

(eq STRING1 STRING2)

Returns T if STRING1 is case sensitively equal STRING2. NIL otherwise.

=cut

  lisp_function "eq", sub {$_[0] eq $_[1] ? $t : $nil };

=pod

=head3 ne

(ne STRING1 STRING2)

Returns T if STRING1 is not case sensitively equal STRING2. NIL otherwise.

=cut

  lisp_function "ne", sub {$_[0] ne $_[1] ? $t : $nil };

=pod

=head3 join

(join SEPARATOR ARG1 ARG2 ARG3 ... ARGn)

Joins arguments into string, using SEPARATOR.


=cut

  lisp_function "join", sub {join(shift, @_)};

=pod

=head3 join-list

(join-list SEPARATOR LIST)

Joins contents of LIST into string, using separator.

=cut

  lisp_function "join-list", sub {join($_[0], @{$_[1]})};

=pod

=head3 concat

(concat arg1 arg2 arg3 ... argn)

Concatenates all arguments together. Intended to be used for strings,
but will work for numbers. Other argument types may yield unpredictable
results without error.

Returns an empty string when passed an empty argument list.

=cut

  lisp_function "concat", sub { join('', @_)};

=pod

=head3 length

(length SEQUENCE)

=head3 len

(len SEQUENCE)

Returns the 'length' of SEQUENCE, which could be an array (list), hash, or string.

=cut

  #FIXME -- This should delegate to each symbol type!

{
  my $len_imp =  sub {
    my $seq = shift;

    if (UNIVERSAL::can($seq, 'length')) {
      return $seq->length;
    }

    #deprecate
    my $seq_type = ref($seq);
    if ($seq_type) {
      if ($seq_type eq 'ARRAY') {
        return int(scalar(@$seq))
      } elsif ($seq_type eq 'HASH') {
        return int(scalar(%$seq));
      } else {
        die "cannot determine length of item type: $seq_type for item $seq\n";
      }
    } else {
      return length($seq);
    }
  };

  lisp_function "length", $len_imp;
  lisp_function "len", $len_imp;
}

=pod

=head3 nth-order-permute

(nth-order-permute START POS END)

Returns a list from START to END in which POS is the first item.

START <= POS <= END

=cut

  lisp_function "nth-order-permute", sub {
    my ($start, $pos, $end) = @_;
    my @range = ($pos);
    push @range, ($pos + 1 ..  $end) if ($pos < $end);
    push @range, ($start .. $pos - 1) if ($pos > $start);
    return \@range;
  };


=pod

=head3 range

(range COUNT STEP)
(range COUNT)

Returns a list of COUNT integers from 0 to (COUNT * STEP) in steps of STEP.

COUNT must be greater than or equal to zero.
STEP can be any integer value and defaults to 1 if not specified.

=cut

my $range_imp = sub {
    my ($count, $start, $step) = @_;
    $step = 1 unless defined $step;
    my @range;
    for (my ($i, $c) = (0,$start); $i < $count; $i++, $c += $step) {
        push @range, $c;
    }
    \@range;
};


lisp_function "range", sub {
    my ($count, $step) = @_;

    die "range: positive count argument required" if $count < 0;

    return list() if $count == 0;

    $step = 1 unless defined $step;

    my $end = do {
      my $c = $count - 1;
      $step == 0 ? $c : $c * $step;
    };

    return $range_imp->($count, 0, $step);
};


=pod

=head3 xrange

(xrange START END STEP)
(xrange START END)

Returns a list of (END - START + 1) / STEP integers from START to END in steps of STEP

STEP defaults to 1 if not specified.

If START == END, STEP is ignored and a list containing the single value is returned.

=cut

lisp_function "xrange", sub {

  my ($start, $end, $step) = @_;

  return list($start) if $start == $end;

  die "xrange: step cannot be zero for unequal start and end values" if $step == 0;

  $step = 1 unless defined $step;

  my $count = (abs($end - $start) + 1) / abs($step);

  return $range_imp->($count, $start, $step);
};

=pod

=head3 elt

(elt SEQ INDEX)

Returns element at INDEX in SEQ.

(elt SEQ INDEX VALUE)

Stores VALUE at INDEX in SEQ. Returns VALUE.

INDEX is a numeric index when SEQ is an array.
INDEX is a key when SEQ is a hash.

=cut

lisp_function "elt", sub {
  my $seq = shift;
  my $index = shift;

  if (UNIVERSAL::can($seq, 'elt')) {
    return $seq->elt($index, @_);
  }

  my $set_value = scalar(@_);
  my $value = shift;

  if ($set_value > 1) {
    warn "ignoring excess arguments to elt function\n";
  }

  my $seq_type = ref($seq);

  if ($seq_type) {
    if ($seq_type eq 'ARRAY') {
      if ($set_value) {
        $seq->[$index] = $value;
        return $value;
      }
      elsif ($index  >= 0 and $index < @$seq) {
        return $seq->[$index];
      } else {
        die "Attempt to access invalid array index [$index] of array @$seq\n";
      }
    } elsif ($seq_type eq 'HASH') {
      if ($set_value) {
        $seq->{$index} = $value;
        return $value;
      }
      else {
        return $seq->{$index};  #index should really be called key here
      }
    } else {
      die "function elt called for non-sequence item $seq\n";
    }
  }
};



=pod

=head3 sprintf

(sprintf FORMAT ARG1 ARG 2 ...)

Returns a string formatted according to FORMAT, using ARGS1 to ARGSn.

See perl documentation for details.

=cut

lisp_function "sprintf", sub {
    my $str = shift;
    sprintf $str, @_;
};

=pod

=head3 substr

(substr EXPR OFFSET LENGTH REPLACEMENT)
(substr EXPR OFFSET LENGTH)
(substr EXPR OFFSET)

Returns or replaces substring in a string.

See perl documentation for details.

=cut

lisp_function "substr", sub {
    my ($expr, $offset, $length, $replacement) = @_;
    if (@_ == 4) {
        substr $expr, $offset, $length, $replacement;
    }
    elsif (@_ == 3) {
        substr $expr, $offset, $length;
    }
    elsif (@_ == 2) {
        substr $expr, $offset;
    }
    else {
        die "function substr requires at least 2 arguments\n";
    }
};


=pod

Zero argument perl functions. See standard perl documentation.

=over

=item time

=item times

=item getlogin

=item getppid

=item fork

=item wait

=back

=cut


  # Make many perl functions available in the lisp envirionment
  # Perl builtins that take zero arguments
  for (qw(time times getlogin getppid fork wait)) {
    lisp_function $_, eval qq[sub { $_ }];
  }


=pod

perl functions that accept optional single argument.

The 'list-' prefixed versions are called in a list context since
some of the functions behave differently when called in a list context.

Note that the list versions of functions whose names begin with a dash '-'
have a single dash as well. So '-x' becomes 'list-x' and not 'list--x';

See standard perl documentation for more information.

=head3 sin, list-sin

(sin)
(list-sin)

=head3 cos, list-cos

(cos)
(list-cos)

=head3 rand, list-rand

(rand)
(list-rand)

=head3 srand, list-srand

(srand)
(list-srand)

=head3 exp, list-exp

(exp)
(list-exp)

=head3 log, list-log

(log)
(list-log)

=head3 sqrt, list-sqrt

(sqrt)
(list-sqrt)

=head3 int, list-int

(int)
(list-int)

=head3 hex, list-hex

(hex)
(list-hex)

=head3 oct, list-oct

(oct)
(list-oct)

=head3 abs, list-abs

(abs)
(list-abs)

=head3 ord, list-ord

(ord)
(list-ord)

=head3 chr, list-chr

(chr)
(list-chr)

=head3 ucfirst, list-ucfirst

(ucfirst)
(list-ucfirst)

=head3 lcfirst, list-lcfirst

(lcfirst)
(list-lcfirst)

=head3 uc, list-uc

(uc)
(list-uc)

=head3 lc, list-lc

(lc)
(list-lc)

=head3 quotemeta, list-quotemeta

(quotemeta)
(list-quotemeta)

=head3 caller, list-caller

(caller)
(list-caller)

=head3 reset, list-reset

(reset)
(list-reset)

=head3 exit, list-exit

(exit)
(list-exit)

=head3 umask, list-umask

(umask)
(list-umask)

=head3 chdir, list-chdir

(chdir)
(list-chdir)

=head3 chroot, list-chroot

(chroot)
(list-chroot)

=head3 readlink, list-readlink

(readlink)
(list-readlink)

=head3 rmdir, list-rmdir

(rmdir)
(list-rmdir)

=head3 getpgrp, list-getpgrp

(getpgrp)
(list-getpgrp)

=head3 localtime, list-localtime

(localtime)
(list-localtime)

=head3 gmtime, list-gmtime

(gmtime)
(list-gmtime)

=head3 alarm, list-alarm

(alarm)
(list-alarm)

=head3 sleep, list-sleep

(sleep)
(list-sleep)

=head3 require, list-require

(require)
(list-require)

=head3 stat, list-stat

(stat)
(list-stat)

=head3 chop, list-chop

(chop)
(list-chop)

=head3 chomp, list-chomp

(chomp)
(list-chomp)

=head3 defined, list-defined

(defined)
(list-defined)

=head3 undef, list-undef

(undef)
(list-undef)

=head3 study, list-study

(study)
(list-study)

=head3 pos, list-pos

(pos)
(list-pos)

=head3 -r, list-r

(-r)
(list-r)

=head3 -w, list-w

(-w)
(list-w)

=head3 -x, list-x

(-x)
(list-x)

=head3 -o, list-o

(-o)
(list-o)

=head3 -R, list-R

(-R)
(list-R)

=head3 -W, list-W

(-W)
(list-W)

=head3 -X, list-X

(-X)
(list-X)

=head3 -O, list-O

(-O)
(list-O)

=head3 -e, list-e

(-e)
(list-e)

=head3 -z, list-z

(-z)
(list-z)

=head3 -s, list-s

(-s)
(list-s)

=head3 -f, list-f

(-f)
(list-f)

=head3 -d, list-d

(-d)
(list-d)

=head3 -l, list-l

(-l)
(list-l)

=head3 -p, list-p

(-p)
(list-p)

=head3 -S, list-S

(-S)
(list-S)

=head3 -b, list-b

(-b)
(list-b)

=head3 -c, list-c

(-c)
(list-c)

=head3 -t, list-t

(-t)
(list-t)

=head3 -u, list-u

(-u)
(list-u)

=head3 -g, list-g

(-g)
(list-g)

=head3 -k, list-k

(-k)
(list-k)

=head3 -u, list-u

(-u)
(list-u)

=head3 -g, list-g

(-g)
(list-g)

=head3 -k, list-k

(-k)
(list-k)

=head3 -T, list-T

(-T)
(list-T)

=head3 -B, list-B

(-B)
(list-B)

=head3 -M, list-M

(-M)
(list-M)

=head3 -A, list-A

(-A)
(list-A)

=head3 -C, list-C

(-C)
(list-C)


=cut

  # Perl builtins that take one optional argument
  for (qw(sin cos rand srand exp log sqrt int hex oct abs ord chr
          ucfirst lcfirst uc lc quotemeta caller reset exit
          umask chdir chroot readlink rmdir getpgrp
          localtime gmtime alarm sleep
          require stat chop chomp defined undef study pos
          -r -w -x -o -R -W -X -O -e -z -s -f -d -l -p -S -b -c
          -t -u -g -k -u -g -k -T -B -M -A -C
         )) {

    lisp_function $_, eval qq[sub { \@_==0?$_:$_ \$_[0] }];
    
    #prefix function name with 'list-' and provide list context.
    #not all functions behave differently in a list context, but all should behave correctly.
    (my $f = "list-$_") =~ s/--/-/;
    lisp_function "$f", eval qq[sub { my \@res = \@_ == 0 ? $_ : $_ \$_[0]; \\\@res; }];
  }


=pod

=head3 ceil

(ceil)

=head3 floor

(floor)

=cut

  #export some one argument posix functions
  for my $func (qw(ceil floor)) {
    lisp_function $func, eval qq/sub { POSIX::$func \$_[0] }/;
  }


=pod

=head3 posix-strftime

(posix-strftime)


=cut

  lisp_function "posix-strftime", sub { POSIX::strftime $_[0], $_[1] ? @{$_[1]} : localtime; };

  # some additional stuff

=pod

=head3 perl-eval

(perl-eval STRING)

Evals perl program.

=cut

  lisp_function "perl-eval", sub { eval $_[0] };

=pod

=head3 perl-use

(perl-use MODULE [(args)+]*)

'uses' perl MODULE, passing any optional arguments in a quoted list.
Returns T on success. On failure, returns NIL and prints an error message to stderr.

=cut

  lisp_function "perl-use", sub {
    my @str = ('use', shift);
    push (@str, 'qw/', @_, '/') if @_;
    my $str = join ' ', @str; 
    eval $str;
    if ($@) {
      warn "perl-use : error with command [$str]. $@\n";
      return $nil;
    }
    return $t;
  };


=pod

=head3 perl-method

(perl-method OBJECT METHOD [ARGS]*)

invokes OBJECT->METHOD([args]) and returns the results.

Note that OBJECT can be an object or a module name.
The following snippet of code uses the same function
to create an instance of class FOO  and invoke methods on the
newly created object.

(when (perl-use "FOO")
  (let ((object (perl-method "FOO" "new")))
    (when object
      (perl-method object some-method args))))

=cut


  lisp_function "perl-method", sub {
    my ($obj, $method, @args) = @_;
    $obj->$method(@args);
  };


=pod

=head3 weighted-rand

(weighted-rand WEIGHT-HASH)

Returns a key from WEIGHT-HASH based on the weight values
(weighted-rand (hash "red" 35 "green" 33  "blue" 32))

With return red 35% of the time, green 33% of the time and blue 32% of the time.

Algorithm copied from the Perl Cookbook by Christiansen and Torkington.

=cut

 
  lisp_function "weighted-rand", sub {
    my $weights = shift;
    
    return unless %$weights;
    
    my $total = 0;
    
    for (values %$weights) {
      $total += $_;
    }

    unless ($total) {
      die "weighted-rand: weights sum to zero!";
    }

    my %dist;
    
    while (my ($k, $v) = each %$weights) {
     $dist{$k} = $v / $total; 
    }
    
    my ($key, $weight);

    while(1) {
      my $rand = rand;
      while (($key, $weight) = each %dist) {
        return $key if ($rand -= $weight) < 0;
      }
    }
  };

  
  #new functions above this line!
}


1;
