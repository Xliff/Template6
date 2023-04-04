unit class Template6::Parser;

use experimental :rakuast;

has @!keywords  = 'eq', 'ne', 'lt', 'gt', 'cmp', 'gte', 'lte';
has @!operators = '==', '!=', '<',  '>' , '<=>', '>=' , '<=';
has $.context;

my regex in-quote   { \" (.*?) \" | \' (.*?) \' }
my token is-numeric { ^ \d+ ['.' \d+]? $        }

sub switch-quotes ($_  is rw) {
  s/^[\"|\']//;
  s/[\"|\']$//;
  $_;
}

method !resolveValue ($_) {
  # cw: Resolve value into RakuAST nodes unless already RakuAST nodes
  when .Num !~~ Failure { RakuAST::NumLiteral.new($_) }
  when .Int !~~ Failure { RakuAST::IntLiteral.new($_) }
  when Str              { RakuAST::StrLiteral.new($_) }
  when RakuAST::Node    { $_ }

}

method !resolveGetValue ($value, :$strict) {
  return self!resolveValue($value) unless $strict;
  (
    self!resolveValue($value),
    RakuAST::ColonPair::True.new('strict')
  )
}

method !resolvePutValues(@values) {
  @values.map({ self!resolveValue($_) });
}

method !doFromStash ($method, $name, *@values, :$strict) {
  RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyPostfix.new(
      operand => RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier('stash')
      ),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier($method),
        args => do given $name {
          when 'get' {
            self!resolveGetValue(@values.head, :$strict)
          }

          when 'put' {
            self!resolvePutValues(@values)
          }
        }
      )
    )
  )
}

multi method append-return ($name, $_) {
  $*return ~= do  {
      when / <in-quote>  / {
          q:to/RAKU/
          %localdata<\qq[$name]> = '\qq«$/<in-quote>[0]»';

          RAKU
      }
      when / <is-numeric> / {
          q:to/RAKU/
          %localdata<\qq[$name]> = '\qq[.Numeric()]';

          RAKU
      }
      default {
          q:to/RAKU/
          %localdata<\qq[$name]> = $stash.get('\qq[ $_ ]');

          RAKU
      }
  }
}
multi method append-return ($name, $_, :$rakuast is required where *.so) {
  nextwith($name, $_) if $*return ~~ Str;
  $*return.push: {
    when / <in-quote> / {
      RakuAST::ApplyInfix.new(
        infix => RakuAST::Infix.new("="),
        left  => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%localdata'),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::StrLiteral.new($name)
              )
            )
          )
        ),
        right => RakuAST::StrLiteral.new( $/<in-quote>[0].Str )
      );
    }

    when / <is-numeric> / {
      RakuAST::ApplyInfix.new(
        infix => RakuAST::Infix.new("="),
        left  => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%localdata'),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::StrLiteral.new($name)
              )
            )
          )
        ),
        right => RakuAST::StrLiteral.new( .Numeric )
      );
    }

    default {
      RakuAST::ApplyInfix.new(
        infix => RakuAST::Infix.new("="),
        left  => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%localdata'),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::StrLiteral.new($name)
              )
            )
          )
        ),
        right => self!doFromStash('get', $_)
      )
    }
  }
}

multi method define-localdata {
  $*return ~= '  my %localdata;' ~ "\n";
}
multi method define-localdata (:$rakuast where *.so) {
  nextwith() if $*return ~~ Str;
  $*return.push: RakuAST::VarDeclaration::Simple.new(
    name => '%localdata'
  );
}

multi method define-tfile ($localline, $template) {
  $*return ~= q:to/RAKU/;
    {
        my $tfile = $stash.get('\qq[$template]');

    RAKU

  $*return ~= $localline;
  $*return ~= q:to/RAKU/;
        with $template { $output ~= $_; }

    RAKU

  $*return ~= q:to/RAKU/;
    }
    RAKU
}
multi method define-tfile($localline, $template, :$rakuast is required where *.so) {
  nextwith($localline, $template) if $*return ~~ Str;
  $*return.push: RakuAST::Block.new(
    body => RakuAST::Blockoid.new(
      RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::VarDeclaration::Simple.new(
            name => '$tfile',
            initializer => self!doFromStash('get', $template)
          )
        ),
        $localline,
        RakuAST::Statement::With.new(
          condition => RakuAST::Var::Lexical.new('$template'),
          block     => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::MetaInfix::Assign.new(
                    RakuAST::ApplyInfix.new(
                      infix => RakuAST::Infix.new("~"),
                      left  => RakuAST::Var::Lexical.new('$output'),
                      right => RakuAST::Var::Lexical.new('$_'),
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

## Incomplete method, supply the $localline which must define a variable called $template
method !parse-template(@defs is copy, $localline, :$rakuast) {
    my $*return = $rakuast ?? [] !! '';

    my $parsing-templates = True;
    my @templates;

    while $parsing-templates && @defs {
        @templates.push: @defs.shift;
        if @defs && @defs[0] eq '+' {
            @defs.shift;
        }
        else {
            $parsing-templates = False;
        }
    }

    unless $*localdata-defined {
      $rakuast ?? self.define-localdata
               !! self.define-localdata(:rakuast);
      $*localdata-defined = True;
    }


    for @defs -> $name, $op, $value {
      $rakuast ?? self.append-return($name, $value)
               !! self.append-return($name, $value, :rakuast);
    }

    for @templates -> $template is rw {
        $template = switch-quotes($template);

        $rakuast ?? self.define-tfile($localline, $template)
                 !! self.define-tfile($localline, $template, :rakuast);
    }

    $*return
}

multi method parse-insert (*@defs, *%named) {
  samewith(@defs, :rakuast) if %named<rakuast>;
  samewith(@defs, :normal);
}
multi method parse-insert(*@defs, :$normal is required) {
    use Test;
    diag "Insert !rakuast";
    my $localline = 'my $template = $context.get-template-text($tfile);' ~ "\n";
    self!parse-template(@defs, $localline)
}
multi method parse-insert(*@defs, :$rakuast is required where *.so) {
  use Test;
  diag "Insert rakuast";
  my $localline = RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      name => '$template',
      initializer => RakuAST::Initializer::Assign.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('$context')
            ),
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier('get-template-text'),
              args => RakuAST::Var::Lexical.new('$tfile')
            )
          )
        )
      )
    )
  );
  self.parse-template(@defs, $localline, :rakuast);
}

multi method parse-include (*@defs, *%named) {
  samewith(@defs, :rakuast) if %named<rakuast>;
  samewith(@defs, :normal);
}
multi method parse-include(@defs, :$normal) {
    my $localline = 'my $template = $context.process($tfile, :localise, |%localdata);' ~ "\n";
    self!parse-template(@defs, $localline)
}
multi method parse-include (@defs, :$rakuast where *.so) {
  my $localline =  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      name => '$template',
      initializer => RakuAST::Initializer::Assign.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('$context')
            ),
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier('process'),
              args => RakuAST::ArgList.new(
                RakuAST::Var::Lexical.new('$tfile'),
                RakuAST::ColonPair::True.new('localize') ,
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::ApplyPrefix.new(
                    prefix => RakuAST::Prefix.new('|'),
                    operand => RakuAST::Var::Lexical.new('%localdata')
                  )
                )
              )
            )
          )
        )
      )
    )
  );

  self!parse-template(@defs, $localline, :rakuast)
}

multi method parse-process (*@defs, *%named) {
  samewith(@defs, :rakuast) if %named<rakuast>;
  samewith(@defs, :normal);
}
multi method parse-process(@defs, :$normal) {
    my $localline = 'my $template = $context.process($tfile, |%localdata);' ~ "\n";
    self!parse-template(@defs, $localline)
}
multi method parse-process (@defs, :$rakuast where *.so) {
  my $localline = RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      name => '$template',
      initializer => RakuAST::Initializer::Assign.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('$context')
            ),
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier('process'),
              args => RakuAST::ArgList.new(
                RakuAST::Var::Lexical.new('$tfile'),
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::ApplyPrefix.new(
                    prefix => RakuAST::Prefix.new('|'),
                    operand => RakuAST::Var::Lexical.new('%localdata')
                  )
                )
              )
            )
          )
        )
      )
    )
  );

  self!parse-template(@defs, $localline, :rakuast)
}


multi method parse-get(Str:D $name) {
    q:to/RAKU/
    $output ~= $stash.get('\qq[$name]');
    RAKU
}
multi method parse-get(Str:D $name, :$rakuast where *.so) {
  @*statements.push: RakuAST::Statement::Expression.new(
    expression => RakuAST::MetaInfix::Assign.new(
      RakuAST::ApplyInfix.new(
        infix => RakuAST::Infix.new("~"),
        left  => RakuAST::Var::Lexical.new('$output'),
        right => self.parse-call($name, :rakuast)
      )
    )
  )
}

multi method parse-call(Str:D $name) {
    q:to/RAKU/
    $stash.get('\qq[$name]');
    RAKU
}
multi method parse-call (Str:D $name, :$rakuast where *.so) {
  # RakuAST::ApplyPostfix.new(
  #   operand => RakuAST::Type::Simple.new(
  #     RakuAST::Name.from-identifier('$stash')
  #   ),
  #   postfix => RakuAST::Call::Method.new(
  #     name => RakuAST::Name.from-identifier('get'),
  #     args => RakuAST::QuotedString.new(
  #       segments   => (
  #         RakuAST::StrLiteral.new($name),
  #       )
  #     )
  #   )
  # );
  self!doFromStash('get', $name);
}

multi method parse-set(:$default, *@values is copy) {
    my $return = '';
    for @values -> $name, $op, $value {
        if $default {
            $return ~= q:to/RAKU/;
            unless $stash.get('\qq[$name]', :strict) {

            RAKU
        }
        $return ~= do given $value {
            when / <in-quote>  / {
                q:to/RAKU/
                $stash.put('\qq[$name]', '\qq«$/<in-quote>[0]»');

                RAKU
            }
            when / <is-numeric> / {
                q:to/RAKU/
                stash.put('\qq[$name]', \qq[$value.Numeric()]);

                RAKU
            }
            default {
                q:to/RAKU/
                $stash.put('\qq[$name]', $stash.get('\qq[$value]'));

                RAKU
            }
        }
        if $default {
            $return ~= q:to/RAKU/;
            }

            RAKU
        }
    }
    $return
}
multi method parse-set(:$default, :$rakuast where *.so, *@values is copy) {
  my @statements;
  for @values -> $name, $op, $value {
    @statements.push: do given $value {
      when / <in-quote>  / {
        self!doFromStash('put', $name, $0.Str);
      }

      when / <is-numeric> / {
        self!doFromStash(
          'put',
          $name,
          RakuAST::Term::TopicCall.new(
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier('Numeric')
            )
          )
        )
      }

      default {
        self!doFromStash('put', $name, self!doFromStash('get', $value) )
      }
    }

    if $default {
      @statements = RakuAST::Statement::Unless(
        condition => self!doFromStash('get', $name, :strict),
        body => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new( |@statements )
          )
        )
      ):$rakuast
    }
  }
}

method parse-default(*@values, :$rakuast) {
  self.parse-set(:default, @values, :$rakuast)
}

my @blocks;
multi method parse-for($left, $op, $right, :$rakuast) {
    my $itemname;
    my $loopname;
    if ($op.lc eq '=' | 'in') {
        $itemname = $right;
        $loopname = $left;
    }
    else {
        $itemname = $left;
        $loopname = $right;
    }
    nextwith($itemname, $loopname, :rakuast) if $rakuast;
    q:to/RAKU/;
    for @($stash.get('\qq[$itemname]', :strict)) -> $\qq[$loopname] {
        $stash.put('\qq[$loopname]', $\qq[$loopname]);
    RAKU
}
multi method parse-for ($itemname, $loopname, :$rakuast where *.so) {
  @blocks.push: ['for', $itemname, $loopname];
}

method get-last-if-block {
  @blocks.first( .head eq 'if' ).grep(* ~~ Pair).head;
}

multi method parse-conditional(Str:D $name, @stmts is copy, :$rakuast) {
    my (@words, $statement);
    for @stmts -> $stmt is rw {
        next if @!keywords.grep($stmt);
        next if $stmt ~~ /^ \d+ $/;
        if $stmt ~~ /^ (\w+) $/ -> $word {
            if $rakuast {
                $statement.push: self!doFromStash('get', $word, :strict)
            } else {
                $stmt .= subst($word, "\$stash.get('$word', :strict)");
            }
        }
    }

    nextwith(@stmts.head, $statement, :rakuast) if $rakuast;
    $statement = @stmts.join(' ');
    "$name $statement \{\n"
}
multi method parse-conditional($node, $statement, :$rakuast where *.so) {
   # my $node = {
   #    when 'if'               { RakuAST::Statement::If     }
   #    when 'unless'           { RakuAST::Statement::Unless }
   #    when 'elsif' | 'elseif' { RakuAST::Statement::Elsif  }
   # }

   given $node {
     when 'elsif' | 'elseif' | 'else' {
       # cw: This should be an error, although I suspect the parser will catch it.
       die 'No proceeding IF!' unless (my $last-if = self.get-last-if-block);
       proceed;
     }

     when 'elsif' | 'elseif' {
       # cw: Need @*statements to be bound to last blocks :$statements so that
       #     the confusing issue of "What block am I now appending to" can be
       #     answered in a proper manner.
       #
       #     Generally, for any block structure, @blocks should consist of
       #     hash references with a :$staments entry. @blocks is then bound to
       #     this array.
       #
       #     In the case of If/Elsif/Else, a block is ended upon encountering
       #     an Elsif or an Else or an End. The statements are then appended to
       #     the proper entry in the previous blocks entry.
       $last-if<elsifs> = [] unless $last-if<elsifs>;
     }
   }

   #     cw: This needs more work. The condition is in $statement which needs
   #     to be parsed further to extract the infix....
   #
   #     For now, we will only support simple statements, so comparisons
   #     where the RHS is a simple literal is the only thing that is in scope.

   my token operator {
     <{ @!operators }> | <{ @!keywords }>
   }

   (
     $statement ~~ /^
       [ $<neg>=['!' \s* ] ]?
       (.+?)
       [ \s* <operator> \s* (.+?) ]?
     $/
   );

   $statement ~~ /^ (.+?) \s* <operator> \s* (.+?) $/;

   my ($prefix, $infix, $rhs);
   my $condition = do {

     when $/<neg> {
        $prefix = '!';
        proceed;
     }

     default {
       $infix = $/<operator>;
       $rhs = $2;
     }
   }

   $last-if{$node}.push: $node ne 'else'
     ?? { :$node, :$not, :$condition, :$infix, :$rhs, statements => [] }
     !! { statements => [] };

   @*statements := $last-if{$node}<statements>;
}


method parse-if (*@stmts, :$rakuast = False) {
    self.parse-conditional('if', @stmts, :$rakuast)
}

method parse-unless(*@stmts, :$rakuast = False) {
    self.parse-conditional('unless', @stmts, :$rakuast)
}

method parse-elsif(*@stmts, :$rakuast = False) {
    my $return;
    $return = "\n}\n" unless $rakuast;

    my $cond = self.parse-conditional('elsif', @stmts, :$rakuast);
    return $return ~ $cond unless $rakuast;
    return $cond;
}
method parse-elseif(*@stmts, :$rakuast = False) {
    self.parse-elsif(|@stmts, :$rakuast)
}

method parse-else( :$rakuast = False ) {
  # cw: Process the previous elsif, if any
  $rakuast ?? self.parse-conditional('else', :$rakuast)
           !! q:b[\n} else {\n];
}

multi method parse-end {
    # cw: Oh. Dear. GOD! The horror...
    "\n}\n"
}
multi method parse-end ( :$rakuast where *.so ) {
  my $block = @blocks.pop;
  @*statements := @block.tail<statements>;

  given $block<node> {

    when 'if' {
      my @elsifs;

      if +$block<elsifs> {
        for $block<elsifs>[] {

          # This may need to be moved to 'parse-else'
          # { :$node, :$not, :$condition, :$infix, :$rhs, :$statements };

          # cw: Only good for logical expressions!
          $block<condition> = RakuAST::ApplyPrefix.new(
            prefix  => RakuAST::Prefix.new('!'),
            operand => $lhs
          ) if $block<not>;

          @elsifs.push: $block<operator>
            ?? RakuAST::Statement::Elsif.new(
                 condition =>  RakuAST::ApplyInfix.new(
                   operator => RakuAST::Infix.new( $operator ),
                   left     => $block<condition>,
                   right    => self!resolveValue($block<rhs>)
                 ),
                 then => RakuAST::Block.new(
                   body => RakuAST::Blockoid.new(
                     RakuAST::StatementList.new( |.<statements> )
                   )
                 )
               )
            !! RakuAST::Statement::Elsif.new(
                 condition => $block<condition>,
                 then => RakuAST::Block.new(
                   body => RakuAST::Blockoid.new(
                     RakuAST::StatementList.new( |.<statements> )
                   )
                 )
               )
        }
      }

      if $block<else> {
        # Look for elsif or if.
        $else = RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new( |$block<else><statements> )
          )
        )
      }

      @*statements.push: RakuAST::Statement::If.new(
        #condition => ...
        then => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              |$block<statements>
            )
          )
        ),
        elsifs => +@elsifs ?? @elsifs !! Nil,
        'else' => $else    ?? $else   !! Nil
      );
    }

    when 'for' {
      #for @($stash.get('\qq[$itemname]', :strict)) -> $\qq[$loopname] {
      #    $stash.put('\qq[$loopname]', $\qq[$loopname]);
      #@*statements.4:

      # cw: Shouldn;t be hard to add NEXT/LAST statements, so we really should
      #     consider adding them.

      @*statements.push: RakuAST::Statement::For.new(
        source => self.doFromStash('get', $itemname, :strict),
        body  => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new("\${ $loopname }")
              )
            )
          )
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(.
              self.doFromStash('put', $loopname, "\${ $loopname }"),
              RakuAST::Label.new( $block<label> ),
              |$block<statements>
            )
          )
        )
      );
    }

    when 'unless' {
      @*statements.push: RakuAST::Statement::Unless.new(
        # condition => ...,
        body => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new( |$block<statements> )
          )
        )
      );
    }
  }
}

method remove-comment(*@tokens --> List) {
    @tokens.toggle(* ne '#').cache
}

method action($statement, :$rakuast = False) {
    my @stmts = $statement
      .lines
      .map({ self.remove-comment(.comb(/ \" .*? \" | \' .*? \' | \S+ /)) })
      .flat;
    return '' unless @stmts;

    my $name = @stmts.shift.lc;
    my $method = 'parse-' ~ $name;
    self.can($method)
      ?? self."$method"(|@stmts, :$rakuast)
      !! @stmts.elems >= 2 && @stmts[0] eq '='
        ?? self.parse-set($name, |@stmts, :$rakuast)
        !! self.parse-get($name, :$rakuast)
}

method get-safe-delimiter($raw-text) {
    say "{ &?ROUTINE.name } - !rakuast";
    my Set() $raw-words = $raw-text.words;
    (1..*).map('END' ~ *).first(* !(elem) $raw-words)
}

method get-segments ($template) {
  $template.split(
    / \n? '[%' $<comment-signature>=('#'?) \s* $<tokens>=(.*?) \s* '%]' /,
    :v
  );
}

method compile($template) {
    my $*localdata-defined = False;
    my $script = q:to/RAKU/;
    return sub ($context) {
        my $stash = $context.stash;
        my $output = '';

    RAKU

    for self!get-segments($template)[] -> $segment {
        if $segment ~~ Stringy
            my $safe-delimiter = self.get-safe-delimiter($segment);
            # Please do not change the string generation logic
            # without paying attention to the implications and
            # changing the test cases appropriately
            my $new-part = q:to/RAKU/;
              $output ~= Q:to/\qq[$safe-delimiter]/.chomp;
              \qq[$segment]
              \qq[$safe-delimiter]
              RAKU
            $script ~= $new-part;
        }
        elsif $segment ~~ Match && !~$segment<comment-signature> {
            my $statement = ~$segment<tokens>;
            $script ~= self.action($statement);
        }
    }

    #say $script;

    $script ~= q:to/RAKU/;
        return $output;
    }
    RAKU

#    $*ERR.say: "<DEBUG:template>\n$script\n</DEBUG:template>";
    # cw: Handled by $*localdata-defined.
    #$script.subst( / 'my %localdata;' /, '', :nd(2..*) ).EVAL
    $script.EVAL;
}
multi method compile($template, :$rakuast is required where *.so) {
  my @script-statements = (
    RakuAST::Statement::Expression.new(
      expression => RakuAST::VarDeclaration::Simple.new(
        name => '$stash',
        initializer => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('$context')
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier('stash')
          )
        )
      )
    ),
    RakuAST::Statement::Expression.new(
      expression => RakuAST::VarDeclaration::Simple.new(
        name => RakuAST::Name.from-identifier('$output')
        initializer => RakAST::StrLiteral('')
      )
    )
  );

  for self!get-segments($template) -> $segment {
    my @list
    if +@blocks {
      @blocks.tail<statements> = [] unless @blocks.tail<statements>;
      @list := @blocks.tail<statements>;
    } else {
      @list := @script-statements;
    }

    if $segment ~~ Stringy {
        @list.push: RakuAST::Statement::Expression.new(
          expression => RakuAST::MetaInfix::Assign.new(
            RakuAST::ApplyInfix.new(
              infix => RakuAST::Infix.new("~"),
              left  => RakuAST::Var::Lexical.new('$output'),
              right => RakuAST::StrLiteral.new($segment)
            )
          )
        );
    } elsif $segment ~~ Match && !~$segment<comment-signature> {
        @list.push: self.action( ~$segment<tokens>, :rakuast );
    }
  }

  RakuAST::Statement::Expression.new(
    expression => RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
            RakuAST::Parameter.new(
              target => RakuAST::ParameterTarget::Var.new('$context')
            ),
          )
        ),
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new( |@script-statements );
        )
      )
    )
  );

}

# vim: expandtab shiftwidth=4
