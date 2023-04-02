unit class Template6::Parser;

has @!keywords = 'eq', 'ne', 'lt', 'gt', 'gte', 'lte';
has $.context;

my token in-quote   { \" (.*?) \" | \' (.*?) \' }
my token is-numeric { ^ \d+ ['.' \d+]? $        }

sub switch-quotes ( $_ ) {
  s/^[\"|\']//;
  s/[\"|\']$//;
  $_;
}

method !append-return ($name, $_) {
  $*return ~= do  {
      when / <in-quote>  / {
          q:to/RAKU/
          %localdata<\qq[$name]> = '\qq[$0]';

          RAKU
      }
      when / <is-numeric> / {
          q:to/RAKU/
          %localdata<\qq[$name]> = '\qq[ .Numeric() ]';

          RAKU
      }
      default {
          q:to/RAKU/
          %localdata<\qq[$name]> = $stash.get('\qq[ $_ ]');

          RAKU
      }
  }
}

method !append-return ($name, $_, :$rakuast where *.so) {
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
        right => RakuAST::StrLiteral.new($0.Str)
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
        right => RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('stash')
            ),
            postfix => RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier('get'),
              args => RakuAST::StrLiteral.new($_)
            )
          )
        )
      );
    }
  }
}

multi method !define-localdata {
  $*return ~= q:to/RAKU/;
    my %localdata;
    RAKU
}
multi method !define-localdata (:$rakuast is required) {
  $*return.push: RakuAST::VarDeclaration::Simple.new(
    name => '%localdata'
  );
}

multi method !define-tfile($localline, $template) {
  $*return ~= q:to/RAKU/;
    {
        my $tfile = $stash.get('\qq[$template]');

    RAKU

    $return ~= $localline;
    $return ~= q:to/RAKU/;
        with $template { $output ~= $_; }

    RAKU

    $return ~= q:to/RAKU/;
    }
    RAKU
}
multi method !define-tfile($localline, $template, :$rakuast where *.so) {
  $*return.push: RakuAST::Block.new(
    body => RakuAST::Blockoid.new(
      RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::VarDeclaration::Simple.new(
            name => '$tfile',
            initializer => RakuAST::Initializer::Assign.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                  operand => RakuAST::Type::Simple.new(
                    RakuAST::Name.from-identifier('$stash')
                  ),
                  postfix => RakuAST::Call::Method.new(
                    name => RakuAST::Name.from-identifier('get'),
                    args => RakuAST::StrLiteral.new($template)
                  )
                )
              )
            )
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

    $rakuast ?? self!define-localdata
             !! self!define-localdata(:rakuast);


    for @defs -> $name, $op, $value {
      $rakuast ?? self!append-return($name, $value)
               !! self!append-return($name, $value, :rakuast);
    }

    for @templates -> $template is rw {
        $template = switch-quotes($template);

        $rakuast ?? self!define-tfile($localline, $template)
                 !! self!define-tfile($localline, $template, :rakuast);
    }
    $*return
}

multi method parse-insert(@defs) {
    my $localline = 'my $template = $context.get-template-text($tfile);' ~ "\n";
    self!parse-template(@defs, $localline)
}
multi method parse-insert(@defs, :$rakuast where *.so) {
  my $localline: RakuAST::Statement::Expression.new(
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
  self!parse-template(@defs, $localline, :rakuast);
}

multi method parse-include (*@defs, *%named) {
  samewith(@defs, :rakuast) if %named<rakuast>;
  samewith(@defs, :normal);
}
multi method parse-include(@defs, :$normal) {
    my $localline = 'my $template = $context.process($tfile, :localise, |%localdata);' ~ "\n";
    self!parse-template(@defs, $localline)
}1
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
  @ast.push: RakuAST::Statement::Expression.new(
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
  RakuAST::ApplyPostfix.new(
    operand => RakuAST::Type::Simple.new(
      RakuAST::Name.from-identifier('$stash')
    ),
    postfix => RakuAST::Call::Method.new(
      name => RakuAST::Name.from-identifier('get'),
      args => RakuAST::QuotedString.new(
        segments   => (
          RakuAST::StrLiteral.new($name),
        )
      )
    )
  );
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
                $stash.put('\qq[$name]', '\qq[$0]');

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
        RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('$stash')
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier('put'),
            args => (
              RakuAST::StrLiteral.new($name),
              RakuAST::StrLiteral.new($0.Str),
            )
          )
        )
      }

      when / <is-numeric> / {
        RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('$stash')
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier('put'),
            args => (
              RakuAST::StrLiteral.new($name),
              RakuAST::Term::TopicCall.new(
                postfix => RakuAST::Call::Method.new(
                  name => RakuAST::Name.from-identifier('Numeric'),
                )
              )
            )
          )
        )
      }

      default {
        RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('$stash')
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier('put'),
            args => (
              RakuAST::StrLiteral.new($name),
              RakuAST::ApplyPostfix.new(
                operand => RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier('$stash')
                ),
                postfix => RakuAST::Call::Method.new(
                  name => RakuAST::Name.from-identifier('get'),
                  args => RakuAST::StrLiteral.new($value)
                )
              )
            )
          )
        )
      }
    }

    if $default {
      @statements = RakuAST::Statement::Unless(
        condition => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('$stash')
          ),
          postfix => RakuAST::Call::Method.new(
            name => RakuAST::Name.from-identifier('get'),
            args => RakuAST::QuotedString.new(
              segments   => (
                RakuAST::StrLiteral.new($name),
                RakuAST::ColonPair::True.new('strict') ,
              )
            )
          )
        ),
        body => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new( |@statements )
          )
        )
      )
    }
  }
}

method parse-default(*@values ,:$rakuast) {
    self.parse-set(:default, @values, :$rakuast)
}

my @blocks;
method parse-for($left, $op, $right, :$rakuast) {
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
    nextwith($left, $op, $right, :rakuast) if $rakuast;
    q:to/RAKU/
    for @($stash.get('\qq[$itemname]', :strict)) -> $\qq[$loopname] {
        $stash.put('\qq[$loopname]', $\qq[$loopname]);
    RAKU
}
multi method parse-for ($left, $op, $right, :$rakuast where *.so) {
  @blocks.push: ['for', $left, $op, $right];
}

method !parse-conditional(Str:D $name, @stmts is copy, :$rakuast) {
    my @words;
    my @statements;
    for @stmts -> $stmt is rw {
        next if @!keywords.grep($stmt);
        next if $stmt ~~ /^ \d+ $/;
        if /^ (\w+) $/, -> $word {
            if $rakuast {
                @statements.push: RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Type::Simple.new(
                        RakuAST::Name.from-identifier('$stash')
                    ),
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('get'),
                        args => RakuAST::QuotedString.new(
                            segments   => (
                                RakuAST::StrLiteral.new($word),
                                RakuAST::ColonPair::True.new('strict') ,
                            )
                        )
                    )
                )
            } else {
                $stmt .= subst($word { "\$stash.get('$word', :strict)" });
            }
        }
    }

    my $statement = @stmts.join(' ');
    nextwith($statement, :rakuast) if $rakuast;
    "$name $statement \{\n"
}
method !parse-conditional($statement, :$rakuast where *.so) {
   my $node = {
      when 'if'               { RakuAST::Statement::If     }
      when 'unless'           { RakuAST::Statement::Unless }
      when 'elsif' | 'elseif' { RakuAST::Statement::Elsif  }

   }

   # cw: Nah. That pesky curly!!! 😠
   #       original: "$name $statement \{\n"
   #     This needs more work. The condition is in $statement which needs
   #     to be parsed further to extract the infix.
   #     For now, we will only support simple statements, so comparisons
   #     where the RHS is a simple literal is the only thing that is in scope.

   my token operator {
       '<' | '>' | '<=>' | 'ne' | 'eq' | 'lt' | 'gt' | 'cmp'
   }

   (
     $statement ~~ /^
       [ $<neg>=['!' \s* ]?
       (.+?)
       [ \s* <operator> \s* (.+?) ]?
     $/
   )

   $statement ~~ /^ (.+?) \s* <operator> \s* (.+?) $/

   my $prefix;
   my $condition = do {

     when $/<neg> {
        $prefix = '!';
        proceed;
     }

     when $1 {
       $lhs = $1;
     }

     default {
       $infix = $/<operator>
       $rhs = $2;
     }
   }

   @block.push: [
     [ $node, $lhs, $prefix, $infix, $rhs ]
   ];
}


method parse-if(*@stmts) {
    self!parse-conditional('if', @stmts)
}

method parse-unless(*@stmts) {
    self!parse-conditional('unless', @stmts)
}

method parse-elsif(*@stmts) {
    "\n}\n" ~ self!parse-conditional('elsif', @stmts)
}

# cw: ... Here.
method parse-elseif(*@stmts) {
    self.parse-elsif(|@stmts)
}

method parse-else() {
    q:b[\n} else {\n]
    @blocks.push: 'else'
}

method parse-end {
    # cw: Oh. Dear. GOD! The horror...
    "\n}\n"
}

method remove-comment(*@tokens --> List) {
    @tokens.toggle(* ne '#').cache
}

method action($statement, :$rakuast) {
    my @stmts = $statement
      .lines
      .map({ self.remove-comment(.comb(/ \" .*? \" | \' .*? \' | \S+ /)) })
      .flat;
    return '' unless @stmts;

    my $name = @stmts.shift.lc;
    my $method = 'parse-' ~ $name;
    self.can($method)
      ?? $rakuast ?? self."$method"(|@stmts, :$rakuast)
      !! @stmts.elems >= 2 && @stmts[0] eq '='
        ?? self.parse-set($name, |@stmts, :$rakuast)
        !! self.parse-get($name, :$rakuast)
}

method get-safe-delimiter($raw-text) {
    my Set() $raw-words = $raw-text.words;
    (1..*).map('END' ~ *).first(* !(elem) $raw-words)
}

method compile($template) {
    my $script = q:to/RAKU/;
    return sub ($context) {
        my $stash = $context.stash;
        my $output = '';

    RAKU
    my @segments = $template.split(/ \n? '[%' $<comment-signature>=('#'?) \s* $<tokens>=(.*?) \s* '%]' /, :v);
    for @segments -> $segment {
        if $segment ~~ Stringy {
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
    $script ~= q:to/RAKU/;
        return $output;
    }
    RAKU
#    $*ERR.say: "<DEBUG:template>\n$script\n</DEBUG:template>";
    $script.subst( / 'my %localdata;' /, '', :nd(2..*) ).EVAL
}

# vim: expandtab shiftwidth=4
