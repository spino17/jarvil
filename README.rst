Jarvil 0.1.0
============

Jarvil is a statically-typed programming language with syntactic resemblance to python. 
Virtually, Jarvil code is just Python code with types sprinkled all over. These types enable an 
extra layer of type-check system which checks for type related semantic errors at compile time.

.. contents::

Build Instruction
-----------------

On Unix, Linux, BSD, macOS::

    ./configure
    make
    sudo make install

This will install the latest version of ``jarvil``.

Formal Description
------------------
Below is the complete grammer of the language with a custom (mostly copied from Python and PEG) expression language::

    # This is full specification of jarvil grammer. The style is PEG type where
    # |    ->  ordered choice
    # []   ->  optional
    # +    ->  at least one occurence
    # ()   ->  empty string
    # This grammer is by no means exhaustive for all programming constructs generally found in production grade languages. However
    # it surely contains enough so that anyone who wishes to learn language grammer can benefit from it.



    code: block ENDMARKER

    block: NEWLINE (INDENT stmt)*

    expr: expr bin_op expr | un_op expr | factor

    bin_op: 'or', 'and', '>', '>=', '<', '<=', '==', '!=', '+', '-', '*', '/'

    un_op: '+', '-', 'not'

    factor: <INT>, <FLOAT>, <LITERAL>, 'True', 'False', id, '(' expr ')'

    params: expr (, expr)*

    atom: (id | id '(' [params] ')' | id::id '(' [params] ')' ) atom_factor

    atom_factor: ('.' id ['(' [params] ')'] | '[' expr ']' | '(' [params] ')') atom_factor

    type_expr: 'int' | 'float' | 'string' | 'bool' | id | '[' type_expr; <INT> ']'

    name_type_spec: id ':' type_expr

    name_type_specs: name_type_spec (, name_type_spec)*

    r_assign: expr NEWLINE | 'func' '(' [name_type_specs] ')' ':' block

    'let' id '=' r_assign

    atom '=' r_assign

    'def' id '(' [name_type_specs] ')' ['->' type_expr] ':' block

    'type' id ':' NEWLINE (INDENT name_type_spec NEWLINE)* | '(' [name_type_specs] ')' ['->' type_expr]

Motivation
----------
Two things I really like in the programming world are Python syntax and the concept of statically-typed languages. 
Why I like Python syntax is quite obvious (I mean who doesn’t like to write English instead of code ;p), the second one is because of my 
experience working with statically-typed languages like Golang and Rust (and C in my early programming days). 
After all these years, I can now confidently say that in most cases if my Golang or Rust code compiles and If I am not doing something fancy like 
multithreading or concurrent programming, then my code would work just fine. This kind of confidence is further strengthened by the efforts of 
languages like Rust which completely redefined what can be checked at compile time (I mean, data-race and dangling pointer checks at compile time 
is still unbelievable) and so now even multithreading or concurrency is not a problem any more. 
So with Rust I can rephrase my confidence as “If my code compiles, I know it will work just fine”. I never gained this confidence with languages 
like Python or Ruby. Also I always wanted to make my own programming language. So I thought Python with static types would be an interesting project,
something which TypeScript did for JavaScript. This is the genesis of the Project-Jarvil.