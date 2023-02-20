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

    type_expr: 'int' | 'float' | 'string' | 'bool' | id | '[' type_expr ']'

    name_type_spec: id ':' type_expr

    name_type_specs: name_type_spec (, name_type_spec)*

    r_assign: expr NEWLINE | 'func' '(' [name_type_specs] ')' ':' block

    'let' id '=' r_assign

    atom '=' r_assign

    'def' id '(' [name_type_specs] ')' ['->' type_expr] ':' block

    'type' id ':' NEWLINE (INDENT name_type_spec NEWLINE)* | '(' [name_type_specs] ')' ['->' type_expr]