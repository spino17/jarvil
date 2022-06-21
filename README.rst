Jarvil 0.1.0
============

JARVIL (Just A Rather Very Idiotic Language) is a metaset of Python. Jarvil add types to a subset of Python programming constructs. 
Jarvil compiles to readable, standards-based Python. Jarvil with it's type-check system in place can eliminate a lot of bugs at 
compile time which typically show up at runtime in a dynamically typed language like Python.

.. contents::

Build Instruction
-----------------

On Unix, Linux, BSD, macOS::

    ./configure
    make
    sudo make install

This will install the latest version of ``jarvil``.

Motivation
----------
Jarvil is a programming language made purely out of fun! I just wanted to learn the answer of "How a prgramming language is made ?".
This repo contains all my learnings on language design and writing a compiler. The patterns used in the implementation is heavily 
inspired by the famous dragon book on compilers (``Compilers: Principles, Techniques, and Tools, Second Edition. Alfred V. Aho, 
Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman``). Because of the descriptive nature of this repo, it can be used by anyone who wishes
to learn how to write a compiler for a simple language. All the stages for building the front-end of the compiler is implemented 
from scratch like lexical analyzer, parser, scope, type-checker, code-generator etc. For the backend, I have used llvm using the 
crate inkwell (safe wrapper on llvm-sys crate).

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

    # python style of block
    block: NEWLINE (INDENT stmt)*

    atom: id atom_factor  # semantic check - indexable with key of type C or propertry with name of id
    atom_factor:
        | ('[' C ']' | '.' id) atom_factor
        | ()
    C:
        | expr              # semantic check - index key is int or float
        | bexpr             # semantic check - index key is bool
        | literal           # semantic check - index key is string
        | atom              # semantic check - index key with type of atom

    type:
        | TYPE              # type token for in-built types
        | id                # semantic check - id is a user-defined type

    stmt: 
        | compound_stmt
        | simple_stmt NEWLINE

    simple_stmt:
        | decls             # semantic check - both side have matched types
        | assign            # semantic check - both side have matched types
        | id '(' params ')' # semantic check - number of params and their datatypes match the definition of the function or lambda
        | # calling a function, break, continue, return

    r_assign:
        | expr
        | bexpr
        | literal
        | atom

    params:
        | r_asssign ',' params
        | r_assign

    decls:
        | decl ',' decls
        | decl

    decl:
        | l_decl ['=' r_assign]

    l_decl:
        | type id

    assign:
        | atom '=' r_assign

    compound_stmt:
        | type_decl_stmt
        | function_stmt
        | if_stmt
        | for_stmt
        | while_stmt

    type_decl_stmt:
        | 'type' id ':' struct_block
        | 'type' id ':' '(' optparams ')' ['->' id] NEWLINE

    struct_block:
        | (INDENT l_decl NEWLINE)*

    function_stmt: 'def' id '(' optparams ')' ['->' id] ':' block

    optparams:
        | param ',' optparams
        | param

    param: type id

    if_stmt:
        | 'if' bexpr ':' block elif_stmt
        | 'if' bexpr ':' block [else_block]

    elif_stmt:
        | 'elif' bexpr ':' block elif_stmt
        | 'elif' bexpr ':' block [else_block]

    else_block:
        | 'else' ':' block

    while_stmt: 'while' bexpr ':' block

    expr: 
        | term additive
        | term

    additive:
        | '+' expr
        | '-' expr
        | ()

    term: 
        | factor multitive
        | factor

    multitive:
        | '*' term
        | '/' term

    factor:
        | '(' expr ')'
        | atom              # semantic check - type of atom is for valid '+', '-', '*', '/' operations
        | int
        | float

    comp_op:
        | '=='
        | '>='
        | '>'
        | '<='
        | '<'

    bexpr: 
        | bterm oritive

    oritive: 
        | 'or' bexpr
        | ()

    bterm: bfactor anditive

    anditive: 
        | 'and' bterm
        | ()

    bfactor:
        | 'not' bfactor
        | expr comp_op expr
        | '(' bexpr ')'
        | atom              # semantic check - type of atom is bool
        | 'True'
        | 'False'
