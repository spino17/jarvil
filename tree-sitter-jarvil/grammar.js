/**
 * Tree-sitter grammar for Jarvil.
 *
 * Blocks are delimited by indentation, so `_newline`, `_indent` and `_dedent`
 * come from the external scanner in `src/scanner.c` rather than from this file.
 *
 * Operator precedences below mirror `precedence()` in the compiler's
 * `lexer/token.rs`. If one changes, change both -- a mismatch means the editor
 * highlights a different tree than the compiler parses.
 *
 * @file Jarvil grammar for tree-sitter
 * @author Bhavya Bhatt
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  or: 1,
  and: 2,
  comparison: 3,
  additive: 4,
  multiplicative: 5,
  unary: 6,
  power: 7,
  call: 8,
};

module.exports = grammar({
  name: "jarvil",

  externals: ($) => [$._newline, $._indent, $._dedent],

  extras: ($) => [/[ \t\r]/, $.comment],

  word: ($) => $.identifier,

  // `<` is genuinely ambiguous: `a < b` is a comparison, `f<T>(x)` is a call
  // with type arguments, and nothing local distinguishes them. The compiler
  // resolves it by always preferring type arguments -- which is why `a < b`
  // does not parse there. A highlighter should be more forgiving, so these
  // conflicts let tree-sitter's GLR explore both and keep whichever completes.
  conflicts: ($) => [
    [$.unary_expression, $.binary_expression, $.call_expression],
    [$.binary_expression, $.call_expression],
    // A capitalised name followed by `<` could be a generic type or a
    // comparison against a type-shaped expression; let GLR decide.
    [$.generic_type, $._expression],
    // `(A, B)` is a tuple type in a signature and a tuple expression in a
    // body; the two are indistinguishable until the surrounding context.
    [$._type, $._expression],
  ],

  rules: {
    source_file: ($) => repeat(choice($._statement, $._newline)),

    // ---------------------------------------------------------------- comments

    comment: (_) =>
      token(
        choice(
          seq("//", /[^\n]*/),
          seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"),
        ),
      ),

    // -------------------------------------------------------------- statements

    _statement: ($) => choice($._simple_statement, $._compound_statement),

    _simple_statement: ($) =>
      seq(
        choice(
          $.variable_declaration,
          $.assignment,
          $.return_statement,
          $.break_statement,
          $.continue_statement,
          $.declare_statement,
          $.expression_statement,
        ),
        $._newline,
      ),

    _compound_statement: ($) =>
      choice(
        $.function_definition,
        $.struct_definition,
        $.enum_definition,
        $.interface_definition,
        $.if_statement,
        $.while_statement,
        $.for_statement,
        $.match_statement,
      ),

    block: ($) =>
      seq($._indent, repeat1(choice($._statement, $._newline)), $._dedent),

    enum_body: ($) =>
      seq($._indent, repeat1(seq($.enum_variant, $._newline)), $._dedent),

    struct_body: ($) =>
      seq(
        $._indent,
        repeat1(choice(seq($.field_declaration, $._newline), $.function_definition)),
        $._dedent,
      ),

    interface_body: ($) =>
      seq(
        $._indent,
        repeat1(
          choice(seq($.field_declaration, $._newline), seq($.method_prototype, $._newline)),
        ),
        $._dedent,
      ),

    match_body: ($) => seq($._indent, repeat1($.case_clause), $._dedent),

    // An interface declares a method without a body.
    method_prototype: ($) =>
      seq(
        "def",
        field("name", $.identifier),
        optional(field("type_parameters", $.generic_parameters)),
        field("parameters", $.parameters),
        optional(seq("->", field("return_type", $._type))),
      ),

    // ------------------------------------------------------------ declarations

    variable_declaration: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
      ),

    assignment: ($) =>
      seq(field("left", $._expression), "=", field("right", $._expression)),

    function_definition: ($) =>
      seq(
        "def",
        field("name", $.identifier),
        optional(field("type_parameters", $.generic_parameters)),
        field("parameters", $.parameters),
        optional(seq("->", field("return_type", $._type))),
        ":",
        field("body", $.block),
      ),

    declare_statement: ($) =>
      seq(
        "declare",
        "def",
        field("name", $.identifier),
        optional(field("type_parameters", $.generic_parameters)),
        field("parameters", $.parameters),
        optional(seq("->", field("return_type", $._type))),
      ),

    parameters: ($) => seq("(", commaSep($.parameter), ")"),

    parameter: ($) =>
      seq(field("name", $.identifier), ":", field("type", $._type)),

    // `type Name<T> struct implements I:` / `type Name<T> enum:`
    struct_definition: ($) =>
      seq(
        "type",
        field("name", $.type_identifier),
        optional(field("type_parameters", $.generic_parameters)),
        "struct",
        optional(seq("implements", commaSep1($.type_identifier))),
        ":",
        field("body", $.struct_body),
      ),

    enum_definition: ($) =>
      seq(
        "type",
        field("name", $.type_identifier),
        optional(field("type_parameters", $.generic_parameters)),
        "enum",
        ":",
        field("body", $.enum_body),
      ),

    interface_definition: ($) =>
      seq(
        "interface",
        field("name", $.type_identifier),
        optional(field("type_parameters", $.generic_parameters)),
        ":",
        field("body", $.interface_body),
      ),

    // Inside a struct or interface body: `name: type` is a field, and an enum
    // body holds `Variant` or `Variant(T)`.
    field_declaration: ($) =>
      seq(field("name", $.identifier), ":", field("type", $._type)),

    enum_variant: ($) =>
      seq(
        field("name", $.type_identifier),
        optional(seq("(", field("payload", $._type), ")")),
      ),

    // --------------------------------------------------------- control flow

    if_statement: ($) =>
      seq(
        "if",
        field("condition", $._expression),
        ":",
        field("consequence", $.block),
        repeat($.elif_clause),
        optional($.else_clause),
      ),

    elif_clause: ($) =>
      seq("elif", field("condition", $._expression), ":", field("body", $.block)),

    else_clause: ($) => seq("else", ":", field("body", $.block)),

    while_statement: ($) =>
      seq("while", field("condition", $._expression), ":", field("body", $.block)),

    for_statement: ($) =>
      seq(
        "for",
        field("binding", $.identifier),
        "in",
        field("iterable", $._expression),
        ":",
        field("body", $.block),
      ),

    match_statement: ($) =>
      seq("match", field("subject", $._expression), ":", field("body", $.match_body)),

    case_clause: ($) =>
      seq("case", field("pattern", $._expression), ":", field("body", $.block)),

    return_statement: ($) => seq("return", optional($._expression)),
    break_statement: (_) => "break",
    continue_statement: (_) => "continue",
    expression_statement: ($) => $._expression,

    // ------------------------------------------------------------- generics

    generic_parameters: ($) => seq("<", commaSep1($.generic_parameter), ">"),

    generic_parameter: ($) =>
      seq(
        field("name", $.type_identifier),
        optional(
          seq(
            ":",
            field("bound", $.type_identifier),
            repeat(seq("+", field("bound", $.type_identifier))),
          ),
        ),
      ),

    generic_arguments: ($) => seq("<", commaSep1($._type), ">"),

    // ---------------------------------------------------------------- types

    _type: ($) =>
      choice(
        $.primitive_type,
        $.array_type,
        $.hashmap_type,
        $.tuple_type,
        $.generic_type,
        $.type_identifier,
      ),

    primitive_type: (_) => choice("int", "float", "str", "bool"),
    array_type: ($) => seq("[", $._type, "]"),
    hashmap_type: ($) => seq("{", $._type, ":", $._type, "}"),
    tuple_type: ($) => seq("(", commaSep1($._type), ")"),
    generic_type: ($) => seq($.type_identifier, $.generic_arguments),

    // ---------------------------------------------------------- expressions

    _expression: ($) =>
      choice(
        $.identifier,
        $.type_identifier,
        $.self_expression,
        $.integer,
        $.float,
        $.string,
        $.boolean,
        $.array_expression,
        $.hashmap_expression,
        $.tuple_expression,
        $.parenthesized_expression,
        $.unary_expression,
        $.binary_expression,
        $.call_expression,
        $.index_expression,
        $.field_expression,
        $.path_expression,
        $.lambda_expression,
      ),

    unary_expression: ($) =>
      prec.right(
        PREC.unary,
        seq(field("operator", choice("+", "-", "not")), field("operand", $._expression)),
      ),

    binary_expression: ($) => {
      const table = [
        [PREC.or, "or"],
        [PREC.and, "and"],
        [PREC.comparison, choice("==", "!=", "<", "<=", ">", ">=")],
        [PREC.additive, choice("+", "-")],
        [PREC.multiplicative, choice("*", "/")],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              field("operator", operator),
              field("right", $._expression),
            ),
          ),
        ),
        // `**` is the one right-associative operator
        prec.right(
          PREC.power,
          seq(field("left", $._expression), field("operator", "**"), field("right", $._expression)),
        ),
      );
    },

    call_expression: ($) =>
      prec(
        PREC.call,
        seq(
          field("function", $._expression),
          optional(field("type_arguments", $.generic_arguments)),
          field("arguments", $.arguments),
        ),
      ),

    field_expression: ($) =>
      prec(PREC.call, seq(field("receiver", $._expression), ".", field("field", $.identifier))),

    index_expression: ($) =>
      prec(PREC.call, seq(field("receiver", $._expression), "[", field("index", $._expression), "]")),

    // `Result<int, str>::Ok(1)` -- an enum variant or a class method
    path_expression: ($) =>
      prec(
        PREC.call,
        seq(
          field("type", choice($.type_identifier, $.generic_type)),
          "::",
          field("member", $.identifier),
        ),
      ),

    lambda_expression: ($) =>
      seq(
        "lambda",
        field("parameters", $.parameters),
        optional(seq("->", field("return_type", $._type))),
        ":",
        field("body", $.block),
      ),

    arguments: ($) => seq("(", commaSep($._expression), ")"),

    array_expression: ($) => seq("[", commaSep($._expression), "]"),

    hashmap_expression: ($) => seq("{", commaSep($.key_value), "}"),

    key_value: ($) =>
      seq(field("key", $._expression), ":", field("value", $._expression)),

    tuple_expression: ($) =>
      seq("(", $._expression, ",", commaSep1($._expression), ")"),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    // ------------------------------------------------------------- literals

    self_expression: (_) => "self",
    boolean: (_) => choice("True", "False"),
    integer: (_) => token(/\d+/),
    float: (_) => token(/\d+\.\d+/),

    string: ($) =>
      choice(
        seq('"', repeat(choice($.escape_sequence, /[^"\\\n]/)), '"'),
        seq("'", repeat(choice($.escape_sequence, /[^'\\\n]/)), "'"),
      ),

    escape_sequence: (_) => token.immediate(seq("\\", /./)),

    // A capitalised name is a type by convention, which is what lets the
    // highlighter colour types differently without consulting the resolver.
    type_identifier: (_) => token(/[A-Z][A-Za-z0-9_]*/),
    identifier: (_) => token(/[a-zA-Z_][a-zA-Z0-9_]*/),
  },
});

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}
