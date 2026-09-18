; Copied from `tree-sitter-jarvil/queries/highlights.scm`, which is the canonical
; file -- Zed loads queries from the extension rather than from the grammar
; repository, so the two have to be kept in step by hand. Edit the canonical one
; and copy it here; the header below explains the ordering rule.
;
; Syntax highlighting for Jarvil.
;
; Where two patterns match the same node the *later* one wins, so this file runs
; general -> specific: the catch-all `(identifier) @variable` sits at the top and
; every pattern below it narrows that down. Moving a pattern above the catch-all
; silently disables it, which is why the ordering is grouped and commented
; rather than alphabetical.
;
; Jarvil has no resolver in the editor, so everything here is syntactic. The
; grammar's `type_identifier` -- a capitalised name -- is what stands in for
; "this is a type", the same convention the compiler itself relies on.

; ------------------------------------------------------------------ catch-all

; Anything no later pattern claims is an ordinary variable reference.
(identifier) @variable

; ---------------------------------------------------------------------- types

(type_identifier) @type
(primitive_type) @type.builtin

(generic_parameter name: (type_identifier) @type.parameter)

; A capitalised name to the left of `::` is the type being reached through.
(path_expression type: (type_identifier) @type)

; Enum variants read as constructors: `Ok(T)`, `Err(E)`.
(enum_variant name: (type_identifier) @constructor)

; ------------------------------------------------------ parameters and members

(parameter name: (identifier) @variable.parameter)

; Struct and interface fields, and the `.field` access that reads them.
(field_declaration name: (identifier) @property)
(field_expression field: (identifier) @property)
(key_value key: (identifier) @property)

(self_expression) @variable.builtin

; ------------------------------------------------------------------ functions

; Last, so that a name being called or defined outranks the `@property` and
; `@variable` readings established above.
(call_expression function: (identifier) @function.call)

; `obj.method()` -- overrides the `field_expression` property rule.
(call_expression
  function: (field_expression field: (identifier) @function.method.call))

; `Result<int, str>::Ok(1)` -- an enum variant or an associated function.
(call_expression
  function: (path_expression member: (identifier) @function.call))

; Definition sites.
(function_definition name: (identifier) @function)
(method_prototype name: (identifier) @function.method)
(declare_statement name: (identifier) @function)

; ------------------------------------------------------------------- keywords

[
  "let"
  "def"
  "declare"
  "type"
  "struct"
  "enum"
  "interface"
  "implements"
  "lambda"
] @keyword

[
  "if"
  "elif"
  "else"
  "match"
  "case"
] @keyword.conditional

[
  "while"
  "for"
  "in"
] @keyword.repeat

"return" @keyword.return

; `break_statement` and `continue_statement` are single-token rules, so the
; keyword is the node itself rather than an anonymous child.
[
  (break_statement)
  (continue_statement)
] @keyword

; Word-shaped operators read better as keywords than as punctuation.
[
  "and"
  "or"
  "not"
] @keyword.operator

; ------------------------------------------------------------------- literals

(string) @string
(escape_sequence) @string.escape
[
  (integer)
  (float)
] @number
(boolean) @boolean
(comment) @comment

; -------------------------------------------------- operators and punctuation

[
  "+"
  "-"
  "*"
  "/"
  "**"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "="
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ":"
  "."
  "::"
  "->"
] @punctuation.delimiter
