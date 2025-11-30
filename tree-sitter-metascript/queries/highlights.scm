; Metascript Syntax Highlighting Queries

; Keywords
[
  "class"
  "function"
  "const"
  "let"
  "var"
  "return"
  "if"
  "else"
  "for"
  "while"
  "new"
  "extends"
  "implements"
] @keyword

; Metascript-specific: Macro decorators
(macro_decorator
  name: (identifier) @keyword.directive)

; Metascript-specific: @comptime blocks
(macro_comptime) @keyword.directive

; Types
(primitive_type) @type.builtin

(type_annotation
  (type
    (identifier) @type))

; Function/Method declarations
(function_declaration
  name: (identifier) @function)

(method_declaration
  name: (identifier) @function.method)

; Class declarations
(class_declaration
  name: (identifier) @type)

; Property declarations
(property_declaration
  name: (identifier) @property)

; Parameters
(parameter
  name: (identifier) @variable.parameter)

; Function calls
(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (member_expression
    property: (identifier) @function.method.call))

; Member access
(member_expression
  property: (identifier) @property)

; Variables
(variable_declaration
  name: (identifier) @variable)

; Literals
(number) @number
(string) @string
(template_string) @string
(boolean) @boolean
(null) @constant.builtin
(undefined) @constant.builtin
(this) @variable.builtin

; Comments
(comment) @comment

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "="
  "==="
  "!=="
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "?"
  ":"
] @operator

; Punctuation
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
  ";"
] @punctuation.delimiter
