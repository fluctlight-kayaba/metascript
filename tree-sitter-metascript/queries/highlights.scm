; Metascript Syntax Highlighting Queries

; Keywords
[
  ; JavaScript/TypeScript core
  "class"
  "interface"
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
  ; TypeScript
  "type"
  "async"
  ; ES modules
  "import"
  "export"
  "from"
  "as"
  "default"
  ; Metascript
  "macro"
  "extern"
  "defer"
  "distinct"
] @keyword

; Metascript-specific: Macro decorators (user macros applied to classes)
(macro_decorator
  "@" @keyword.directive
  name: (identifier) @keyword.directive)

; Metascript-specific: @comptime blocks
(macro_comptime
  "@comptime" @keyword.directive)

; Metascript-specific: macro declaration
(macro_declaration
  name: (identifier) @function.macro)

; Metascript-specific: @target blocks (conditional compilation)
(macro_target_block
  "@target" @keyword.directive)

; Metascript-specific: @emit statements (raw backend code)
(macro_emit_statement
  "@emit" @keyword.directive)

; Metascript-specific: @extern statements (native bindings)
(macro_extern_statement
  "@extern" @keyword.directive)

; Metascript-specific: extern macro declarations
(extern_macro
  name: (macro_name
    "@" @keyword.directive
    (identifier) @function.macro))

; Types
(primitive_type) @type.builtin
(metascript_type) @type.builtin

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

; Interface declarations
(interface_declaration
  name: (identifier) @type)

; Interface properties
(interface_property
  name: (identifier) @property)

; Interface methods
(interface_method
  name: (identifier) @function.method)

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
  "..."
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
