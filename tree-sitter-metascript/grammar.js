/**
 * Metascript Grammar for Tree-sitter
 * TypeScript syntax + compile-time macros + multi-backend
 *
 * Synced with: src/lexer/token.zig (via tokens-generated.js)
 */

module.exports = grammar({
  name: 'metascript',

  extras: $ => [/\s/, $.comment],

  conflicts: $ => [
    [$.block, $.object],
    [$.parameter, $._expression],
  ],

  word: $ => $.identifier,

  rules: {
    program: $ => repeat($._statement),

    // =========================================================================
    // Statements
    // =========================================================================

    _statement: $ => choice(
      $.class_declaration,
      $.function_declaration,
      $.variable_declaration,
      $.type_alias_declaration,
      $.defer_statement,
      $.return_statement,
      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.expression_statement,
      $.block,
    ),

    // Metascript: defer statement
    defer_statement: $ => seq('defer', $._expression, ';'),

    // =========================================================================
    // Declarations
    // =========================================================================

    class_declaration: $ => seq(
      repeat($.macro_decorator),
      'class',
      field('name', $.identifier),
      optional($.type_parameters),
      optional(seq('extends', $.type)),
      optional(seq('implements', commaSep1($.type))),
      field('body', $.class_body),
    ),

    class_body: $ => seq(
      '{',
      repeat(choice($.property_declaration, $.method_declaration)),
      '}',
    ),

    property_declaration: $ => seq(
      repeat($.macro_decorator),
      field('name', $.identifier),
      optional($.type_annotation),
      optional(seq('=', $._expression)),
      ';',
    ),

    method_declaration: $ => seq(
      repeat($.macro_decorator),
      field('name', $.identifier),
      $.parameters,
      optional($.type_annotation),
      field('body', $.block),
    ),

    function_declaration: $ => seq(
      repeat($.macro_decorator),
      optional('async'),
      'function',
      field('name', $.identifier),
      optional($.type_parameters),
      $.parameters,
      optional($.type_annotation),
      field('body', $.block),
    ),

    variable_declaration: $ => seq(
      choice('const', 'let', 'var'),
      field('name', $.identifier),
      optional($.type_annotation),
      optional(seq('=', $._expression)),
      ';',
    ),

    // Type alias with optional distinct
    type_alias_declaration: $ => seq(
      'type',
      field('name', $.identifier),
      optional($.type_parameters),
      '=',
      optional('distinct'),  // Metascript: distinct types
      $.type,
      ';',
    ),

    // =========================================================================
    // Macros (Metascript decorators)
    // =========================================================================

    macro_decorator: $ => seq(
      '@',
      field('name', $.identifier),
      optional($.macro_arguments),
    ),

    macro_arguments: $ => seq('(', optional(commaSep1($._expression)), ')'),

    // =========================================================================
    // Types
    // =========================================================================

    type_annotation: $ => seq(':', $.type),

    type: $ => choice(
      $.primitive_type,
      $.metascript_type,
      $.identifier,
      $.array_type,
      $.union_type,
      $.generic_type,
    ),

    primitive_type: $ => choice(
      'string', 'number', 'boolean', 'void', 'any', 'unknown', 'never',
    ),

    metascript_type: $ => choice(
      // Sized integer types
      'int8', 'int16', 'int32', 'int64',
      'uint8', 'uint16', 'uint32', 'uint64',
      // Sized float types
      'float32', 'float64',
      // Type aliases
      'int', 'float', 'double',
    ),

    array_type: $ => prec.left(seq($.type, '[', ']')),

    union_type: $ => prec.left(seq($.type, '|', $.type)),

    generic_type: $ => seq($.identifier, '<', commaSep1($.type), '>'),

    type_parameters: $ => seq('<', commaSep1($.identifier), '>'),

    parameters: $ => seq('(', optional(commaSep1($.parameter)), ')'),

    parameter: $ => seq(
      field('name', $.identifier),
      optional($.type_annotation),
      optional(seq('=', $._expression)),
    ),

    // =========================================================================
    // Control Flow
    // =========================================================================

    block: $ => seq('{', repeat($._statement), '}'),

    return_statement: $ => seq('return', optional($._expression), ';'),

    if_statement: $ => prec.right(seq(
      'if', '(', $._expression, ')', $._statement,
      optional(seq('else', $._statement)),
    )),

    for_statement: $ => seq(
      'for', '(',
      choice($.variable_declaration, seq(optional($._expression), ';')),
      optional($._expression), ';',
      optional($._expression),
      ')', $._statement,
    ),

    while_statement: $ => seq('while', '(', $._expression, ')', $._statement),

    expression_statement: $ => seq($._expression, ';'),

    // =========================================================================
    // Expressions
    // =========================================================================

    _expression: $ => choice(
      $.identifier,
      $.number,
      $.string,
      $.boolean,
      $.null,
      $.undefined,
      $.this,
      $.assignment_expression,
      $.ternary_expression,
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.member_expression,
      $.new_expression,
      $.array,
      $.object,
      $.arrow_function,
      $.parenthesized_expression,
      $.macro_comptime,
    ),

    macro_comptime: $ => seq('@comptime', $.block),

    assignment_expression: $ => prec.right(1, seq(
      field('left', choice($.identifier, $.member_expression)),
      choice('=', '+=', '-=', '*=', '/='),
      field('right', $._expression),
    )),

    ternary_expression: $ => prec.right(2, seq(
      $._expression, '?', $._expression, ':', $._expression,
    )),

    binary_expression: $ => choice(
      prec.left(12, seq($._expression, '*', $._expression)),
      prec.left(12, seq($._expression, '/', $._expression)),
      prec.left(12, seq($._expression, '%', $._expression)),
      prec.left(11, seq($._expression, '+', $._expression)),
      prec.left(11, seq($._expression, '-', $._expression)),
      prec.left(9, seq($._expression, '<', $._expression)),
      prec.left(9, seq($._expression, '>', $._expression)),
      prec.left(9, seq($._expression, '<=', $._expression)),
      prec.left(9, seq($._expression, '>=', $._expression)),
      prec.left(8, seq($._expression, '===', $._expression)),
      prec.left(8, seq($._expression, '!==', $._expression)),
      prec.left(8, seq($._expression, '==', $._expression)),
      prec.left(8, seq($._expression, '!=', $._expression)),
      prec.left(4, seq($._expression, '&&', $._expression)),
      prec.left(3, seq($._expression, '||', $._expression)),
      prec.right(13, seq($._expression, '**', $._expression)),
    ),

    unary_expression: $ => prec(14, choice(
      seq('!', $._expression),
      seq('-', $._expression),
      seq('+', $._expression),
    )),

    call_expression: $ => prec(18, seq(
      field('function', $._expression),
      field('arguments', $.arguments),
    )),

    arguments: $ => seq('(', optional(commaSep1($._expression)), ')'),

    member_expression: $ => prec(18, seq(
      field('object', $._expression),
      '.',
      field('property', $.identifier),
    )),

    new_expression: $ => prec.right(17, seq(
      'new', $._expression, optional(seq('(', optional(commaSep1($._expression)), ')')),
    )),

    array: $ => seq('[', optional(commaSep1($._expression)), ']'),

    object: $ => seq('{', optional(commaSep1($.pair)), '}'),

    pair: $ => seq(
      field('key', choice($.identifier, $.string)),
      ':',
      field('value', $._expression),
    ),

    arrow_function: $ => prec.right(seq(
      choice($.identifier, $.parameters),
      '=>',
      choice($.block, $._expression),
    )),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // =========================================================================
    // Literals
    // =========================================================================

    identifier: $ => /[a-zA-Z_$][a-zA-Z0-9_$]*/,

    number: $ => choice(
      /0[xX][0-9a-fA-F]+/,
      /0[bB][01]+/,
      /0[oO][0-7]+/,
      /\d+(\.\d+)?([eE][+-]?\d+)?/,
    ),

    string: $ => choice(
      // Use token() to make strings atomic - prevents // inside strings from being parsed as comments
      token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),
      token(seq("'", repeat(choice(/[^'\\]/, /\\./)), "'")),
      $.template_string,
    ),

    template_string: $ => seq(
      '`',
      repeat(choice(/[^`$\\]/, /\\./, seq('${', $._expression, '}'))),
      '`',
    ),

    boolean: $ => choice('true', 'false'),
    null: $ => 'null',
    undefined: $ => 'undefined',
    this: $ => 'this',

    comment: $ => choice(
      seq('//', /.*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
    ),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}
