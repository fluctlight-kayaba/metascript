/**
 * Metascript Grammar for Tree-sitter
 * Based on TypeScript with @macro extensions
 */

module.exports = grammar({
  name: 'metascript',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  conflicts: $ => [
    [$.block, $.object],
  ],

  word: $ => $.identifier,

  rules: {
    program: $ => repeat($._statement),

    // Statements
    _statement: $ => choice(
      $.class_declaration,
      $.function_declaration,
      $.variable_declaration,
      $.expression_statement,
      $.return_statement,
      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.block,
    ),

    // Metascript-specific: Macro decorators
    macro_decorator: $ => seq(
      '@',
      field('name', $.identifier),
      optional($.macro_arguments),
    ),

    macro_arguments: $ => seq(
      '(',
      optional(commaSep1(choice($.identifier, $.string, $.number))),
      ')',
    ),

    // Class declaration with optional macro
    class_declaration: $ => seq(
      optional($.macro_decorator),
      'class',
      field('name', $.identifier),
      optional($.type_parameters),
      optional(seq('extends', $.type)),
      optional(seq('implements', commaSep1($.type))),
      field('body', $.class_body),
    ),

    class_body: $ => seq(
      '{',
      repeat(choice(
        $.property_declaration,
        $.method_declaration,
      )),
      '}',
    ),

    property_declaration: $ => seq(
      field('name', $.identifier),
      optional($.type_annotation),
      optional(seq('=', $._expression)),
      ';',
    ),

    method_declaration: $ => seq(
      field('name', $.identifier),
      $.parameters,
      optional($.type_annotation),
      field('body', $.block),
    ),

    // Function declaration
    function_declaration: $ => seq(
      'function',
      field('name', $.identifier),
      $.parameters,
      optional($.type_annotation),
      field('body', $.block),
    ),

    parameters: $ => seq(
      '(',
      optional(commaSep1($.parameter)),
      ')',
    ),

    parameter: $ => seq(
      field('name', $.identifier),
      optional($.type_annotation),
    ),

    type_annotation: $ => seq(':', $.type),

    type: $ => choice(
      $.primitive_type,
      $.identifier,
      $.array_type,
      $.union_type,
    ),

    primitive_type: $ => choice(
      'string',
      'number',
      'boolean',
      'void',
      'any',
      'unknown',
    ),

    array_type: $ => seq($.type, '[', ']'),

    union_type: $ => prec.left(seq($.type, '|', $.type)),

    type_parameters: $ => seq(
      '<',
      commaSep1($.identifier),
      '>',
    ),

    // Variable declaration
    variable_declaration: $ => seq(
      choice('const', 'let', 'var'),
      field('name', $.identifier),
      optional($.type_annotation),
      optional(seq('=', $._expression)),
      ';',
    ),

    // Block
    block: $ => seq(
      '{',
      repeat($._statement),
      '}',
    ),

    // Return statement
    return_statement: $ => seq(
      'return',
      optional($._expression),
      ';',
    ),

    // If statement
    if_statement: $ => prec.right(seq(
      'if',
      '(',
      $._expression,
      ')',
      $._statement,
      optional(seq('else', $._statement)),
    )),

    // For statement
    for_statement: $ => seq(
      'for',
      '(',
      choice(
        $.variable_declaration,
        seq($._expression, ';'),
      ),
      optional($._expression),
      ';',
      optional($._expression),
      ')',
      $._statement,
    ),

    // While statement
    while_statement: $ => seq(
      'while',
      '(',
      $._expression,
      ')',
      $._statement,
    ),

    // Expression statement
    expression_statement: $ => seq($._expression, ';'),

    // Expressions
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
      $.parenthesized_expression,
      $.macro_comptime,  // Metascript-specific
    ),

    // Metascript-specific: @comptime blocks
    macro_comptime: $ => seq(
      '@comptime',
      $.block,
    ),

    assignment_expression: $ => prec.right(1, seq(
      field('left', choice($.identifier, $.member_expression)),
      '=',
      field('right', $._expression),
    )),

    ternary_expression: $ => prec.right(2, seq(
      field('condition', $._expression),
      '?',
      field('consequence', $._expression),
      ':',
      field('alternative', $._expression),
    )),

    binary_expression: $ => choice(
      prec.left(10, seq($._expression, '*', $._expression)),
      prec.left(10, seq($._expression, '/', $._expression)),
      prec.left(9, seq($._expression, '+', $._expression)),
      prec.left(9, seq($._expression, '-', $._expression)),
      prec.left(7, seq($._expression, '<', $._expression)),
      prec.left(7, seq($._expression, '>', $._expression)),
      prec.left(7, seq($._expression, '<=', $._expression)),
      prec.left(7, seq($._expression, '>=', $._expression)),
      prec.left(6, seq($._expression, '===', $._expression)),
      prec.left(6, seq($._expression, '!==', $._expression)),
      prec.left(6, seq($._expression, '==', $._expression)),
      prec.left(6, seq($._expression, '!=', $._expression)),
      prec.left(4, seq($._expression, '&&', $._expression)),
      prec.left(3, seq($._expression, '||', $._expression)),
    ),

    unary_expression: $ => choice(
      prec(14, seq('!', $._expression)),
      prec(14, seq('-', $._expression)),
      prec(14, seq('+', $._expression)),
    ),

    call_expression: $ => prec(18, seq(
      field('function', $._expression),
      field('arguments', $.arguments),
    )),

    arguments: $ => seq(
      '(',
      optional(commaSep1($._expression)),
      ')',
    ),

    member_expression: $ => prec(18, seq(
      field('object', $._expression),
      '.',
      field('property', $.identifier),
    )),

    new_expression: $ => prec.right(17, seq(
      'new',
      field('constructor', $._expression),
      optional($.arguments),
    )),

    array: $ => seq(
      '[',
      optional(commaSep1($._expression)),
      ']',
    ),

    object: $ => seq(
      '{',
      optional(commaSep1($.pair)),
      '}',
    ),

    pair: $ => seq(
      field('key', choice($.identifier, $.string)),
      ':',
      field('value', $._expression),
    ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // Literals
    identifier: $ => /[a-zA-Z_$][a-zA-Z0-9_$]*/,

    number: $ => /\d+(\.\d+)?/,

    string: $ => choice(
      seq('"', repeat(choice(/[^"\\]/, $.escape_sequence)), '"'),
      seq("'", repeat(choice(/[^'\\]/, $.escape_sequence)), "'"),
      $.template_string,
    ),

    template_string: $ => seq(
      '`',
      repeat(choice(
        /[^`$\\]/,
        $.escape_sequence,
        seq('$', /[^{]/),
        seq('${', $._expression, '}'),
      )),
      '`',
    ),

    escape_sequence: $ => /\\./,

    boolean: $ => choice('true', 'false'),

    null: $ => 'null',

    undefined: $ => 'undefined',

    this: $ => 'this',

    // Comments
    comment: $ => choice(
      seq('//', /.*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
    ),
  },
});

// Helper function for comma-separated lists
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}
