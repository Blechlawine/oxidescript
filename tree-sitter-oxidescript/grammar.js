// https://github.com/tree-sitter/tree-sitter-rust/blob/master/grammar.js#L1261

module.exports = grammar({
    name: "oxidescript",

    rules: {
        source_file: ($) => repeat($._statement),

        _statement: ($) => choice($.expression_statement, $.declaration_statement),
        expression_statement: ($) => seq($.expression, $._semicolon),
        declaration_statement: ($) => choice($.variable_declaration),

        variable_declaration: ($) =>
            seq(
                "let",
                field("mutable", optional("mut")),
                field("name", $.identifier),
                optional($.initializer),
                $._semicolon,
            ),

        initializer: ($) => seq("=", $.expression),

        expression: ($) => choice($.literal, $.identifier),

        literal: ($) => choice($.number, $.string, $.boolean),

        identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

        number: ($) => /[0-9]*/,
        string: ($) => seq('"', /[^"]*/, '"'),
        boolean: ($) => /true|false/,

        _semicolon: ($) => ";",
    },
});
