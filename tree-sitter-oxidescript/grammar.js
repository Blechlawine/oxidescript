// https://github.com/tree-sitter/tree-sitter-rust/blob/master/grammar.js
// https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
// https://github.com/tree-sitter/tree-sitter-typescript/blob/master/common/define-grammar.js
// TODO: import the tree-sitter-typescript grammar and use that as a base, to not have to define basic types, literals and operators again
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

        identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

        range_pattern: ($) => seq($.number_literal, choice("..", "..="), $.number_literal),

        // LITERALS
        literal: ($) => choice($.number_literal, $.string_literal, $.boolean_literal),

        number_literal: ($) => {
            // this is the javascript number literal rule (https://github.com/tree-sitter/tree-sitter-javascript/blob/5720b249490b3c17245ba772f6be4a43edb4e3b7/grammar.js#LL1008C19-L1008C19)
            const hex_literal = seq(choice("0x", "0X"), /[\da-fA-F](_?[\da-fA-F])*/);

            const decimal_digits = /\d(_?\d)*/;
            const signed_integer = seq(optional(choice("-", "+")), decimal_digits);
            const exponent_part = seq(choice("e", "E"), signed_integer);

            const binary_literal = seq(choice("0b", "0B"), /[0-1](_?[0-1])*/);

            const octal_literal = seq(choice("0o", "0O"), /[0-7](_?[0-7])*/);

            const bigint_literal = seq(
                choice(hex_literal, binary_literal, octal_literal, decimal_digits),
                "n",
            );

            const decimal_integer_literal = choice(
                "0",
                seq(optional("0"), /[1-9]/, optional(seq(optional("_"), decimal_digits))),
            );

            const decimal_literal = choice(
                seq(
                    decimal_integer_literal,
                    ".",
                    optional(decimal_digits),
                    optional(exponent_part),
                ),
                seq(".", decimal_digits, optional(exponent_part)),
                seq(decimal_integer_literal, exponent_part),
                seq(decimal_digits),
            );

            return token(
                choice(hex_literal, decimal_literal, binary_literal, octal_literal, bigint_literal),
            );
        },
        string_literal: ($) => seq('"', /[^"]*/, '"'),
        boolean_literal: ($) => choice("true", "false"),

        // COMMENTS
        comment: ($) => choice($.line_comment, $.block_comment),
        line_comment: ($) => seq("//", /.*/),
        block_comment: ($) => seq("/*", /.*/, "*/"),

        _semicolon: ($) => ";",
    },
});
