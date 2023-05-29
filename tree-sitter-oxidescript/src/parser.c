#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 25
#define LARGE_STATE_COUNT 4
#define SYMBOL_COUNT 20
#define ALIAS_COUNT 0
#define TOKEN_COUNT 10
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 2
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 3

enum {
  anon_sym_let = 1,
  anon_sym_mut = 2,
  anon_sym_EQ = 3,
  sym_identifier = 4,
  sym_number = 5,
  anon_sym_DQUOTE = 6,
  aux_sym_string_token1 = 7,
  sym_boolean = 8,
  sym__semicolon = 9,
  sym_source_file = 10,
  sym__statement = 11,
  sym_expression_statement = 12,
  sym_declaration_statement = 13,
  sym_variable_declaration = 14,
  sym_initializer = 15,
  sym_expression = 16,
  sym_literal = 17,
  sym_string = 18,
  aux_sym_source_file_repeat1 = 19,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_let] = "let",
  [anon_sym_mut] = "mut",
  [anon_sym_EQ] = "=",
  [sym_identifier] = "identifier",
  [sym_number] = "number",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [sym_boolean] = "boolean",
  [sym__semicolon] = "_semicolon",
  [sym_source_file] = "source_file",
  [sym__statement] = "_statement",
  [sym_expression_statement] = "expression_statement",
  [sym_declaration_statement] = "declaration_statement",
  [sym_variable_declaration] = "variable_declaration",
  [sym_initializer] = "initializer",
  [sym_expression] = "expression",
  [sym_literal] = "literal",
  [sym_string] = "string",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_mut] = anon_sym_mut,
  [anon_sym_EQ] = anon_sym_EQ,
  [sym_identifier] = sym_identifier,
  [sym_number] = sym_number,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [sym_boolean] = sym_boolean,
  [sym__semicolon] = sym__semicolon,
  [sym_source_file] = sym_source_file,
  [sym__statement] = sym__statement,
  [sym_expression_statement] = sym_expression_statement,
  [sym_declaration_statement] = sym_declaration_statement,
  [sym_variable_declaration] = sym_variable_declaration,
  [sym_initializer] = sym_initializer,
  [sym_expression] = sym_expression,
  [sym_literal] = sym_literal,
  [sym_string] = sym_string,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_mut] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_number] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_boolean] = {
    .visible = true,
    .named = true,
  },
  [sym__semicolon] = {
    .visible = false,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__statement] = {
    .visible = false,
    .named = true,
  },
  [sym_expression_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_declaration_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_variable_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_initializer] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_mutable = 1,
  field_name = 2,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_mutable] = "mutable",
  [field_name] = "name",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 1},
  [1] =
    {field_mutable, 1},
    {field_name, 2},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(3);
      if (lookahead == '"') ADVANCE(20);
      if (lookahead == ';') ADVANCE(23);
      if (lookahead == '=') ADVANCE(6);
      if (lookahead == 'f') ADVANCE(7);
      if (lookahead == 'l') ADVANCE(9);
      if (lookahead == 'm') ADVANCE(16);
      if (lookahead == 't') ADVANCE(11);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 1:
      if (lookahead == 'm') ADVANCE(16);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 2:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_mut);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(10);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(17);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(13);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(15);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(8);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(4);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(5);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(8);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(14);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(17);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(18);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_number);
      if (eof) ADVANCE(3);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(18);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(21);
      if (lookahead != 0 &&
          lookahead != '"') ADVANCE(22);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"') ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym__semicolon);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 19},
  [2] = {.lex_state = 19},
  [3] = {.lex_state = 19},
  [4] = {.lex_state = 18},
  [5] = {.lex_state = 19},
  [6] = {.lex_state = 19},
  [7] = {.lex_state = 19},
  [8] = {.lex_state = 19},
  [9] = {.lex_state = 19},
  [10] = {.lex_state = 19},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 1},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 2},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 21},
  [24] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_mut] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [sym_boolean] = ACTIONS(1),
    [sym__semicolon] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(20),
    [sym__statement] = STATE(3),
    [sym_expression_statement] = STATE(3),
    [sym_declaration_statement] = STATE(3),
    [sym_variable_declaration] = STATE(9),
    [sym_expression] = STATE(17),
    [sym_literal] = STATE(15),
    [sym_string] = STATE(24),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [sym_identifier] = ACTIONS(7),
    [sym_number] = ACTIONS(9),
    [anon_sym_DQUOTE] = ACTIONS(11),
    [sym_boolean] = ACTIONS(13),
  },
  [2] = {
    [sym__statement] = STATE(2),
    [sym_expression_statement] = STATE(2),
    [sym_declaration_statement] = STATE(2),
    [sym_variable_declaration] = STATE(9),
    [sym_expression] = STATE(17),
    [sym_literal] = STATE(15),
    [sym_string] = STATE(24),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(15),
    [anon_sym_let] = ACTIONS(17),
    [sym_identifier] = ACTIONS(20),
    [sym_number] = ACTIONS(23),
    [anon_sym_DQUOTE] = ACTIONS(26),
    [sym_boolean] = ACTIONS(29),
  },
  [3] = {
    [sym__statement] = STATE(2),
    [sym_expression_statement] = STATE(2),
    [sym_declaration_statement] = STATE(2),
    [sym_variable_declaration] = STATE(9),
    [sym_expression] = STATE(17),
    [sym_literal] = STATE(15),
    [sym_string] = STATE(24),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(32),
    [anon_sym_let] = ACTIONS(5),
    [sym_identifier] = ACTIONS(7),
    [sym_number] = ACTIONS(9),
    [anon_sym_DQUOTE] = ACTIONS(11),
    [sym_boolean] = ACTIONS(13),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 7,
    ACTIONS(7), 1,
      sym_identifier,
    ACTIONS(9), 1,
      sym_number,
    ACTIONS(11), 1,
      anon_sym_DQUOTE,
    ACTIONS(13), 1,
      sym_boolean,
    STATE(15), 1,
      sym_literal,
    STATE(22), 1,
      sym_expression,
    STATE(24), 1,
      sym_string,
  [22] = 2,
    ACTIONS(34), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(36), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [33] = 2,
    ACTIONS(38), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(40), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [44] = 2,
    ACTIONS(42), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(44), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [55] = 2,
    ACTIONS(46), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(48), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [66] = 2,
    ACTIONS(50), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(52), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [77] = 2,
    ACTIONS(54), 2,
      ts_builtin_sym_end,
      sym_number,
    ACTIONS(56), 4,
      anon_sym_let,
      sym_identifier,
      anon_sym_DQUOTE,
      sym_boolean,
  [88] = 3,
    ACTIONS(58), 1,
      anon_sym_EQ,
    ACTIONS(60), 1,
      sym__semicolon,
    STATE(21), 1,
      sym_initializer,
  [98] = 3,
    ACTIONS(58), 1,
      anon_sym_EQ,
    ACTIONS(62), 1,
      sym__semicolon,
    STATE(18), 1,
      sym_initializer,
  [108] = 2,
    ACTIONS(64), 1,
      anon_sym_mut,
    ACTIONS(66), 1,
      sym_identifier,
  [115] = 1,
    ACTIONS(68), 1,
      anon_sym_DQUOTE,
  [119] = 1,
    ACTIONS(70), 1,
      sym__semicolon,
  [123] = 1,
    ACTIONS(72), 1,
      sym_identifier,
  [127] = 1,
    ACTIONS(74), 1,
      sym__semicolon,
  [131] = 1,
    ACTIONS(76), 1,
      sym__semicolon,
  [135] = 1,
    ACTIONS(78), 1,
      sym__semicolon,
  [139] = 1,
    ACTIONS(80), 1,
      ts_builtin_sym_end,
  [143] = 1,
    ACTIONS(82), 1,
      sym__semicolon,
  [147] = 1,
    ACTIONS(84), 1,
      sym__semicolon,
  [151] = 1,
    ACTIONS(86), 1,
      aux_sym_string_token1,
  [155] = 1,
    ACTIONS(88), 1,
      sym__semicolon,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(4)] = 0,
  [SMALL_STATE(5)] = 22,
  [SMALL_STATE(6)] = 33,
  [SMALL_STATE(7)] = 44,
  [SMALL_STATE(8)] = 55,
  [SMALL_STATE(9)] = 66,
  [SMALL_STATE(10)] = 77,
  [SMALL_STATE(11)] = 88,
  [SMALL_STATE(12)] = 98,
  [SMALL_STATE(13)] = 108,
  [SMALL_STATE(14)] = 115,
  [SMALL_STATE(15)] = 119,
  [SMALL_STATE(16)] = 123,
  [SMALL_STATE(17)] = 127,
  [SMALL_STATE(18)] = 131,
  [SMALL_STATE(19)] = 135,
  [SMALL_STATE(20)] = 139,
  [SMALL_STATE(21)] = 143,
  [SMALL_STATE(22)] = 147,
  [SMALL_STATE(23)] = 151,
  [SMALL_STATE(24)] = 155,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [17] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(13),
  [20] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(15),
  [23] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(24),
  [26] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(23),
  [29] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(24),
  [32] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_declaration, 5, .production_id = 2),
  [36] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_declaration, 5, .production_id = 2),
  [38] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_declaration, 4, .production_id = 1),
  [40] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_declaration, 4, .production_id = 1),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_declaration, 4, .production_id = 2),
  [44] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_declaration, 4, .production_id = 2),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_variable_declaration, 3, .production_id = 1),
  [48] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_variable_declaration, 3, .production_id = 1),
  [50] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration_statement, 1),
  [52] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_declaration_statement, 1),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_statement, 2),
  [56] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression_statement, 2),
  [58] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [62] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [64] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [66] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [72] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [80] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_initializer, 2),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_literal, 1),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_oxidescript(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
