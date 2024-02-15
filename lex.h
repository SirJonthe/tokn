#ifndef LEX_H
#define LEX_H

struct chars
{
	char str[32];
	struct view
	{
		const char *str;
		unsigned    len;
		unsigned    page;
	};
};

struct token
{
	enum tokentype
	{
		KEYWORD = 1<<12,
		ALIAS = 2<<12,
		OPERATOR = 3<<12,
		LITERAL = 5<<12,
		STOP = 6<<12,
			STOP_ERR,
			STOP_EOF,
		
		TYPEMASK0 = 0xF000,
		TYPEMASK1 = 0xFF00,
		TYPEMASK2 = 0xFFF0
	};

	chars     text; // NOTE: For string literals, just split it up into chunks of 31 characters each.
	unsigned  hash;
	tokentype type;
	unsigned  user_type;
	unsigned  (*hashfn)(const char*,unsigned);
	unsigned  head, row, col;
};

struct ctoken
{
	enum tokentype
	{
		KEYWORD_TYPE = token::KEYWORD | (1<<8),
			
			// NOTE: This order must be preserved due to how C always casts operations with mixed types UP the ladder in the way below.
			
			KEYWORD_TYPE_VOID,
			//KEYWORD_TYPE_CHAR,
			//KEYWORD_TYPE_UCHAR,
			KEYWORD_TYPE_SIGNED,
			KEYWORD_TYPE_INT = KEYWORD_TYPE_SIGNED,
			KEYWORD_TYPE_UNSIGNED,
			KEYWORD_TYPE_FLOAT,

			//KEYWORD_TYPE_ENUM,
			//KEYWORD_TYPE_STRUCT,
			//KEYWORD_TYPE_TYPEDEF,
			//KEYWORD_TYPE_UNION,
		KEYWORD_CONTROL = token::KEYWORD | (2<<8),
			KEYWORD_CONTROL_RETURN,
			KEYWORD_CONTROL_IF,
			KEYWORD_CONTROL_ELSE,
		KEYWORD_INTRINSIC = token::KEYWORD | (3<<8),
			KEYWORD_INTRINSIC_ASM,

		ALIAS_VAR = token::ALIAS | (1<<8),
		ALIAS_FUNC = token::ALIAS | (2<<8),
		//ALIAS_TYPE, // the name of the type, not the instantiated variable (the instantiated variable would be an ALIAS_VAR)

		OPERATOR_ARITHMETIC = token::OPERATOR | (1<<8),
			OPERATOR_ARITHMETIC_ADD,
			OPERATOR_ARITHMETIC_SUB,
			OPERATOR_ARITHMETIC_MUL,
			OPERATOR_ARITHMETIC_DIV,
			OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ASSIGNMENT = token::OPERATOR | (2<<8),
			//OPERATOR_ASSIGNMENT_ADD,
			//OPERATOR_ASSIGNMENT_SUB,
			//OPERATOR_ASSIGNMENT_MUL,
			//OPERATOR_ASSINGMENT_DIV,
			//OPERATOR_ASSIGNMENT_MOD,
			OPERATOR_ASSIGNMENT_SET,
		OPERATOR_ENCLOSE = token::OPERATOR | (3<<8),
			OPERATOR_ENCLOSE_PARENTHESIS = OPERATOR_ENCLOSE | (1<<4),
				OPERATOR_ENCLOSE_PARENTHESIS_L,
				OPERATOR_ENCLOSE_PARENTHESIS_R,
			OPERATOR_ENCLOSE_BRACKET = OPERATOR_ENCLOSE | (2<<4),
				OPERATOR_ENCLOSE_BRACKET_L,
				OPERATOR_ENCLOSE_BRACKET_R,
			OPERATOR_ENCLOSE_BRACE = OPERATOR_ENCLOSE | (3<<4),
				OPERATOR_ENCLOSE_BRACE_L,
				OPERATOR_ENCLOSE_BRACE_R,
		OPERATOR_SEMICOLON = token::OPERATOR | 1,
		OPERATOR_COMMA,

		LITERAL_INT = token::LITERAL | (1<<8),
		//LITERAL_FLOAT = token::LITERAL | (2<<8),
	};
};

struct lexer
{
	chars::view code;
	unsigned    head;
	unsigned    row;
	unsigned    col;
	unsigned    page;
	chars::view (*load_page)(unsigned);
};

token new_keyword (const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_operator(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_literal (const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_alias   (const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_eof     ( void );
token new_error   (const char *chars, unsigned char_count);

/// @brief Creates a new lexer from the given code.
/// @param code The code to load.
/// @return The newly created lexer.
lexer init_lexer(chars::view code);

/// @brief Generic lexer that tokenizes the next segment of code in the parser.
/// @param l The lexer/tokenizer;
/// @param tokens The tokens to match the code against.
/// @param num_tokens The number of tokens to match the code against.
/// @return The token read. The token will contain status to determine if there was an error.
token lex(lexer *l, const token *tokens, signed num_tokens);

/// @brief C programming language lexer that tokenizes the next segment of code in the parser.
/// @param l The lexer/tokenizer;
token clex(lexer *l);

/// @brief Returns the number of tokens remaining in the given lexer.
/// @param lex_fn Lexer function.
/// @param l The lexer.
/// @return The number of tokens remaining in the lexer. Negative number if there was an error in the lexing.
/// @note EOF counts as one token, so even if the lexer is exhausted, the count should be 1 if there was no error.
/// @warning This process lexes through the entire remaining code which may be very slow.
signed count_tokens(token (*lex_fn)(lexer*), lexer l);

/// @brief Returns the number of tokens remaining in the lexer given C language tokens.
/// @param l The lexer.
/// @return The number of tokens remaining in the lexer. Negative numbers if there was an error in the lexing.
/// @note EOF counts as one token, so even if the lexer is exhausted, the count should be 1 if there was no error.
/// @warning This process lexes through the entire remaining code which may be very slow.
signed count_ctokens(lexer l);

#endif
