#ifndef LEX_H
#define LEX_H

/// @brief Short string data representing the largest acceptable token (32 characters). This structure owns its own data.
struct chars
{
	char str[32];

	/// @brief A view into string data. This structure has no ownership of the underlying string data - only references it.
	struct view
	{
		const char *str;
		unsigned    len;
		unsigned    page;
	};
};

/// @brief Converts a C string into a fixed-length chars structure. Maximum length is 31 characters. Anything bigger will be clipped.
/// @param str The string.
/// @param len The length of the string.
/// @return The chars object.
chars to_chars(const char *str, unsigned len);

/// @brief A data structure representing token data that the lexer produces.
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

	chars     text;                            // The token text (hard copy). NOTE: For string literals, just split it up into chunks of 31 characters each.
	unsigned  hash;                            // A hash of the token string. Numeric literals are hashed into a binary 1:1 representation of the human-readable string.
	tokentype type;                            // The broad group type of the token.
	unsigned  user_type;                       // The custom type that this token should have.
	unsigned  head;                            // The character location in the code of this token.
	unsigned  row;                             // The row/line location in the code of this token.
	unsigned  col;                             // The column location in the code of this token.
	unsigned  index;                           // The sequential token index in the code of this token.
	unsigned  (*hashfn)(const char*,unsigned); // A custom hash function to use to hash the token string into a hash.
};

/// @brief Token data specific to the C programming language.
struct ctoken
{
	/// @brief Token types specific to the C programming language.
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
			KEYWORD_CONTROL_WHILE,
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
		OPERATOR_COLON,
		OPERATOR_COMMA,

		LITERAL_INT = token::LITERAL | (1<<8),
		//LITERAL_FLOAT = token::LITERAL | (2<<8),
	};
};

/// @brief The lexer data structure which keeps track of the location of the location of the code being read as well as streams new data in when needed.
struct lexer
{
	chars::view code;                   // The code to lex. This could be a smaller segment of a larger code blob, which can be streamed in using 'load_page' function.
	unsigned    head;                   // The index location of the character in the code about to be lexed.
	unsigned    row;                    // The row/line index in the code that is about to be lexed.
	unsigned    col;                    // The column index in the code that is about to be lexed.
	unsigned    index;                  // The sequence index of the token that is about to be lexed.
	unsigned    page;                   // The current page that is being lexed.
	token       last;                   // The last token lexed.
	chars::view (*load_page)(unsigned); // The function to use to stream more code into into the code buffer when 'head' has reached the end of the code.
};

/// @brief Creates a new keyword token that can be used as to identify other keywords when lexing code.
/// @param chars The characters representing the token. Can be a regular expression.
/// @param char_count The number of characters in the 'chars' string.
/// @param user_type The custom type of this token.
/// @param hashfn A custom hash function to use when matching against this token.
/// @return The resulting token.
token new_keyword(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);

/// @brief Creates a new operator token that can be used as to identify other operators when lexing code.
/// @param chars The characters representing the token. Can be a regular expression.
/// @param char_count The number of characters in the 'chars' string.
/// @param user_type The custom type of this token.
/// @param hashfn A custom hash function to use when matching against this token.
/// @return The resulting token.
token new_operator(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);

/// @brief Creates a new literal token that can be used as to identify other literals when lexing code.
/// @param chars The characters representing the token. Should be a regular expression, otherwise a separate literal token must be created for each valid literal (1,2,3,4 etc.).
/// @param char_count The number of characters in the 'chars' string.
/// @param user_type The custom type of this token.
/// @param hashfn A custom hash function to use when matching against this token.
/// @return The resulting token.
token new_literal(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);

/// @brief Creates a new alias token that can be used as to identify other aliases when lexing code.
/// @param chars The characters representing the token. Should be a regular expression, otherwise a separate alias token must be created for each valid alias.
/// @param char_count The number of characters in the 'chars' string.
/// @param user_type The custom type of this token.
/// @param hashfn A custom hash function to use when matching against this token.
/// @return The resulting token.
token new_alias(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);

/// @brief Creates an end of file token. Not really used to set up tokens to match input code against, but rather emitted when EOF is reached in input code. Can also be used when supplying an array of tokens to parse to indicate where to stop parsing.
/// @return The resulting token.
token new_eof( void );

/// @brief Creates a new error token. Not really used to set up tokens to match input code against, but rather mainly used to emit errors while lexing.
/// @param chars The text of the error.
/// @param char_count The number of characters in 'chars'.
/// @return The resulting token.
token new_error(const char *chars, unsigned char_count);

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
