#include "lex.h"

/* tiny-regex-c
 * https://github.com/kokke/tiny-regex-c
 * Unlicense
 *
 * Mini regex-module inspired by Rob Pike's regex code described in:
 *
 * http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
 *
 * Modified for use in this package.
 *
 * Supports:
 * ---------
 *   '.'        Dot, matches any character
 *   '^'        Start anchor, matches beginning of string
 *   '$'        End anchor, matches end of string
 *   '*'        Asterisk, match zero or more (greedy)
 *   '+'        Plus, match one or more (greedy)
 *   '?'        Question, match zero or one (non-greedy)
 *   '[abc]'    Character class, match if one of {'a', 'b', 'c'}
 *   '[a-zA-Z]' Character ranges, the character set of the ranges { a-z | A-Z }
 *   '\s'       Whitespace, \t \f \r \n \v and spaces
 *   '\S'       Non-whitespace
 *   '\w'       Alphanumeric, [a-zA-Z0-9_]
 *   '\W'       Non-alphanumeric
 *   '\d'       Digits, [0-9]
 *   '\D'       Non-digits
 */

#include <stdio.h>
#include <ctype.h>

/* Definitions: */
#define MAX_REGEXP_OBJECTS 30 /* Max number of regex symbols in expression. */
#define MAX_CHAR_CLASS_LEN 40 /* Max length of character-class buffer in.   */

enum { UNUSED, DOT, BEGIN, END, QUESTIONMARK, STAR, PLUS, CHAR, CHAR_CLASS, INV_CHAR_CLASS, DIGIT, NOT_DIGIT, ALPHA, NOT_ALPHA, WHITESPACE, NOT_WHITESPACE, /* BRANCH */ };

typedef struct regex_t
{
	unsigned char type;   /* CHAR, STAR, etc. */
	union
	{
		unsigned char  ch;  /*     The character itself             */
		unsigned char *ccl; /* OR  a posigneder to characters in class */
	} u;
} regex_t;

/* Private function declarations: */
static signed   re_match(const char *pattern, const char *text, signed textlength, signed *matchlength);
static signed   re_matchp(regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static regex_t *re_compile(const char *pattern);
static void     re_prsigned(regex_t *pattern);
static signed   matchpattern(regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchcharclass(char c, const char* str);
static signed   matchstar(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchplus(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchone(regex_t p, char c);
static signed   matchdigit(char c);
static signed   matchalpha(char c);
static signed   matchwhitespace(char c);
static signed   matchmetachar(char c, const char *str);
static signed   matchrange(char c, const char *str);
static signed   matchdot(char c);
static signed   ismetachar(char c);

/* Public functions: */
static signed re_match(const char *pattern, const char *text, signed textlength, signed *matchlength)
{
	return re_matchp(re_compile(pattern), text, textlength, matchlength);
}

static signed re_matchp(regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	*matchlength = 0;
	if (pattern != 0) {
		if (pattern[0].type == BEGIN) {
			return ((matchpattern(&pattern[1], text, textlength, matchlength)) ? 0 : -1);
		} else {
			signed idx = -1;

			do {
				idx += 1;

				if (matchpattern(pattern, text, textlength, matchlength)) {
					if (text[0] == '\0') {
						return -1;
					}

					return idx;
				}
			} while (*text++ != '\0' && --textlength > 0);
		}
	}
	return -1;
}

static regex_t* re_compile(const char *pattern)
{
	/* The sizes of the two static arrays below substantiates the static RAM usage of this module.
		MAX_REGEXP_OBJECTS is the max number of symbols in the expression.
		MAX_CHAR_CLASS_LEN determines the size of buffer for chars in all char-classes in the expression. */
	static regex_t re_compiled[MAX_REGEXP_OBJECTS];
	static unsigned char ccl_buf[MAX_CHAR_CLASS_LEN];
	signed ccl_bufidx = 1;

	char c;     /* current char in pattern   */
	signed i = 0;  /* index signedo pattern        */
	signed j = 0;  /* index signedo re_compiled    */

	while (pattern[i] != '\0' && (j+1 < MAX_REGEXP_OBJECTS)) {
		c = pattern[i];

		switch (c) {
			/* Meta-characters: */
			case '^': { re_compiled[j].type = BEGIN;        } break;
			case '$': { re_compiled[j].type = END;          } break;
			case '.': { re_compiled[j].type = DOT;          } break;
			case '*': { re_compiled[j].type = STAR;         } break;
			case '+': { re_compiled[j].type = PLUS;         } break;
			case '?': { re_compiled[j].type = QUESTIONMARK; } break;
		/*    case '|': {    re_compiled[j].type = BRANCH;          } break; <-- not working properly */

			/* Escaped character-classes (\s \w ...): */
			case '\\':
			{
			if (pattern[i+1] != '\0') {
				/* Skip the escape-char '\\' */
				i += 1;
				/* ... and check the next */
				switch (pattern[i]) {
				/* Meta-character: */
				case 'd': { re_compiled[j].type = DIGIT;          } break;
				case 'D': { re_compiled[j].type = NOT_DIGIT;      } break;
				case 'w': { re_compiled[j].type = ALPHA;          } break;
				case 'W': { re_compiled[j].type = NOT_ALPHA;      } break;
				case 's': { re_compiled[j].type = WHITESPACE;     } break;
				case 'S': { re_compiled[j].type = NOT_WHITESPACE; } break;

				/* Escaped character, e.g. '.' or '$' */
				default:
				{
					re_compiled[j].type = CHAR;
					re_compiled[j].u.ch = pattern[i];
				} break;
				}
			}
			/* '\\' as last char in pattern -> invalid regular expression. */
		/*
			else
			{
				re_compiled[j].type = CHAR;
				re_compiled[j].ch = pattern[i];
			}
		*/
			} break;

			/* Character class: */
			case '[':
			{
				/* Remember where the char-buffer starts. */
				signed buf_begin = ccl_bufidx;

				/* Look-ahead to determine if negated */
				if (pattern[i+1] == '^') {
					re_compiled[j].type = INV_CHAR_CLASS;
					i += 1; /* Increment i to avoid including '^' in the char-buffer */
					if (pattern[i+1] == 0) /* incomplete pattern, missing non-zero char after '^' */
					{
						return 0;
					}
				} else {
					re_compiled[j].type = CHAR_CLASS;
				}

				/* Copy characters inside [..] to buffer */
				while ((pattern[++i] != ']') && (pattern[i]   != '\0')) /* Missing ] */
				{
					if (pattern[i] == '\\') {
						if (ccl_bufidx >= MAX_CHAR_CLASS_LEN - 1) {
							//fputs("exceeded signedernal buffer!\n", stderr);
							return 0;
						}
						if (pattern[i+1] == 0) /* incomplete pattern, missing non-zero char after '\\' */
						{
							return 0;
						}
						ccl_buf[ccl_bufidx++] = pattern[i++];
					} else if (ccl_bufidx >= MAX_CHAR_CLASS_LEN) {
						//fputs("exceeded signedernal buffer!\n", stderr);
						return 0;
					}
					ccl_buf[ccl_bufidx++] = pattern[i];
				}
				if (ccl_bufidx >= MAX_CHAR_CLASS_LEN) {
					/* Catches cases such as [00000000000000000000000000000000000000][ */
					//fputs("exceeded signedernal buffer!\n", stderr);
					return 0;
				}
				/* Null-terminate string end */
				ccl_buf[ccl_bufidx++] = 0;
				re_compiled[j].u.ccl = &ccl_buf[buf_begin];
			} break;

			/* Other characters: */
			default:
			{
				re_compiled[j].type = CHAR;
				re_compiled[j].u.ch = c;
			} break;
		}
		/* no buffer-out-of-bounds access on invalid patterns - see https://github.com/kokke/tiny-regex-c/commit/1a279e04014b70b0695fba559a7c05d55e6ee90b */
		if (pattern[i] == 0) {
			return 0;
		}

		i += 1;
		j += 1;
	}
	/* 'UNUSED' is a sentinel used to indicate end-of-pattern */
	re_compiled[j].type = UNUSED;

	return (regex_t*)re_compiled;
}

static void re_prsigned(regex_t *pattern)
{
	const char *types[] = { "UNUSED", "DOT", "BEGIN", "END", "QUESTIONMARK", "STAR", "PLUS", "CHAR", "CHAR_CLASS", "INV_CHAR_CLASS", "DIGIT", "NOT_DIGIT", "ALPHA", "NOT_ALPHA", "WHITESPACE", "NOT_WHITESPACE", "BRANCH" };

	signed i;
	signed j;
	char c;
	for (i = 0; i < MAX_REGEXP_OBJECTS; ++i) {
		if (pattern[i].type == UNUSED) {
			break;
		}

		printf("type: %s", types[pattern[i].type]);
		if (pattern[i].type == CHAR_CLASS || pattern[i].type == INV_CHAR_CLASS) {
			printf(" [");
			for (j = 0; j < MAX_CHAR_CLASS_LEN; ++j) {
				c = pattern[i].u.ccl[j];
				if ((c == '\0') || (c == ']')) {
					break;
				}
				printf("%c", c);
			}
			printf("]");
		} else if (pattern[i].type == CHAR) {
			printf(" '%c'", pattern[i].u.ch);
		}
		printf("\n");
	}
}

/* Private functions: */
static signed matchdigit(char c)
{
	return isdigit(c);
}
static signed matchalpha(char c)
{
	return isalpha(c);
}
static signed matchwhitespace(char c)
{
	return isspace(c);
}
static signed matchalphanum(char c)
{
	return ((c == '_') || matchalpha(c) || matchdigit(c));
}
static signed matchrange(char c, const char* str)
{
	return (    (c != '-')
			&& (str[0] != '\0')
			&& (str[0] != '-')
			&& (str[1] == '-')
			&& (str[2] != '\0')
			&& (    (c >= str[0])
				&& (c <= str[2])));
}
static signed matchdot(char c)
{
	return c != '\n' && c != '\r';
}
static signed ismetachar(char c)
{
	return ((c == 's') || (c == 'S') || (c == 'w') || (c == 'W') || (c == 'd') || (c == 'D'));
}

static signed matchmetachar(char c, const char *str)
{
	switch (str[0]) {
		case 'd': return  matchdigit(c);
		case 'D': return !matchdigit(c);
		case 'w': return  matchalphanum(c);
		case 'W': return !matchalphanum(c);
		case 's': return  matchwhitespace(c);
		case 'S': return !matchwhitespace(c);
		default:  return (c == str[0]);
	}
}

static signed matchcharclass(char c, const char *str)
{
	do {
		if (matchrange(c, str)) {
			return 1;
		}
		else if (str[0] == '\\') {
			/* Escape-char: increment str-ptr and match on next char */
			str += 1;
			if (matchmetachar(c, str)) {
				return 1;
			}
			else if ((c == str[0]) && !ismetachar(c)) {
				return 1;
			}
		}
		else if (c == str[0]) {
			if (c == '-') {
				return ((str[-1] == '\0') || (str[1] == '\0'));
			}
			else {
				return 1;
			}
		}
	}
	while (*str++ != '\0');

	return 0;
}

static signed matchone(regex_t p, char c)
{
	switch (p.type) {
		case DOT:            return  matchdot(c);
		case CHAR_CLASS:     return  matchcharclass(c, (const char*)p.u.ccl);
		case INV_CHAR_CLASS: return !matchcharclass(c, (const char*)p.u.ccl);
		case DIGIT:          return  matchdigit(c);
		case NOT_DIGIT:      return !matchdigit(c);
		case ALPHA:          return  matchalphanum(c);
		case NOT_ALPHA:      return !matchalphanum(c);
		case WHITESPACE:     return  matchwhitespace(c);
		case NOT_WHITESPACE: return !matchwhitespace(c);
		default:             return  (p.u.ch == c);
	}
}

static signed matchstar(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	signed prelen = *matchlength;
	const char *preposigned = text;
	while ((text[0] != '\0') && matchone(p, *text)) {
		text++;
		--textlength;
		(*matchlength)++;
	}
	while (text >= preposigned) {
		if (matchpattern(pattern, text--, textlength + 1, matchlength)) {
			return 1;
		}
		(*matchlength)--;
	}

	*matchlength = prelen;
	return 0;
}

static signed matchplus(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	const char *preposigned = text;
	while ((text[0] != '\0') && matchone(p, *text)) {
		text++;
		--textlength;
		(*matchlength)++;
	}
	while (text > preposigned) {
		if (matchpattern(pattern, text--, textlength + 1, matchlength)) {
			return 1;
		}
		(*matchlength)--;
	}
	return 0;
}

static signed matchquestion(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	if (p.type == UNUSED) {
		return 1;
	}
	if (matchpattern(pattern, text, textlength, matchlength)) {
		return 1;
	}
	if (*text && matchone(p, *text++)) {
		if (matchpattern(pattern, text, textlength - 1, matchlength)) {
			(*matchlength)++;
			return 1;
		}
	}
	return 0;
}

/* Iterative matching */
static signed matchpattern(regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	signed pre = *matchlength;
	do {
		if ((pattern[0].type == UNUSED) || (pattern[1].type == QUESTIONMARK)) {
			return matchquestion(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if (pattern[1].type == STAR) {
			return matchstar(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if (pattern[1].type == PLUS) {
			return matchplus(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if ((pattern[0].type == END) && pattern[1].type == UNUSED) {
			return (text[0] == '\0') || textlength == 1;
		}
		/*  Branching is not working properly
		else if (pattern[1].type == BRANCH)
		{
			return (matchpattern(pattern, text) || matchpattern(&pattern[2], text));
		}
		*/
		(*matchlength)++;
	} while ((text[0] != '\0') && matchone(*pattern++, *text++) && --textlength >= 0);

	*matchlength = pre;
	return 0;
}

static unsigned init_hash_ch( void )
{
	return unsigned(0xcbf29ce484222325ULL);
}

static unsigned chhash(unsigned h, char ch)
{
	h ^= unsigned(ch);
	h *= unsigned(0x100000001b3ULL);
	return h;
};

static unsigned strhash(const char *str, unsigned len)
{
	unsigned h = init_hash_ch();
	for (signed i = 0; i < len; ++i) {
		h = chhash(h, str[i]);
	}
	return h;
}

static unsigned numhash(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 0; i < len; ++i) {
		h = h  * 10 + nums[i] - '0';
	}
	return h;
}

static token new_token(const char *chars, unsigned char_count, token::tokentype type, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr)
{
	token t;

	const unsigned size = sizeof(t.text.str) - 1 < char_count ? sizeof(t.text.str) - 1 : char_count;
	for (unsigned i = 0; i < size; ++i)                  { t.text.str[i] = chars[i]; }
	for (unsigned i = size; i < sizeof(t.text.str); ++i) { t.text.str[i] = '\0'; }
	t.hashfn = hashfn;
	if (hashfn != nullptr) {
		t.hash = hashfn(chars, char_count);
	} else {
		t.hash = (type == token::LITERAL) ? numhash(chars, char_count) : strhash(chars, char_count);
	}
	t.type = type;
	t.user_type = user_type;
	t.head = t.row = t.col = t.index = 0;
	return t;
}

token new_keyword(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::KEYWORD, user_type, hashfn);
}

token new_operator(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::OPERATOR, user_type, hashfn);
}

token new_literal(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	// NOTE: Requires custom hash functions to hash hex and octal (and other bases) correctly.
	// NOTE: Will not handle floating point numbers correctly as the . will separate the numbers into two parts.
	return new_token(chars, char_count, token::LITERAL, user_type, hashfn);
}

token new_alias(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::ALIAS, user_type, hashfn);
}

token new_eof( void )
{
	return new_token("", 0, token::STOP, token::STOP_EOF);
}

token new_error(const char *chars, unsigned char_count)
{
	return new_token(chars, char_count, token::STOP, token::STOP_ERR);
}

static bool scmp(chars::view a, chars::view b)
{
	if (a.len != b.len) { return false; }
	for (signed i = 0; i < a.len; ++i) {
		if (a.str[i] != b.str[i]) { return false; }
	}
	return true;
}

static bool is_num(char c)
{
	return (c >= '0' && c <= '9');
}

static bool is_alpha(char c)
{
	return
		(c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';
}

static bool is_alnum(char c)
{
	return
		is_num(c) ||
		is_alpha(c);
}

static bool is_white(char c)
{
	return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\0';
}

static void next_char(lexer *p)
{
	++p->col;
	switch (p->code.str[p->head]) {
	case '\r':
	case '\n':
		p->col = 0;
		++p->row;
		break;
	}
	++p->head;
	if (p->head >= p->code.len && p->load_page != NULL) {
		p->code = p->load_page(++p->page);
	}
}

static void skip_white(lexer *p)
{
	while (p->head < p->code.len && is_white(p->code.str[p->head])) {
		next_char(p);
	}
}

static token match_token(chars::view s, token::tokentype type, const token *tokens, signed num_tokens)
{
	unsigned h = strhash(s.str, s.len);
	for (signed i = 0; i < num_tokens; ++i) {
		if (tokens[i].type == type && tokens[i].hash == h) {
			return tokens[i];
		}
	}
	return new_error(s.str, s.len);
}

static token get_key(chars::view s, const token *tokens, signed num_tokens)
{
	return match_token(s, token::KEYWORD, tokens, num_tokens);
}

static token get_op(chars::view s, const token *tokens, signed num_tokens)
{
	// NOTE: This does exact matches. Look to cc0::lexer::find_operator for longest match. Will require some restructuring.
	return match_token(s, token::OPERATOR, tokens, num_tokens);
}

static token get_lit(chars::view s, const token *tokens, signed num_tokens)
{
	for (signed i = 0; i < num_tokens; ++i) {
		signed matchlen;
		if (tokens[i].type == token::LITERAL && re_match(tokens[i].text.str, s.str, s.len, &matchlen) == 0 && matchlen == s.len) {
			return new_literal(s.str, s.len, tokens[i].user_type, tokens[i].hashfn);
		}
	}
	return new_error(s.str, s.len);
}

static token get_alias(chars::view s, const token *tokens, signed num_tokens)
{
	for (signed i = 0; i < num_tokens; ++i) {
		signed matchlen;
		if (tokens[i].type == token::ALIAS && re_match(tokens[i].text.str, s.str, s.len, &matchlen) == 0 && matchlen == s.len) {
			return new_alias(s.str, s.len, tokens[i].user_type, tokens[i].hashfn);
		}
	}
	return new_error(s.str, s.len);
}

static token get_eof(chars::view s, const token *tokens, signed num_tokens)
{
	if (s.len > 0) {
		return new_error(s.str, s.len);
	}
	token t;
	t.hash = strhash(s.str, s.len);
	t.text.str[0] = '\0';
	t.type = token::STOP;
	t.user_type = token::STOP_EOF;
	return t;
}

static chars::view read(lexer *p, unsigned &head, unsigned &row, unsigned &col, unsigned &index)
{
	skip_white(p);
	head  = p->head;
	row   = p->row;
	col   = p->col;
	index = p->index;

	char c;
	unsigned s = p->head;
	unsigned i = 0;
	while (i < sizeof(chars::str) - 1 && p->head < p->code.len) {
		c = p->code.str[p->head];
		if (!is_alnum(c)) {
			if (!is_white(c) && s == p->head) {
				next_char(p);
			}
			break;
		}
		next_char(p);
		++i;
	}
	++p->index;
	return chars::view{ p->code.str + s, p->head - s, 0 };
}

static token classify(lexer *p, const token *tokens, signed num_tokens)
{
	token t;
	unsigned head, row, col, index;
	chars::view s = read(p, head, row, col, index);
	const signed GET_COUNT = 5;
	token (*get[GET_COUNT])(chars::view,const token*,signed) = { get_eof, get_lit, get_key, get_op, get_alias };

	for (signed i = 0; i < GET_COUNT; ++i) {
		t = get[i](s, tokens, num_tokens);
		if (t.user_type != token::STOP_ERR) {
			break;
		}
	}
	t.head  = head;
	t.row   = row;
	t.col   = col;
	t.index = index;
	return t;
}

lexer init_lexer(chars::view code)
{
	return lexer{ code, 0, 0, 0, 0, 0, new_error("<no token>", 10), NULL };
}

token lex(lexer *l, const token *tokens, signed max_tokens)
{
	l->last = classify(l, tokens, max_tokens);
	return l->last;
}

static unsigned hex2u(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 2; i < len; ++i) {
		if (nums[i] >= '0' && nums[i] <= '9') {
			h = h  * 16 + nums[i] - '0';
		} else if (nums[i] >= 'a' && nums[i] <= 'f') {
			h = h  * 16 + nums[i] - 'a' + 10;
		} else if (nums[i] >= 'A' && nums[i] <= 'F') {
			h = h  * 16 + nums[i] - 'A' + 10;
		}
	}
	return h;
}

const signed C_TOKEN_COUNT = 25;
const token C_TOKENS[C_TOKEN_COUNT] = {
	new_keyword ("void",                    4, ctoken::KEYWORD_TYPE_VOID),
	new_keyword ("unsigned",                8, ctoken::KEYWORD_TYPE_UNSIGNED),
	new_keyword ("signed",                  6, ctoken::KEYWORD_TYPE_SIGNED),
	new_keyword ("int",                     3, ctoken::KEYWORD_TYPE_INT),
	new_keyword ("return",                  6, ctoken::KEYWORD_CONTROL_RETURN),
	new_keyword ("if",                      2, ctoken::KEYWORD_CONTROL_IF),
	new_keyword ("else",                    4, ctoken::KEYWORD_CONTROL_ELSE),
	new_keyword ("__asm",                   5, ctoken::KEYWORD_INTRINSIC_ASM),
	new_operator("(",                       1, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator("{",                       1, ctoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, ctoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("[",                       1, ctoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, ctoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("+",                       1, ctoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, ctoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, ctoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, ctoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, ctoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("=",                       1, ctoken::OPERATOR_ASSIGNMENT_SET),
	new_operator(",",                       1, ctoken::OPERATOR_COMMA),
	new_operator(";",                       1, ctoken::OPERATOR_SEMICOLON),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, ctoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, ctoken::LITERAL_INT, hex2u)
};

token clex(lexer *l)
{
	return lex(l, C_TOKENS, C_TOKEN_COUNT);
}

signed count_tokens(token (*lex_fn)(lexer*), lexer l)
{
	signed count = 1;
	token t;
	while ((t = lex_fn(&l)).type != token::STOP) {
		++count;
	}
	return t.user_type == token::STOP_EOF ? count : -1;
}

signed count_ctokens(lexer l)
{
	return count_tokens(clex, l);
}
