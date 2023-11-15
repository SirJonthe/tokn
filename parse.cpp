#include "parse.h"

#include <iostream>

struct parser
{
	int               token_index_states[256];
	int               current_token_state;
	const token      *tokens;
	int               token_index;
	int               node_index_states[256];
	node              nodes[256]; // The AST
	int               current_node_state;
	int               node_index;
	int               max_tokens;
	parse_callbacks   callbacks;
	lexer            *l;
	token           (*lex_fn)(lexer*);
	bool              debug_text;
};

static bool debug_print(parser *p, const char *text)
{
	if (p->debug_text) {
		for (int i = 0; i < p->current_token_state; ++i) {
			std::cout << " |";
		}
		std::cout << text << std::endl;
	}
	return true;
}

static bool debug_print_fn(parser *p, const char *fn_name, const char *msg)
{
	if (p->debug_text) {
		for (int i = 0; i < p->current_token_state; ++i) {
			std::cout << " |";
		}
		std::cout << fn_name << ": " << msg << std::endl;
	}
	return true;
}

static bool debug_match_print(parser *p, unsigned expected, unsigned got, const char *chars, const char *msg)
{
	if (p->debug_text) {
		for (int i = 0; i < p->current_token_state; ++i) {
			std::cout << " |";
		}
		std::cout << "expect=" << expected << ", got=" << got << "(" << chars << "): " << msg << std::endl;
	}
	return true;
}

#define DEBUG_PRINT(text) debug_print(p, text)

static bool save_state(parser *p)
{
	if (p->current_token_state >= 255) { return false; }
	p->token_index_states[p->current_token_state] = p->token_index;
	++p->current_token_state;
	return true;
}

static void restore_state(parser *p)
{
	if (p->current_token_state <= 0) { return; }
	--p->current_token_state;
	p->token_index = p->token_index_states[p->current_token_state];
}

static void commit_state(parser *p)
{
	if (p->current_token_state <= 0) { return; }
	for (int i = 0; i < p->current_token_state; ++i) {
		std::cout << "  ";
	}
	for (int i = p->token_index_states[p->current_token_state - 1]; i < p->token_index; ++i) {
		std::cout << p->tokens[i].chars << "|";
	}
	std::cout << std::endl;
	--p->current_token_state;
}

static bool manage_state(const char *fn_name, parser *p, bool success)
{
	if (!success) {
		restore_state(p);
		debug_print_fn(p, fn_name, "no match");
	} else {
		commit_state(p);
		debug_print_fn(p, fn_name, "match");
	}
	return success;
}

#define MANAGE_STATE(fn_name, success) (debug_print(p, fn_name) && save_state(p) && manage_state(fn_name, p, success))

static bool parse_empty        (parser *p, unsigned end);
static bool parse_program      (parser *p, unsigned end);
static bool parse_body         (parser *p, unsigned end);
static bool parse_stmt_list    (parser *p, unsigned end);
static bool parse_stmt         (parser *p, unsigned end);
static bool parse_typename     (parser *p, unsigned end);
static bool parse_var_sig      (parser *p, unsigned end);
static bool parse_var_sig_list (parser *p, unsigned end);
static bool parse_decl_var_stmt(parser *p, unsigned end);
static bool parse_param_list   (parser *p, unsigned end);
static bool parse_func_sig     (parser *p, unsigned end);
static bool parse_def_func_stmt(parser *p, unsigned end);
static bool parse_assign_op    (parser *p, unsigned end);
static bool parse_assign_stmt  (parser *p, unsigned end);
static bool parse_if_stmt      (parser *p, unsigned end);
static bool parse_return_stmt  (parser *p, unsigned end);
static bool parse_expr_stmt    (parser *p, unsigned end);
static bool parse_expr_list    (parser *p, unsigned end);
static bool parse_expr         (parser *p, unsigned end);
static bool parse_term         (parser *p, unsigned end);
static bool parse_opt_term     (parser *p, unsigned end);
static bool parse_func_call    (parser *p, unsigned end);
static bool parse_factor       (parser *p, unsigned end);
static bool parse_opt_factor   (parser *p, unsigned end);

static bool match(parser *p, unsigned type)
{
	if (p->token_index >= p->max_tokens) { return false; }
	unsigned t = p->tokens[p->token_index].user_type;
	if (p->tokens[p->token_index].type != token::STOP && type == t) {
		debug_match_print(p, type, t, p->tokens[p->token_index].chars, "match");
		++p->token_index;
		return true;
	}
	debug_match_print(p, type, t, p->tokens[p->token_index].chars, "no match");
	return false;
}

static bool peek(parser *p, unsigned type)
{
	return p->tokens[p->token_index].user_type == type;
}

static bool scan_scope(unsigned open, bool (*parse_fn)(parser*,unsigned), unsigned close, parser *p)
{
	DEBUG_PRINT("scan_scope");

	if (!match(p, open)) {
		return false;
	}
	while (!match(p, close)) {
		if (!parse_fn(p, close)) {
			return false;
		}
	}
	return true;
}

// ε
static bool parse_empty(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_empty",
		peek(p, end)
	);
}

// program ::= def_func_stmt
static bool parse_program(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_program",
		parse_def_func_stmt(p, end)
	);
}

// body ::= "{" stmt_list "}"
static bool parse_body(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_body",
		scan_scope(ctoken::OPERATOR_ENCLOSE_BRACE_L, parse_stmt_list, ctoken::OPERATOR_ENCLOSE_BRACE_R, p)
	);
}

// stmt_list ::= stmt stmt_list | ε
static bool parse_stmt_list(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_stmt_list",
		(parse_stmt(p, end) && parse_stmt_list(p, end))
		|| parse_empty(p, end)
	);
}

// stmt ::= decl_var_stmt | assign_stmt | return_stmt | if_stmt | expr_stmt
static bool parse_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_stmt",
		parse_body(p, end)
		|| parse_decl_var_stmt(p, end)
		|| parse_assign_stmt(p, end)
		|| parse_return_stmt(p, end)
		|| parse_if_stmt(p, end)
		|| parse_expr_stmt(p, end)
	);
}

// typename ::= "int" | "float"
static bool parse_typename(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_typename",
		match(p, ctoken::KEYWORD_TYPE_INT)
		|| match(p, ctoken::KEYWORD_TYPE_FLOAT)
	);
}

// var_sig ::= typename IDENTIFIER
static bool parse_var_sig(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_var_sig",
		parse_typename(p, end) && match(p, token::ALIAS)
	);
}

// var_sig_list ::= var_sig "," var_sig_list | var_sig | ε
static bool parse_var_sig_list(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_var_sig_list",
		(parse_var_sig(p, end) && match(p, ctoken::OPERATOR_COMMA) && parse_var_sig_list(p, end))
	)
	|| MANAGE_STATE(
		"parse_var_sig_list",
		parse_var_sig(p, end)
		|| parse_empty(p, end)
	);
}

// decl_var_stmt ::= var_sig ";"
static bool parse_decl_var_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_decl_var_stmt",
		parse_var_sig(p, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// param_list ::= var_sig "," param_list | var_sig | ε
static bool parse_param_list(parser *p, unsigned end)
{
	return
		MANAGE_STATE(
			"parse_param_list",
			parse_var_sig(p, end) && match(p, ctoken::OPERATOR_COMMA) && parse_param_list(p, end)
		)
		|| MANAGE_STATE(
			"parse_param_list",
			parse_var_sig(p, end)
			|| parse_empty(p, end)
		);
}

// func_sig ::= var_sig "(" param_list ")"
static bool parse_func_sig(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_func_sig",
		parse_var_sig(p, end) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_param_list, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p)
	);
}

// def_func_stmt ::= func_sig body
static bool parse_def_func_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_def_func_stmt",
		parse_func_sig(p, end) && parse_body(p, end)
	);
}

// assign_op ::= "="
static bool parse_assign_op(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_assign_op",
		match(p, ctoken::OPERATOR_ASSIGNMENT_SET)
	);
}

// assign_stmt ::= IDENTIFIER "=" expr ";"
static bool parse_assign_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_assign_stmt",
		match(p, token::ALIAS) && parse_assign_op(p, end) && parse_expr(p, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// if_stmt ::= "if" "(" expr ")" stmt
static bool parse_if_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_if_stmt",
		match(p, ctoken::KEYWORD_CONTROL_IF) && match(p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L) && parse_expr(p, end) && match(p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R) && parse_stmt(p, end)
	);
}

// return_stmt ::= "return" expr_stmt | "return" ";"
static bool parse_return_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_return_stmt",
		match(p, ctoken::KEYWORD_CONTROL_RETURN) && (parse_expr_stmt(p, end) || match(p, ctoken::OPERATOR_SEMICOLON))
	);
}

// expr_stmt ::= expr ";"
static bool parse_expr_stmt(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_expr_stmt",
		parse_expr(p, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// expr_list ::= expr "," expr_list | expr | ε
static bool parse_expr_list(parser *p, unsigned end)
{
	return
		MANAGE_STATE(
			"parse_expr_list",
			parse_expr(p, end) && parse_empty(p, end)
		)
		|| MANAGE_STATE(
			"parse_expr_list",
			parse_expr(p, end) && match(p, ctoken::OPERATOR_COMMA) && parse_expr_list(p, end)
		);
}

// expr ::= term opt_term
static bool parse_expr(parser *p, unsigned end)
{
	return
		MANAGE_STATE(
			"parse_expr",
			parse_term(p, end) && parse_opt_term(p, end)
		);
}

// term ::= factor opt_factor
static bool parse_term(parser *p, unsigned end)
{
	return
		MANAGE_STATE(
			"parse_term",
			parse_factor(p, end) && parse_opt_factor(p, end)
		);
}

// opt_term ::= (("+" | "-") term)*
static bool parse_opt_term(parser *p, unsigned end)
{
	while (match(p, ctoken::OPERATOR_ARITHMETIC_ADD) || match(p, ctoken::OPERATOR_ARITHMETIC_SUB)) {
		if (!MANAGE_STATE("parse_opt_term", parse_term(p, end))) {
			return false;
		}
	}
	return true;
}

// factor ::= func_call | IDENTIFIER | LITERAL | "(" expr ")"
static bool parse_factor(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_factor",
		parse_func_call(p, end)
		|| match(p, token::ALIAS)
		|| match(p, ctoken::LITERAL_INT)
		|| scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p)
	);
}

// opt_factor ::= (("*" | "/" | "%") factor)*
static bool parse_opt_factor(parser *p, unsigned end)
{
	while (match(p, ctoken::OPERATOR_ARITHMETIC_MUL) || match(p, ctoken::OPERATOR_ARITHMETIC_DIV) || match(p, ctoken::OPERATOR_ARITHMETIC_MOD)) {
		if (!MANAGE_STATE("parse_opt_term", parse_factor(p, end))) {
			return false;
		}
	}
	return true;
}

// func_call ::= IDENTIFIER "(" expr_list ")"
static bool parse_func_call(parser *p, unsigned end)
{
	return MANAGE_STATE(
		"parse_func_call",
		match(p, token::ALIAS) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr_list, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p)
	);
}

parse_callbacks new_callbacks( void )
{
	return parse_callbacks{
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr,
		nullptr
	};
}

bool parse(int max_tokens, const token *tokens, parse_callbacks cb, bool debug_text)
{
	parser ps;
	ps.token_index         = 0;
	ps.current_token_state = 0;
	ps.node_index          = 0;
	ps.current_node_state  = 0;
	ps.max_tokens          = max_tokens;
	ps.tokens              = tokens;
	ps.callbacks           = cb;
	ps.debug_text          = debug_text;
	parser *p = &ps;
	return MANAGE_STATE("parse", parse_program(p, token::STOP_EOF));
}
