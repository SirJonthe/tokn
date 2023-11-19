#include "parse.h"

#include <iostream>

struct token_stream
{
	const token *tokens;
	int          index;
	int          max_tokens;
};

struct node_tree
{
	node *nodes;
	int   index;
	int   max_nodes;
};

struct parser
{
	token_stream in;
	node_tree    out;
};

#define DEBUG_PRINT(text) debug_print(p, text)

static void restore_state(parser *p, int prev_token_index, int prev_node_index)
{
	p->in.index = prev_token_index;
	p->out.index = prev_node_index;
}

static int manage_state(const char *fn_name, parser *p, int prev_token_index, int prev_node_index, int success)
{
	if (!success) {
		restore_state(p, prev_token_index, prev_node_index);
	}
	return success;
}

#define MANAGE_STATE(fn_name, success) (manage_state(fn_name, p, prev_token_index, prev_node_index, success))

static int parse_empty        (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_program      (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_body         (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_stmt_list    (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_stmt         (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_typename     (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_var_sig      (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_var_sig_list (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_decl_var_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_param_list   (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_func_sig     (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_def_func_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_assign_op    (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_assign_stmt  (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_if_stmt      (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_return_stmt  (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_expr_stmt    (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_expr_list    (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_expr         (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_term         (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_opt_term     (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_func_call    (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_factor       (parser *p, int prev_token_index, int prev_node_index, unsigned end);
static int parse_opt_factor   (parser *p, int prev_token_index, int prev_node_index, unsigned end);

static bool match(parser *p, unsigned type)
{
	if (p->in.index >= p->in.max_tokens) { return false; }
	unsigned t = p->in.tokens[p->in.index].user_type;
	if (p->in.tokens[p->in.index].type != token::STOP && type == t) {
		++p->in.index;
		return true;
	}
	return false;
}

static bool peek(parser *p, unsigned type)
{
	return p->in.tokens[p->in.index].user_type == type;
}

static int scan_scope(unsigned open, int (*parse_fn)(parser*,int,int,unsigned), unsigned close, parser *p, int prev_token_index, int prev_node_index)
{
	if (!match(p, open)) {
		return 0;
	}
	while (!match(p, close)) {
		if (MANAGE_STATE("scan_scope", parse_fn(p, p->in.index, p->out.index, close)) <= 0) {
			return 0;
		}
	}
	return 1;
}

// ε
static int parse_empty(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"empty",
		peek(p, end)
	);
}

// program ::= def_func_stmt
static int parse_program(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"program",
		parse_def_func_stmt(p, p->in.index, p->out.index, end)
	);
}

// body ::= "{" stmt_list "}"
static int parse_body(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"body",
		scan_scope(ctoken::OPERATOR_ENCLOSE_BRACE_L, parse_stmt_list, ctoken::OPERATOR_ENCLOSE_BRACE_R, p, p->in.index, p->out.index)
	);
}

// stmt_list ::= stmt stmt_list | ε
static int parse_stmt_list(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"stmt_list",
		(parse_stmt(p, p->in.index, p->out.index, end) && parse_stmt_list(p, p->in.index, p->out.index, end))
		|| parse_empty(p, p->in.index, p->out.index, end)
	);
}

// stmt ::= decl_var_stmt | assign_stmt | return_stmt | if_stmt | expr_stmt
static int parse_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"stmt",
		parse_body(p, p->in.index, p->out.index, end)
		|| parse_decl_var_stmt(p, p->in.index, p->out.index, end)
		|| parse_assign_stmt(p, p->in.index, p->out.index, end)
		|| parse_return_stmt(p, p->in.index, p->out.index, end)
		|| parse_if_stmt(p, p->in.index, p->out.index, end)
		|| parse_expr_stmt(p, p->in.index, p->out.index, end)
	);
}

// typename ::= "int" | "float"
static int parse_typename(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"typename",
		match(p, ctoken::KEYWORD_TYPE_INT)
		|| match(p, ctoken::KEYWORD_TYPE_FLOAT)
	);
}

// var_sig ::= typename IDENTIFIER
static int parse_var_sig(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"var_sig",
		parse_typename(p, p->in.index, p->out.index, end) && match(p, token::ALIAS)
	);
}

// var_sig_list ::= var_sig "," var_sig_list | var_sig | ε
static int parse_var_sig_list(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"var_sig_list",
		(parse_var_sig(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_COMMA) && parse_var_sig_list(p, p->in.index, p->out.index, end))
	)
	|| MANAGE_STATE(
		"var_sig_list",
		parse_var_sig(p, p->in.index, p->out.index, end)
		|| parse_empty(p, p->in.index, p->out.index, end)
	);
}

// decl_var_stmt ::= var_sig ";"
static int parse_decl_var_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"decl_var_stmt",
		parse_var_sig(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// param_list ::= var_sig "," param_list | var_sig | ε
static int parse_param_list(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return
		MANAGE_STATE(
			"param_list",
			parse_var_sig(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_COMMA) && parse_param_list(p, p->in.index, p->out.index, end)
		)
		|| MANAGE_STATE(
			"param_list",
			parse_var_sig(p, p->in.index, p->out.index, end)
			|| parse_empty(p, p->in.index, p->out.index, end)
		);
}

// func_sig ::= var_sig "(" param_list ")"
static int parse_func_sig(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"func_sig",
		parse_var_sig(p, p->in.index, p->out.index, end) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_param_list, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p, p->in.index, p->out.index)
	);
}

// def_func_stmt ::= func_sig body
static int parse_def_func_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"def_func_stmt",
		parse_func_sig(p, p->in.index, p->out.index, end) && parse_body(p, p->in.index, p->out.index, end)
	);
}

// assign_op ::= "="
static int parse_assign_op(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"assign_op",
		match(p, ctoken::OPERATOR_ASSIGNMENT_SET)
	);
}

// assign_stmt ::= IDENTIFIER "=" expr ";"
static int parse_assign_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"assign_stmt",
		match(p, token::ALIAS) && parse_assign_op(p, p->in.index, p->out.index, end) && parse_expr(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// if_stmt ::= "if" "(" expr ")" stmt
static int parse_if_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"if_stmt",
		match(p, ctoken::KEYWORD_CONTROL_IF) && match(p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L) && parse_expr(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R) && parse_stmt(p, p->in.index, p->out.index, end)
	);
}

// return_stmt ::= "return" expr_stmt | "return" ";"
static int parse_return_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"return_stmt",
		match(p, ctoken::KEYWORD_CONTROL_RETURN) && (parse_expr_stmt(p, p->in.index, p->out.index, end) || match(p, ctoken::OPERATOR_SEMICOLON))
	);
}

// expr_stmt ::= expr ";"
static int parse_expr_stmt(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"expr_stmt",
		parse_expr(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_SEMICOLON)
	);
}

// expr_list ::= expr "," expr_list | expr | ε
static int parse_expr_list(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return
		MANAGE_STATE(
			"expr_list",
			parse_expr(p, p->in.index, p->out.index, end) && parse_empty(p, p->in.index, p->out.index, end)
		)
		|| MANAGE_STATE(
			"expr_list",
			parse_expr(p, p->in.index, p->out.index, end) && match(p, ctoken::OPERATOR_COMMA) && parse_expr_list(p, p->in.index, p->out.index, end)
		);
}

// expr ::= term opt_term
static int parse_expr(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return
		MANAGE_STATE(
			"expr",
			parse_term(p, p->in.index, p->out.index, end) && parse_opt_term(p, p->in.index, p->out.index, end)
		);
}

// term ::= factor opt_factor
static int parse_term(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return
		MANAGE_STATE(
			"term",
			parse_factor(p, p->in.index, p->out.index, end) && parse_opt_factor(p, p->in.index, p->out.index, end)
		);
}

// opt_term ::= (("+" | "-") term)*
static int parse_opt_term(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	while (match(p, ctoken::OPERATOR_ARITHMETIC_ADD) || match(p, ctoken::OPERATOR_ARITHMETIC_SUB)) {
		if (!MANAGE_STATE("opt_term", parse_term(p, p->in.index, p->out.index, end))) {
			return 0;
		}
	}
	return 1;
}

// factor ::= func_call | IDENTIFIER | LITERAL | "(" expr ")"
static int parse_factor(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"factor",
		parse_func_call(p, p->in.index, p->out.index, end)
		|| match(p, token::ALIAS)
		|| match(p, ctoken::LITERAL_INT)
		|| scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p, p->in.index, p->out.index)
	);
}

// opt_factor ::= (("*" | "/" | "%") factor)*
static int parse_opt_factor(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	while (match(p, ctoken::OPERATOR_ARITHMETIC_MUL) || match(p, ctoken::OPERATOR_ARITHMETIC_DIV) || match(p, ctoken::OPERATOR_ARITHMETIC_MOD)) {
		if (!MANAGE_STATE("opt_term", parse_factor(p, p->in.index, p->out.index, end))) {
			return 0;
		}
	}
	return 1;
}

// func_call ::= IDENTIFIER "(" expr_list ")"
static int parse_func_call(parser *p, int prev_token_index, int prev_node_index, unsigned end)
{
	return MANAGE_STATE(
		"func_call",
		match(p, token::ALIAS) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr_list, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R, p, p->in.index, p->out.index)
	);
}

int parsec(int max_tokens, const token *tokens, int max_nodes, node *nodes)
{
	parser ps;
	
	//ps.in.current_state = 0;
	ps.in.tokens = tokens;
	ps.in.index = 0;
	ps.in.max_tokens = max_tokens;

	//ps.out.current_state = 0;
	ps.out.nodes = nodes;
	ps.out.index = 0;
	ps.out.max_nodes = max_nodes;

	parser *p = &ps;
	int prev_token_index = 0;
	int prev_node_index = 0;

	return MANAGE_STATE("parse", parse_program(p, p->in.index, p->out.index, token::STOP_EOF)) <= 0 ? -1 : ps.out.index;
}
