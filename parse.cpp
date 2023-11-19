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

struct parser_state
{
	parser   *p;
	int       token_index;
	int       node_index;
	unsigned  end;
};

node *new_node(signed *index, parser *p, unsigned type)
{
	*index = p->out.index;
	node *n = p->out.nodes + p->out.index++;
	n->type = type;
	return n;
}

parser_state new_state(parser *p, unsigned end)
{
	parser_state s = { p, p->in.index, p->out.index, end };
	return s;
}

static int manage_state(const char *fn_name, parser_state ps, int success)
{
	if (!success) {
		ps.p->in.index  = ps.token_index;
		ps.p->out.index = ps.node_index;
	}
	return success;
}

#define MANAGE_STATE(fn_name, success) (manage_state(fn_name, ps, success))

static int parse_empty        (parser_state ps);
static int parse_program      (parser_state ps);
static int parse_body         (parser_state ps);
static int parse_stmt_list    (parser_state ps);
static int parse_stmt         (parser_state ps);
static int parse_typename     (parser_state ps);
static int parse_var_sig      (parser_state ps);
static int parse_var_sig_list (parser_state ps);
static int parse_decl_var_stmt(parser_state ps);
static int parse_param_list   (parser_state ps);
static int parse_func_sig     (parser_state ps);
static int parse_def_func_stmt(parser_state ps);
static int parse_assign_op    (parser_state ps);
static int parse_assign_stmt  (parser_state ps);
static int parse_if_stmt      (parser_state ps);
static int parse_return_stmt  (parser_state ps);
static int parse_expr_stmt    (parser_state ps);
static int parse_expr_list    (parser_state ps);
static int parse_expr         (parser_state ps);
static int parse_term         (parser_state ps);
static int parse_opt_term     (parser_state ps);
static int parse_func_call    (parser_state ps);
static int parse_factor       (parser_state ps);
static int parse_opt_factor   (parser_state ps);

static bool match(parser *p, unsigned type)
{
	if (p->in.index >= p->in.max_tokens || p->in.tokens[p->in.index].type == token::STOP) {
		return type == token::STOP_EOF;
	}
	if (type == p->in.tokens[p->in.index].user_type) {
		++p->in.index;
		return true;
	}
	return false;
}

static bool peek(parser *p, unsigned type)
{
	return p->in.tokens[p->in.index].user_type == type;
}

static int scan_scope(unsigned open, int (*parse_fn)(parser_state), parser_state ps)
{
	if (!match(ps.p, open)) {
		return 0;
	}
	while (!match(ps.p, ps.end)) {
		if (MANAGE_STATE("scan_scope", parse_fn(new_state(ps.p, ps.end))) == 0) {
			return 0;
		}
	}
	return 1;
}

// ε
static int parse_empty(parser_state ps)
{
	return MANAGE_STATE(
		"empty",
		peek(ps.p, ps.end)
	);
}

// program ::= def_func_stmt
static int parse_program(parser_state ps)	
{
	while (match(ps.p, ps.end) == 0) {
		if (MANAGE_STATE("program", parse_def_func_stmt(new_state(ps.p, ps.end))) == 0) {
			return 0;
		}
	}
	return 1;
}

// body ::= "{" stmt_list "}"
static int parse_body(parser_state ps)
{
	return MANAGE_STATE(
		"body",
		scan_scope(ctoken::OPERATOR_ENCLOSE_BRACE_L, parse_stmt_list, new_state(ps.p, ctoken::OPERATOR_ENCLOSE_BRACE_R))
	);
}

// stmt_list ::= stmt stmt_list | ε
static int parse_stmt_list(parser_state ps)
{
	return MANAGE_STATE(
		"stmt_list",
		(parse_stmt(new_state(ps.p, ps.end)) && parse_stmt_list(new_state(ps.p, ps.end)))
		|| parse_empty(new_state(ps.p, ps.end))
	);
}

// stmt ::= decl_var_stmt | assign_stmt | return_stmt | if_stmt | expr_stmt
static int parse_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"stmt",
		parse_body(new_state(ps.p, ps.end))
		|| parse_decl_var_stmt(new_state(ps.p, ps.end))
		|| parse_assign_stmt(new_state(ps.p, ps.end))
		|| parse_return_stmt(new_state(ps.p, ps.end))
		|| parse_if_stmt(new_state(ps.p, ps.end))
		|| parse_expr_stmt(new_state(ps.p, ps.end))
	);
}

// typename ::= "int" | "float"
static int parse_typename(parser_state ps)
{
	return MANAGE_STATE(
		"typename",
		match(ps.p, ctoken::KEYWORD_TYPE_INT)
		|| match(ps.p, ctoken::KEYWORD_TYPE_FLOAT)
	);
}

// var_sig ::= typename IDENTIFIER
static int parse_var_sig(parser_state ps)
{
	return MANAGE_STATE(
		"var_sig",
		parse_typename(new_state(ps.p, ps.end)) && match(ps.p, token::ALIAS)
	);
}

// var_sig_list ::= var_sig "," var_sig_list | var_sig | ε
static int parse_var_sig_list(parser_state ps)
{
	return MANAGE_STATE(
		"var_sig_list",
		(parse_var_sig(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_COMMA) && parse_var_sig_list(new_state(ps.p, ps.end)))
	)
	|| MANAGE_STATE(
		"var_sig_list",
		parse_var_sig(new_state(ps.p, ps.end))
		|| parse_empty(new_state(ps.p, ps.end))
	);
}

// decl_var_stmt ::= var_sig ";"
static int parse_decl_var_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"decl_var_stmt",
		parse_var_sig(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_SEMICOLON)
	);
}

// param_list ::= var_sig "," param_list | var_sig | ε
static int parse_param_list(parser_state ps)
{
	return
		MANAGE_STATE(
			"param_list",
			parse_var_sig(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_COMMA) && parse_param_list(new_state(ps.p, ps.end))
		)
		|| MANAGE_STATE(
			"param_list",
			parse_var_sig(new_state(ps.p, ps.end))
			|| parse_empty(new_state(ps.p, ps.end))
		);
}

// func_sig ::= var_sig "(" param_list ")"
static int parse_func_sig(parser_state ps)
{
	return MANAGE_STATE(
		"func_sig",
		parse_var_sig(new_state(ps.p, ps.end)) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_param_list, new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R))
	);
}

// def_func_stmt ::= func_sig body
static int parse_def_func_stmt(parser_state ps)
{
	//node *n = new_node()
	return MANAGE_STATE(
		"def_func_stmt",
		parse_func_sig(new_state(ps.p, ps.end)) && parse_body(new_state(ps.p, ps.end))
	);
}

// assign_op ::= "="
static int parse_assign_op(parser_state ps)
{
	return MANAGE_STATE(
		"assign_op",
		match(ps.p, ctoken::OPERATOR_ASSIGNMENT_SET)
	);
}

// assign_stmt ::= IDENTIFIER "=" expr ";"
static int parse_assign_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"assign_stmt",
		match(ps.p, token::ALIAS) && parse_assign_op(new_state(ps.p, ps.end)) && parse_expr(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_SEMICOLON)
	);
}

// if_stmt ::= "if" "(" expr ")" stmt
static int parse_if_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"if_stmt",
		match(ps.p, ctoken::KEYWORD_CONTROL_IF) && match(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L) && parse_expr(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R) && parse_stmt(new_state(ps.p, ps.end))
	);
}

// return_stmt ::= "return" expr_stmt | "return" ";"
static int parse_return_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"return_stmt",
		match(ps.p, ctoken::KEYWORD_CONTROL_RETURN) && (parse_expr_stmt(new_state(ps.p, ps.end)) || match(ps.p, ctoken::OPERATOR_SEMICOLON))
	);
}

// expr_stmt ::= expr ";"
static int parse_expr_stmt(parser_state ps)
{
	return MANAGE_STATE(
		"expr_stmt",
		parse_expr(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_SEMICOLON)
	);
}

// expr_list ::= expr "," expr_list | expr | ε
static int parse_expr_list(parser_state ps)
{
	return
		MANAGE_STATE(
			"expr_list",
			parse_expr(new_state(ps.p, ps.end)) && parse_empty(new_state(ps.p, ps.end))
		)
		|| MANAGE_STATE(
			"expr_list",
			parse_expr(new_state(ps.p, ps.end)) && match(ps.p, ctoken::OPERATOR_COMMA) && parse_expr_list(new_state(ps.p, ps.end))
		);
}

// expr ::= term opt_term
static int parse_expr(parser_state ps)
{
	return
		MANAGE_STATE(
			"expr",
			parse_term(new_state(ps.p, ps.end)) && parse_opt_term(new_state(ps.p, ps.end))
		);
}

// term ::= factor opt_factor
static int parse_term(parser_state ps)
{
	return
		MANAGE_STATE(
			"term",
			parse_factor(new_state(ps.p, ps.end)) && parse_opt_factor(new_state(ps.p, ps.end))
		);
}

// opt_term ::= (("+" | "-") term)*
static int parse_opt_term(parser_state ps)
{
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_ADD) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_SUB)) {
		if (!MANAGE_STATE("opt_term", parse_term(new_state(ps.p, ps.end)))) {
			return 0;
		}
	}
	return 1;
}

// factor ::= func_call | IDENTIFIER | LITERAL | "(" expr ")"
static int parse_factor(parser_state ps)
{
	return MANAGE_STATE(
		"factor",
		parse_func_call(new_state(ps.p, ps.end))
		|| match(ps.p, token::ALIAS)
		|| match(ps.p, ctoken::LITERAL_INT)
		|| scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr, new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R))
	);
}

// opt_factor ::= (("*" | "/" | "%") factor)*
static int parse_opt_factor(parser_state ps)
{
	while (match(ps.p, ctoken::OPERATOR_ARITHMETIC_MUL) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_DIV) || match(ps.p, ctoken::OPERATOR_ARITHMETIC_MOD)) {
		if (!MANAGE_STATE("opt_term", parse_factor(new_state(ps.p, ps.end)))) {
			return 0;
		}
	}
	return 1;
}

// func_call ::= IDENTIFIER "(" expr_list ")"
static int parse_func_call(parser_state ps)
{
	return MANAGE_STATE(
		"func_call",
		match(ps.p, token::ALIAS) && scan_scope(ctoken::OPERATOR_ENCLOSE_PARENTHESIS_L, parse_expr_list, new_state(ps.p, ctoken::OPERATOR_ENCLOSE_PARENTHESIS_R))
	);
}

int parsec(int max_tokens, const token *tokens, int max_nodes, node *nodes)
{
	parser p;
	p.in.tokens = tokens;
	p.in.index = 0;
	p.in.max_tokens = max_tokens;
	p.out.nodes = nodes;
	p.out.index = 0;
	p.out.max_nodes = max_nodes;

	parser_state ps = new_state(&p, token::STOP_EOF);
	return MANAGE_STATE("c", parse_program(new_state(ps.p, token::STOP_EOF))) <= 0 ? -1 : p.out.index;
}
