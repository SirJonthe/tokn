#ifndef PARSE_H
#define PARSE_H

#include "lex.h"

// TODO DOC
struct node
{
	unsigned type;

	node *next;

	enum {
		ROOT,
		PROGRAM,
		FN_DEF
	};

	union {
		struct root_data {
			node *program;
		} root;
		struct program_data {
			node *statements;
			// statements
		} program;
		struct fn_def_data {
			// signature
			// body
		} fn_def;
		// body (contains statements)
	} data;
};

/// @brief Parses C code in the form of a stream of C tokens.
/// @param max_tokens The maximum number of tokens to parse. Never parse more tokens than the tokens pointer holds.
/// @param tokens The token stream.
/// @param max_nodes The maximum number of nodes in the output abstract syntax tree.
/// @param nodes The nodes that make up the abstract syntax tree. The first node (index=0) is always the root node.
/// @return The number of nodes that has been output to the nodes field. Errors resulting in an untraversable node tree yeilds negative numbers in the return value.
int parsec(int max_tokens, const token *tokens, int max_nodes, node *nodes);

#endif
