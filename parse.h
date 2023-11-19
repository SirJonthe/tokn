#ifndef PARSE_H
#define PARSE_H

#include "lex.h"

// flattening an expression
// use Polish notation

// a = 1 + 2 * 3
	// node(* 2 3)
	// node(+ 1)
	// node(= a)

// 1 + 2 * 3
	// node(* 2 3)
	// node(+ 1)
	// node(DISCARD)

// if (1 + 2 * 3)
	// node(* 2 3)
	// node(+ 1)
	// node(TEST)

// TODO DOC
struct node
{
	token  tok;  // Information about the token.
	signed next; // The next token in line.
	enum {
		UNIT, // A translation unit. Every file starts with this and can be treated as a local root node. The root of the AST is also a UNIT type.
		INSTANCE,
		OPERATION,
		OPERAND
		//CONDITIONAL,
		//LOOP
	} type;

	union {
		/// @brief Data relating to a variable/constant instance of a data type.
		struct instance_data
		{
			unsigned type;   // Used to determine the resulting type of an instance: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types is 0xf...f
			unsigned deref; // Essentially, the number of pointer asterisks plus one. Zero if it is an immediate constant.
			signed   array; // TODO(implement) >= 0 means that this is an array, and the number specifies the number of elements.
			unsigned flags; // const, static, extern, etc.
		} instance;

		/// @brief Data relating to a mathematical operation.
		struct operation_data
		{
			unsigned  type;      // Used to determine the resulting type of an operation between mixed types, i.e. always the highest ranking type in this order: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types not allowed.
			unsigned  operation; // add, sub, mul etc.
			signed    left;      // lhs
			signed    right;     // rhs
		} operation;

		/// @brief Data relating to a mathematical operand.
		struct operand_data
		{
			unsigned type; // Used to determine the resulting type of an operand: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types not allowed.
		} operand;

		/// @brief Data relating to a conditional statement including actions on true and false.
		struct conditional_data
		{
			signed test; // Guaranteed to be an expression node.
			signed on_true;
			signed on_false; // This is empty if the condition does not have an else clause.
		} conditional;

		/// @brief Data relating to loops.
		struct loop_data
		{
			unsigned type; // for, while, do-while
			signed   condition; // Guaranteed to be a conditional node.
		} loop;

		/// @brief Data relating to some kind of parsing error.
		struct error_data {} error;
	} data;
};

/// @brief Parses C code in the form of a stream of C tokens.
/// @param max_tokens The maximum number of tokens to parse. Never parse more tokens than the tokens pointer holds.
/// @param tokens The token stream.
/// @param max_nodes The maximum number of nodes in the output abstract syntax tree.
/// @param nodes The nodes that make up the abstract syntax tree. The first node (index=0) is always the root node.
/// @param debug_text Prints debug text when true.
/// @return The number of nodes that has been output to the nodes field. Errors resulting in an untraversable node tree yeilds negative numbers in the return value.
int parsec(int max_tokens, const token *tokens, int max_nodes, node *nodes, bool debug_text);

#endif
