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

struct node;

/// @brief Data relating to a variable/constant instance of a data type.
struct instance_data
{
	unsigned type;   // Used to determine the resulting type of an instance: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types is 0xf...f
	unsigned deref; // Essentially, the number of pointer asterisks plus one. Zero if it is an immediate constant.
	signed   array; // TODO(implement) >= 0 means that this is an array, and the number specifies the number of elements.
	unsigned flags; // const, static, extern, etc.
};

/// @brief Data relating to a mathematical operation.
struct operation_data
{
	unsigned  type;      // Used to determine the resulting type of an operation between mixed types, i.e. always the highest ranking type in this order: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types not allowed.
	unsigned  operation; // add, sub, mul etc.
	node     *left;      // lhs
	node     *right;     // rhs
};

/// @brief Data relating to a mathematical operand.
struct operand_data
{
	unsigned type; // Used to determine the resulting type of an operand: schar, uchar, sshort, ushort, sint, uint, slong, ulong, sllong, ullong, float, double. User-defined types not allowed.
};

/// @brief Data relating to a conditional statement including actions on true and false.
struct condition_data
{
	node *test; // Guaranteed to be an expression node.
	node *on_true;
	node *on_false; // This is empty if the condition does not have an else clause.
};

/// @brief Data relating to loops.
struct loop_data
{
	unsigned  type; // for, while, do-while
	node     *condition; // Guaranteed to be a conditional node.
};

// TODO DOC
struct node
{
	token  tok;  // Information about the token.
	node  *next; // The next token in line.
	enum {
		INSTANCE,
		OPERATION,
		OPERAND
		//CONDITION,
		//LOOP
	} type;

	union {
		instance_data  instance;
		operation_data operation;
		operand_data   operand;
		//condition_data condition;
		//loop_data      loop;
	} data;
};

// TODO DOC
int parse(int max_tokens, const token *tokens, int max_nodes, node *nodes, bool debug_text);

#endif
