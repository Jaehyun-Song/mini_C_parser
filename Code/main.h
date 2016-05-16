#ifndef MAIN_H
#define MAIN_H

#include "AST.h"

struct func_elem {
	struct func_elem *prev;
	struct func_elem *next;
	char name[16];
	int nextCnt;
};

struct func_list {
	struct func_elem head;
	struct func_elem tail;
};

struct PROGRAM *head;

void startProgram (void);
void printDeclList (struct DECLARATION *, int num);
void printFuncList (struct FUNCTION *);
void printIdenList (struct IDENTIFIER *, int num);
void printParaList (struct PARAMETER *, int num);
void printCompStmt (struct COMPOUNDSTMT *);
void printStmtList (struct STMT *);
void printAssi (struct ASSIGN *);
void printCall (struct CALL *);
void printRet (struct EXPR *);
void printWhil (struct WHILE_S *);
void printFor (struct FOR_S *);
void printIf (struct IF_S *);
void printSwit (struct SWITCH_S *);
void printCaseList (struct CASE *);
void printExpr (struct EXPR *);
void printArguList (struct ARG *, int num);

void init_list (struct func_list *);
void inc_comp (struct func_list *);
void dec_comp (struct func_list *);
void list_push (struct func_list *, struct func_elem *);
void list_pop (struct func_list *);
struct func_elem *list_back (struct func_list *list);
void list_remove (struct func_elem *);
void print_list (struct func_list *);
void print_variable (struct IDENTIFIER *, bool t);

#endif
