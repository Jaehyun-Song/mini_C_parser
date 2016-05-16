#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "main.h"

static struct func_list funclist;

FILE *fp;
FILE *st;

int declNum;
bool printcheck = false;

int main (void) {
	fp = fopen ("tree.txt", "w");
	st = fopen ("table.txt", "w");

	if (!yyparse ())
		startProgram ();

	fprintf (fp, "\n");
	fclose (fp);
	fclose (st);

	return 0;
}

void startProgram (void) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));

	if (head == NULL) {
		fprintf (stderr, "File does not exist.\n");
		return;
	}

	init_list (&funclist);

	strcpy (e->name, "NULL");
	e->nextCnt = 0;
	list_push (&funclist, e);

	printDeclList (head->decl, 0);
	printFuncList (head->func);

	list_pop (&funclist);
	free (e);
}

void printDeclList (struct DECLARATION *d, int num) {
	if (d == NULL) {
		//fprintf (stderr, "Declaration list does not exist.\n");
		return;
	}

	declNum = 0;

	if (num == 0)
		print_list (&funclist);

	if (d->prev != NULL)
		printDeclList (d->prev, num+1);

	if (d->t == eInt) {
		fprintf (fp, "int ");
		print_variable (d->id, true);
	}
	else {
		fprintf (fp, "float ");
		print_variable (d->id, false);
	}

	printIdenList (d->id, 0);
	fprintf (fp, ";\n");
}

void printFuncList (struct FUNCTION *f) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));
	int num = 0;

	if (f == NULL) {
		//fprintf (stderr, "Function list does not exist.\n");
		return;
	}

	if (f->prev != NULL)
		printFuncList (f->prev);

	if (f->t == eInt)
		fprintf (fp, "int ");
	else
		fprintf (fp, "float ");

	fprintf (fp, "%s(", f->ID);
	strcpy (e->name, f->ID);
	e->nextCnt = 0;
	list_push (&funclist, e);

	if (f->param != NULL)
		printParaList (f->param, num);

	fprintf (fp, "){\n");
	printCompStmt (f->cstmt);
	fprintf (fp, "}\n");

	list_pop (&funclist);
	free (e);
}

void printIdenList (struct IDENTIFIER *iden, int num) {
	if (iden == NULL) {
		//fprintf (stderr, "Identifier list does not exist.\n");
		return;
	}

	if (iden->prev != NULL)
		printIdenList (iden->prev, num+1);

	if (iden->intnum == 0)
		fprintf (fp, "%s", iden->ID);
	else
		fprintf (fp, "%s[%d]", iden->ID, iden->intnum);

	if (num != 0)
		fprintf (fp, ", ");
}

void printParaList (struct PARAMETER *p, int num) {
	if (p == NULL) {
		//fprintf (stderr, "Parameter list does not exist.\n");
		return;
	}

	if (p->prev != NULL)
		printParaList (p->prev, num+1);

	if (p->t == eInt)
		fprintf (fp, "int ");
	else
		fprintf (fp, "float ");

	printIdenList (p->id, 0);

	if (num != 0)
		fprintf (fp, ", ");
}

void printCompStmt (struct COMPOUNDSTMT *c) {
	if (c == NULL) {
		//fprintf (stderr, "Compountstmt does not exist.\n");
		return;
	}

	inc_comp (&funclist);

	if (c->decl != NULL) {
		printDeclList (c->decl, 0);
	}

	printStmtList (c->stmt);

	dec_comp (&funclist);
}

void printStmtList (struct STMT *st) {
	if (st == NULL) {
		//fprintf (stderr, "Statement list does not exist.\n");
		return;
	}

	if (st->prev != NULL)
		printStmtList (st->prev);

	if (st->s == eAssign) {
		printAssi (st->stmt.assign_);
		fprintf (fp, ";\n");
	}
	else if (st->s == eCall) {
		printCall (st->stmt.call_);
		fprintf (fp, ";\n");
	}
	else if (st->s == eRet) {
		printRet (st->stmt.return_);
		fprintf (fp, ";\n");
	}
	else if (st->s == eWhile) {
		printWhil (st->stmt.while_);
		fprintf (fp, "\n");
	}
	else if (st->s == eFor) {
		printFor (st->stmt.for_);
		fprintf (fp, "\n");
	}
	else if (st->s == eIf) {
		printIf (st->stmt.if_);
		fprintf (fp, "\n");
	}
	else if (st->s == eCompound) {
		printCompStmt (st->stmt.cstmt_);
	}
	else if (st->s == eSwitch) {
		printSwit (st->stmt.switch_);
		fprintf (fp, "\n");
	}
	else
		fprintf (fp, ";\n");
}

void printAssi (struct ASSIGN *a) {
	if (a == NULL) {
		//fprintf (stderr, "Assign does not exist.\n");
		return;
	}

	if (a->index == NULL)
		fprintf (fp, "%s", a->ID);
	else {
		fprintf (fp, "%s[", a->ID);
		printExpr (a->index);
		fprintf (fp, "]");
	}

	fprintf (fp, "=");
	printExpr (a->expr);
}

void printCall (struct CALL *c) {
	if (c == NULL) {
		//fprintf (stderr, "Call does not exist.\n");
		return;
	}

	fprintf (fp, "%s(", c->ID);
	printArguList (c->arg, 0);
	fprintf (fp, ")");
}

void printRet (struct EXPR *r) {
	fprintf (fp, "return");

	if (r != NULL) {
		fprintf (fp, " ");
		printExpr (r);
	}
}

void printWhil (struct WHILE_S *w) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));

	if (w == NULL) {
		//fprintf (stderr, "While does not exist.\n");
		return;
	}
	
	strcpy (e->name, "while");
	e->nextCnt = 0;
	list_push (&funclist, e);

	if (w->do_while) {
		fprintf (fp, "do{\n");
		printStmtList (w->stmt);
		fprintf (fp, "} while(");
		printExpr (w->cond);
		fprintf (fp, ");");
	}
	else {
		fprintf (fp, "while(");
		printExpr (w->cond);
		fprintf (fp, "){\n");
		printStmtList (w->stmt);
		fprintf (fp, "}");
	}

	list_pop (&funclist);
	free (e);
}

void printFor (struct FOR_S *f) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));

	if (f == NULL) {
		//fprintf (stderr, "For does not exist.\n");
		return;
	}

	strcpy (e->name, "for");
	e->nextCnt = 0;
	list_push (&funclist, e);

	fprintf (fp, "for(");
	printAssi (f->init);
	fprintf (fp, ";");
	printExpr (f->cond);
	fprintf (fp, ";");
	printAssi (f->inc);
	fprintf (fp, "){\n");
	printStmtList (f->stmt);
	fprintf (fp, "}");

	list_pop (&funclist);
	free (e);
}

void printIf (struct IF_S *i) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));

	if (i == NULL) {
		//fprintf (stderr, "If does not exist.\n");
		return;
	}

	strcpy (e->name, "if");
	e->nextCnt = 0;
	list_push (&funclist, e);

	fprintf (fp, "if(");
	printExpr (i->cond);
	fprintf (fp, "){\n");
	printStmtList (i->if_);
	fprintf (fp, "}");

	if (i->else_ != NULL) {
		fprintf (fp, " else{\n");
		printStmtList (i->else_);
		fprintf (fp, "}");
	}

	list_pop (&funclist);
	free (e);
}

void printSwit (struct SWITCH_S *s) {
	struct func_elem *e = (struct func_elem *) malloc (sizeof (struct func_elem));

	if (s == NULL) {
		//fprintf (stderr, "Switch does not exist.\n");
		return;
	}

	strcpy (e->name,  "switch");
	e->nextCnt = 0;
	list_push (&funclist, e);

	fprintf (fp, "switch(");
	printIdenList (s->identifier, 0);
	fprintf (fp, "){\n");
	printCaseList (s->case_);
	fprintf (fp, "}");

	list_pop (&funclist);
	free (e);
}

void printCaseList (struct CASE *c) {
	if (c == NULL) {
		//fprintf (stderr, "Case list does not exist.\n");
		return;
	}

	if (c->prev != NULL)
		printCaseList (c->prev);

	if (c->intnum == 0)
		fprintf (fp, "default:\n");
	else
		fprintf (fp, "case %d:\n", c->intnum);
	
	printStmtList (c->stmt);

	if (c->break_)
		fprintf (fp, "break;\n");
}

void printExpr (struct EXPR *ex) {
	if (ex == NULL) {
		//fprintf (stderr, "Expression does not exist.\n");
		return;
	}

	if (ex->e == eUnop) {
		if (ex->expression.unop_->u == eNegative)
			fprintf (fp, "-");
		printExpr (ex->expression.unop_->expr);
	}
	else if (ex->e == eAddi) {
		printExpr (ex->expression.addiop_->lhs);

		if (ex->expression.addiop_->a == ePlus)
			fprintf (fp, "+");
		else
			fprintf (fp, "-");

		printExpr (ex->expression.addiop_->rhs);
	}
	else if (ex->e == eMult) {
		printExpr (ex->expression.multop_->lhs);

		if (ex->expression.multop_->m == eMulti)
			fprintf (fp, "*");
		else
			fprintf (fp, "/");

		printExpr (ex->expression.multop_->rhs);
	}
	else if (ex->e == eRela) {
		printExpr (ex->expression.eqltop_->lhs);

		if (ex->expression.relaop_->r == eLT)
			fprintf (fp, "<");
		else if (ex->expression.relaop_->r == eGT)
			fprintf (fp, ">");
		else if (ex->expression.relaop_->r == eLE)
			fprintf (fp, "<=");
		else
			fprintf (fp, ">=");

		printExpr (ex->expression.eqltop_->rhs); 
	}
	else if (ex->e == eEqlt) {
		printExpr (ex->expression.eqltop_->lhs);

		if (ex->expression.eqltop_->e == eEQ)
			fprintf (fp, "==");
		else
			fprintf (fp, "!=");

		printExpr (ex->expression.eqltop_->rhs);
	}
	else if (ex->e == eCallExpr) {
		printCall (ex->expression.call_);
	}
	else if (ex->e == eIntnum) {
		fprintf (fp, "%d", ex->expression.intnum);
	}
	else if (ex->e == eFloatnum) {
		fprintf (fp, "%f", ex->expression.floatnum);
	}
	else if (ex->e == eId) {
		fprintf (fp, "%s", ex->expression.ID_->ID);

		if (ex->expression.ID_->expr != NULL) {
			fprintf (fp, "[");
			printExpr (ex->expression.ID_->expr);
			fprintf (fp, "]");
		}
	}
	else {	// eExpr
		fprintf (fp, "(");
		printExpr (ex->expression.bracket);
		fprintf (fp, ")");
	}
}

void printArguList (struct ARG *a, int num) {
	if (a == NULL) {
		//fprintf (stderr, "Argument list does not exist.\n");
		return;
	}

	if (a->prev != NULL)
		printArguList (a->prev, num+1);

	printExpr (a->expr);

	if (num != 0)
		fprintf (fp, ", ");
}

void init_list (struct func_list *list) {
	list->head.prev = NULL;
	list->head.next = &list->tail;
	list->tail.prev = &list->head;
	list->tail.next = NULL;
}

void inc_comp (struct func_list *list) {
	list->tail.prev->nextCnt++;
}

void dec_comp (struct func_list *list) {
	list->tail.prev->nextCnt--;
}

void list_push (struct func_list *list, struct func_elem *elem) {
	elem->prev = list->tail.prev;
	elem->next = &list->tail;
	list->tail.prev->next = elem;
	list->tail.prev = elem;
}

void list_pop (struct func_list *list) {
	struct func_elem *back = list_back (list);
	list_remove (back);
}

struct func_elem *list_back (struct func_list *list) {
	return list->tail.prev;
}

void list_remove (struct func_elem *elem) {
	elem->prev->next = elem->next;
	elem->next->prev = elem->prev;
}

void print_list (struct func_list *list) {
	struct func_elem *e = list->head.next;

	if (printcheck == true)
		fprintf (st, "\n");

	fprintf (st, "Function name : ");

	while (e != &list->tail) {
		fprintf (st, "%s(", e->name);

		if (e->prev != &list->head)
			fprintf (st, "%d)", e->nextCnt);
		else
			fprintf (st, "0)");

		if (e->next != &list->tail)
			fprintf (st, " - ");

		e = e->next;
	}

	fprintf (st, "\n     Count      Type      Name     Array      Role\n");
	printcheck = true;
}

void print_variable (struct IDENTIFIER *i, bool t) {
	if (i->prev != NULL)
		print_variable (i->prev, t);

	declNum++;

	if (t == true)
		fprintf (st, "%10d       int%10s%10d  Variable\n", declNum, i->ID, i->intnum);
	else
		fprintf (st, "%10d     float%10s%10d  Variable\n", declNum, i->ID, i->intnum);
}
