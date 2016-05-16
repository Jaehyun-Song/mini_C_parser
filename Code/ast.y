%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "main.h"
%}

%union {
	struct PROGRAM *ptr_prog;
	struct DECLARATION *ptr_decl;
	struct IDENTIFIER *ptr_iden;
	struct FUNCTION *ptr_func;
	struct PARAMETER *ptr_para;
	struct COMPOUNDSTMT *ptr_comp;
	struct STMT *ptr_stmt;
	struct CASE *ptr_case;
	struct SWITCH_S *ptr_swit;
	struct ASSIGN *ptr_assi;
	struct CALL *ptr_call;
	struct ARG *ptr_arg;
	struct WHILE_S *ptr_whil;
	struct FOR_S *ptr_for;
	struct IF_S *ptr_if;
	struct EXPR *ptr_expr;
	struct UNOP *ptr_unop;
	struct ADDIOP *ptr_addi;
	struct MULTOP *ptr_mult;
	struct RELAOP *ptr_rela;
	struct EQLTOP *ptr_eqlt;
	struct ID_S *ptr_id;
	int type;
	int intnum;
	float floatnum;
	char identifier[256];
}

%token INTNUM FLOATNUM
%token INT FLOAT RETURN WHILE DO FOR IF ELSE SWITCH CASE BREAK DEFAULT
%token MULT DIVI PLUS MINU RELT REGT RELE REGE EQUL UNEQ ASGN
%token COMMA LSBT RSBT LBKT RBKT LBRA RBRA SEMICOL COLON
%token ID

%left LBKT
%left RBKT

%right UNOP

%left MULT
%left DIVI

%left PLUS
%left MINU

%left RELT
%left REGT
%left RELE
%left REGE

%left EQUL
%left UNEQ

%right ASGN

%nonassoc IF_THEN
%nonassoc ELSE

%type <ptr_prog> Prog;
%type <ptr_decl> Decl declList;
%type <ptr_iden> Iden idenList;
%type <ptr_func> Func funcList;
%type <ptr_para> paraList;
%type <ptr_comp> compStmt;
%type <ptr_stmt> Stmt stmtList;
%type <ptr_case> caseList;
%type <ptr_swit> switStmt;
%type <ptr_assi> Assi assiStmt;
%type <ptr_call> Call callStmt;
%type <ptr_arg> argList;
%type <ptr_whil> whilStmt;
%type <ptr_for> forStmt;
%type <ptr_if> ifStmt;
%type <ptr_expr> Expr retStmt;
%type <ptr_unop> Unop;
%type <ptr_addi> Addi;
%type <ptr_mult> Mult;
%type <ptr_rela> Rela;
%type <ptr_eqlt> Eqlt;
%type <type> Type;
%type <intnum> INTNUM;
%type <floatnum> FLOATNUM;
%type <identifier> ID;

%%

Prog	: declList {
	//printf("DECLLIST\n");
	struct PROGRAM *prog = (struct PROGRAM *) malloc (sizeof (struct PROGRAM));
	prog->decl = $1;
	prog->func = NULL;
	head = prog;
	$$ = prog;
}
	| funcList {
	//printf("FUNCLIST\n");
	struct PROGRAM *prog = (struct PROGRAM *) malloc (sizeof (struct PROGRAM));
	prog->decl = NULL;
	prog->func = $1;
	head = prog;
	$$ = prog;
}
	| declList funcList {
	//printf("DECLLIST FUNCLIST\n");
	struct PROGRAM *prog = (struct PROGRAM *) malloc (sizeof (struct PROGRAM));
	prog->decl = $1;
	prog->func = $2;
	head = prog;
	$$ = prog;
}
	| {
	//printf("NULL in prog\n");
	head = NULL;
}
	;

declList : Decl {
	//printf("DECL\n");
	struct DECLARATION *decl = (struct DECLARATION *) malloc (sizeof (struct DECLARATION));
	decl = $1;
	decl->prev = NULL;
	$$ = decl;
}
	| declList Decl {
	//printf("DECLLIST DECL\n");
	struct DECLARATION *decl = (struct DECLARATION *) malloc (sizeof (struct DECLARATION));
	decl = $2;
	decl->prev = $1;
	$$ = decl;
}
	;

funcList : Func {
	//printf("FUNC\n");
	struct FUNCTION *func = (struct FUNCTION *) malloc (sizeof (struct FUNCTION));
	func = $1;
	$$ = func;
}
	| funcList Func {
	//printf("FUNCLIST FUNC\n");
	struct FUNCTION *func = (struct FUNCTION *) malloc (sizeof (struct FUNCTION));
	func = $2;
	func->prev = $1;
	$$ = func;
}
	;

Decl	: Type idenList SEMICOL {
	//printf("TYPE IDENLIST SEMICOL\n");
	struct DECLARATION *decl = (struct DECLARATION *) malloc (sizeof (struct DECLARATION));
	decl->t = $1;
	decl->id = $2;
	decl->prev = NULL;
	$$ = decl;
}
	;

idenList : Iden {
	//printf("IDEN\n");
	struct IDENTIFIER *iden = (struct IDENTIFIER *) malloc (sizeof (struct IDENTIFIER));
	iden = $1;
	iden->prev = NULL;
	$$ = iden;
}
	| idenList COMMA Iden {
	//printf("IDENLIST COMMA IDEN\n");
	struct IDENTIFIER *iden = (struct IDENTIFIER *) malloc (sizeof (struct IDENTIFIER));
	iden = $3;
	iden->prev = $1;
	$$ = iden;
}
	;

Iden	: ID {
	//printf("ID in iden\n");
	struct IDENTIFIER *iden = (struct IDENTIFIER *) malloc (sizeof (struct IDENTIFIER));
	iden->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (iden->ID, $1, strlen($1)+1);
	iden->intnum = 0;
	iden->prev = NULL;
	$$ = iden;
}
	| ID LSBT INTNUM RSBT {
	//printf("ID LSBT INTNUM RSBT\n");
	struct IDENTIFIER *iden = (struct IDENTIFIER *) malloc (sizeof (struct IDENTIFIER));
	iden->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (iden->ID, $1, strlen($1)+1);
	iden->intnum = $3;
	iden->prev = NULL;
	$$ = iden;
}
	;

Func	: Type ID LBKT RBKT compStmt {
	//printf("TYPE ID LBKT RBKT COMPSTMT\n");
	struct FUNCTION *func = (struct FUNCTION *) malloc (sizeof (struct FUNCTION));
	func->t = $1;
	func->ID = (char *) malloc (sizeof (strlen($2)+1));
	strncpy (func->ID, $2, strlen($2)+1);
	func->param = NULL;
	func->cstmt = $5;
	func->prev = NULL;
	$$ = func;
}
	| Type ID LBKT paraList RBKT compStmt {
	//printf("TYPE ID LBKT PARALIST RBKT COMPSTMT\n");
	struct FUNCTION *func = (struct FUNCTION *) malloc (sizeof (struct FUNCTION));
	func->t = $1;
	func->ID = (char *) malloc (sizeof (strlen($2)+1));
	strncpy (func->ID, $2, strlen($2)+1);
	func->param = $4;
	func->cstmt = $6;
	func->prev = NULL;
	$$ = func;
}
	;

paraList : Type Iden {
	//printf("TYPE IDEN\n");
	struct PARAMETER *para = (struct PARAMETER *) malloc (sizeof (struct PARAMETER));
	para->t = $1;
	para->id = $2;
	para->prev = NULL;
	$$ = para;
}
	| paraList COMMA Type Iden {
	//printf("PARALIST COMMA TYPE IDEN\n");
	struct PARAMETER *para = (struct PARAMETER *) malloc (sizeof (struct PARAMETER));
	para->t = $3;
	para->id = $4;
	para->prev = $1;
	$$ = para;
}
	;

Type	: INT {
	//printf("INT in Y\n");
	$$ = eInt;
}
	| FLOAT {
	//printf("FLOAT in Y\n");
	$$ = eFloat;
}
	;

compStmt : LBRA stmtList RBRA {
	//printf("LBRA STMTLIST RBRA\n");
	struct COMPOUNDSTMT *comp = (struct COMPOUNDSTMT *) malloc (sizeof (struct COMPOUNDSTMT));
	comp->decl = NULL;
	comp->stmt = $2;
	$$ = comp;
}
	| LBRA declList stmtList RBRA {
	//printf("LBRA DECLLIST STMTLIST RBRA\n");
	struct COMPOUNDSTMT *comp = (struct COMPOUNDSTMT *) malloc (sizeof (struct COMPOUNDSTMT));
	comp->decl = $2;
	comp->stmt = $3;
	$$ = comp;
}
	;

stmtList : stmtList Stmt {
	//printf("STMTLIST STMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement = $2;
	statement->prev = $1;
	$$ = statement;
}
	| {
	//printf("NULL in stmtlist\n");
	$$ = NULL;
}
	;

Stmt	: assiStmt {
	//printf("ASSISTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eAssign;
	statement->stmt.assign_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| callStmt {
	//printf("CALLSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eCall;
	statement->stmt.call_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| retStmt {
	//printf("RETSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eRet;
	statement->stmt.return_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| whilStmt {
	//printf("WHILSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eWhile;
	statement->stmt.while_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| forStmt {
	//printf("FORSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eFor;
	statement->stmt.for_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| ifStmt {
	//printf("IFSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eIf;
	statement->stmt.if_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| switStmt {
	//printf("SWITSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eSwitch;
	statement->stmt.switch_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| compStmt {
	//printf("COMPSTMT\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eCompound;
	statement->stmt.cstmt_ = $1;
	statement->prev = NULL;
	$$ = statement;
}
	| SEMICOL {
	//printf("SEMICOL\n");
	struct STMT *statement = (struct STMT *) malloc (sizeof (struct STMT));
	statement->s = eSemi;
	statement->prev = NULL;
	$$ = statement;
}
	;

assiStmt : Assi SEMICOL {
	//printf("ASSI SEMICOL\n");
	struct ASSIGN *assi = (struct ASSIGN *) malloc (sizeof (struct ASSIGN));
	assi = $1;
	$$ = assi;
}
	;

Assi	: ID ASGN Expr {
	//printf("ID ASGN EXPR\n");
	struct ASSIGN *assi = (struct ASSIGN *) malloc (sizeof (struct ASSIGN));
	assi->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (assi->ID, $1, strlen($1)+1);
	assi->index = NULL;
	assi->expr = $3;
	$$ = assi;
}
	| ID LSBT Expr RSBT ASGN Expr {
	//printf("ID LSBT EXPR RSBT ASGN EXPR\n");
	struct ASSIGN *assi = (struct ASSIGN *) malloc (sizeof (struct ASSIGN));
	assi->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (assi->ID, $1, strlen($1)+1);
	assi->index = $3;
	assi->expr = $6;
	$$ = assi;
}
	;

callStmt : Call SEMICOL {
	//printf("CALL SEMICOL\n");
	struct CALL *call = (struct CALL *) malloc (sizeof (struct CALL));
	call = $1;
	$$ = call;
}
	;

Call	: ID LBKT RBKT {
	//printf("ID LBKT RBKT\n");
	struct CALL *call = (struct CALL *) malloc (sizeof (struct CALL));
	call->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (call->ID, $1, strlen($1)+1);
	call->arg = NULL;
	$$ = call;
}
	| ID LBKT argList RBKT {
	//printf("ID LBKT ARGLIST RBKT\n");
	struct CALL *call = (struct CALL *) malloc (sizeof (struct CALL));
	call->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (call->ID, $1, strlen($1)+1);
	call->arg = $3;
	$$ = call;
}
	;

retStmt	: RETURN SEMICOL {
	//printf("RETURN SEMICOL\n");
	struct EXPR *expr = (struct EXPR *) malloc (sizeof (struct EXPR));
	expr = NULL;
	$$ = expr;
}
	| RETURN Expr SEMICOL {
	//printf("RETURN EXPR SEMICOL\n");
	struct EXPR *expr = (struct EXPR *) malloc (sizeof (struct EXPR));
	expr = $2;
	$$ = expr;
}
	;

whilStmt : WHILE LBKT Expr RBKT Stmt {
	//printf("WHILE LBKT EXPR RBKT STMT\n");
	struct WHILE_S *while_s = (struct WHILE_S *) malloc (sizeof (struct WHILE_S));
	while_s->do_while = false;
	while_s->cond = $3;
	while_s->stmt = $5;
	$$ = while_s;
}
	| DO Stmt WHILE LBKT Expr RBKT SEMICOL {
	//printf("DO STMT WHILE LBKT EXPR RBKT SEMICOL\n");
	struct WHILE_S *while_s = (struct WHILE_S *) malloc (sizeof (struct WHILE_S));
	while_s->do_while = true;
	while_s->cond = $5;
	while_s->stmt = $2;
	$$ = while_s;
}
	;

forStmt	: FOR LBKT Assi SEMICOL Expr SEMICOL Assi RBKT Stmt {
	//printf("FOR LBKT ASSI SEMICOL EXPR SEMICOL ASSI RBKT STMT\n");
	struct FOR_S *for_s = (struct FOR_S *) malloc (sizeof (struct FOR_S));
	for_s->init = $3;
	for_s->cond = $5;
	for_s->inc = $7;
	for_s->stmt = $9;
	$$ = for_s;
}
	;

ifStmt	: IF LBKT Expr RBKT Stmt %prec IF_THEN {
	//printf("IF LBKT EXPR RBKT STMT\n");
	struct IF_S *if_s = (struct IF_S *) malloc (sizeof (struct IF_S));
	if_s->cond = $3;
	if_s->if_ = $5;
	if_s->else_ = NULL;
	$$ = if_s;
}
	| IF LBKT Expr RBKT Stmt ELSE Stmt {
	//printf("IF LBKT EXPR RBKT STMT ELSE STMT\n");
	struct IF_S *if_s = (struct IF_S *) malloc (sizeof (struct IF_S));
	if_s->cond = $3;
	if_s->if_ = $5;
	if_s->else_ = $7;
	$$ = if_s;
}
	;

switStmt : SWITCH LBKT Iden RBKT LBRA caseList RBRA {
	//printf("SWITCH LBKT IDEN RBKT LBRA CASELIST RBRA\n");
	struct SWITCH_S *switch_s = (struct SWITCH_S *) malloc (sizeof (struct SWITCH_S));
	switch_s->identifier = $3;
	switch_s->case_ = $6;
	$$ = switch_s;
}
	;

caseList : CASE INTNUM COLON stmtList {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = false;
	c->intnum = $2;
	c->stmt = $4;
	c->prev = NULL;
	$$ = c;
}
	| CASE INTNUM COLON stmtList BREAK SEMICOL {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = true;
	c->intnum = $2;
	c->stmt = $4;
	c->prev = NULL;
	$$ = c;
} 
	| DEFAULT COLON stmtList {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = false;
	c->intnum = 0;
	c->stmt = $3;
	c->prev = NULL;
	$$ = c;
}
	| DEFAULT COLON stmtList BREAK SEMICOL {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = true;
	c->intnum = 0;
	c->stmt = $3;
	c->prev = NULL;	
	$$ = c;
}
	| caseList CASE INTNUM COLON stmtList {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = false;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	$$ = c;
}
	| caseList CASE INTNUM COLON stmtList BREAK SEMICOL {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = true;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	$$ = c;
}
	| caseList CASE INTNUM COLON stmtList DEFAULT COLON stmtList {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	struct CASE *d = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = false;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	d->break_ = false;
	d->intnum = 0;
	d->stmt = $8;
	d->prev = c;
	$$ = d;
}
	| caseList CASE INTNUM COLON stmtList BREAK SEMICOL DEFAULT COLON stmtList {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	struct CASE *d = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = true;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	d->break_ = false;
	d->intnum = 0;
	d->stmt = $10;
	d->prev = c;
	$$ = d;
}
	| caseList CASE INTNUM COLON stmtList DEFAULT COLON stmtList BREAK SEMICOL {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	struct CASE *d = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = false;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	d->break_ = true;
	d->intnum = 0;
	d->stmt = $8;
	d->prev = c;
	$$ = d;
}
	| caseList CASE INTNUM COLON stmtList BREAK SEMICOL DEFAULT COLON stmtList BREAK SEMICOL {
	struct CASE *c = (struct CASE *) malloc (sizeof (struct CASE));
	struct CASE *d = (struct CASE *) malloc (sizeof (struct CASE));
	c->break_ = true;
	c->intnum = $3;
	c->stmt = $5;
	c->prev = $1;
	d->break_ = true;
	d->intnum = 0;
	d->stmt = $10;
	d->prev = c;	
	$$ = d;
}
	| {
	$$ = NULL;
}
	;

Expr	: Unop Expr %prec UNOP {
	//printf("UNOP EXPR\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eUnop;
	exp->expression.unop_ = $1;
	exp->expression.unop_->expr = $2;
	$$ = exp;
}
	| Expr Addi Expr {
	//printf("EXPR ADDI EXPR\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eAddi;
	exp->expression.addiop_ = $2;
	exp->expression.addiop_->lhs = $1;
	exp->expression.addiop_->rhs = $3;
	$$ = exp;
}
	| Expr Mult Expr {
	//printf("EXPR MULT EXPR\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eMult;
	exp->expression.multop_ = $2;
	exp->expression.multop_->lhs = $1;
	exp->expression.multop_->rhs = $3;
	$$ = exp;
}
	| Expr Rela Expr {
	//printf("EXPR RELA EXPR\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eRela;
	exp->expression.relaop_ = $2;
	exp->expression.relaop_->lhs = $1;
	exp->expression.relaop_->rhs = $3;
	$$ = exp;
}
	| Expr Eqlt Expr {
	//printf("EXPR EQLT EXPR\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eEqlt;
	exp->expression.eqltop_ = $2;
	exp->expression.eqltop_->lhs = $1;
	exp->expression.eqltop_->rhs = $3;
	$$ = exp;
}
	| Call {
	//printf("CALL\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eCallExpr;
	exp->expression.call_ = $1;
	$$ = exp;
}
	| INTNUM {
	//printf("INTNUM\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eIntnum;
	exp->expression.intnum = $1;
	$$ = exp;
}
	| FLOATNUM {
	//printf("FLOATNUM\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eFloatnum;
	exp->expression.floatnum = $1;
	$$ = exp;
}
	| ID {
	//printf("ID in expr\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	struct ID_S *_exp = (struct ID_S *) malloc (sizeof (struct ID_S));
	_exp->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (_exp->ID, $1, strlen($1)+1);
	_exp->expr = NULL;
	exp->e = eId;
	exp->expression.ID_ = _exp;
	$$ = exp;
}
	| ID LSBT Expr RSBT {
	//printf("ID LSBT EXPR RSBT\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	struct ID_S *_exp = (struct ID_S *) malloc (sizeof (struct ID_S));
	_exp->ID = (char *) malloc (sizeof (strlen($1)+1));
	strncpy (_exp->ID, $1, strlen($1)+1);
	_exp->expr = $3;
	exp->e = eId;
	exp->expression.ID_ = _exp;
	$$ = exp;
}
	| LBKT Expr RBKT {
	//printf("LBKT EXPR RBKT\n");
	struct EXPR *exp = (struct EXPR *) malloc (sizeof (struct EXPR));
	exp->e = eExpr;
	exp->expression.bracket = $2;
	$$ = exp;
}
	;

argList	: Expr {
	//printf("EXPR\n");
	struct ARG *arg = (struct ARG *) malloc (sizeof (struct ARG));
	arg->expr = $1;
	arg->prev = NULL;
	$$ = arg;
}
	| argList COMMA Expr {
	//printf("ARGLIST COMMA EXPR\n");
	struct ARG *arg = (struct ARG *) malloc (sizeof (struct ARG));
	arg->expr = $3;
	arg->prev = $1;
	$$ = arg;
}
	;

Unop	: MINU {
	//printf("UNOP\n");
	struct UNOP *unop = (struct UNOP *) malloc (sizeof (struct UNOP));
	unop->u = eNegative;
	$$ = unop;
}
	;

Addi	: PLUS {
	//printf("PLUS\n");
	struct ADDIOP *addi = (struct ADDIOP *) malloc (sizeof (struct ADDIOP));
	addi->a = ePlus;
	$$ = addi;
}
	| MINU {
	//printf("MINU\n");
	struct ADDIOP *addi = (struct ADDIOP *) malloc (sizeof (struct ADDIOP));
	addi->a = eMinus;
	$$ = addi;
}
	;

Mult	: MULT {
	//printf("MULT\n");
	struct MULTOP *mult = (struct MULTOP *) malloc (sizeof (struct MULTOP));
	mult->m = eMulti;
	$$ = mult;
}
	| DIVI {
	//printf("DIVI\n");
	struct MULTOP *mult = (struct MULTOP *) malloc (sizeof (struct MULTOP));
	mult->m = eDiv;
	$$ = mult;
}
	;

Rela	: RELT {
	//printf("RELT\n");
	struct RELAOP *rela = (struct RELAOP *) malloc (sizeof (struct RELAOP));
	rela->r = eLT;
	$$ = rela;
}
	| REGT {
	//printf ("REGT\n");
	struct RELAOP *rela = (struct RELAOP *) malloc (sizeof (struct RELAOP));
	rela->r = eGT;
	$$ = rela;
}
	| RELE {
	//printf ("RELE\n");
	struct RELAOP *rela = (struct RELAOP *) malloc (sizeof (struct RELAOP));
	rela->r = eLE;
	$$ = rela;
}
	| REGE {
	//printf ("REGE\n");
	struct RELAOP *rela = (struct RELAOP *) malloc (sizeof (struct RELAOP));
	rela->r = eGE;
	$$ = rela;
}
	;

Eqlt	: EQUL {
	//printf ("EQUL\n");
	struct EQLTOP *eqlt = (struct EQLTOP *) malloc (sizeof (struct EQLTOP));
	eqlt->e = eEQ;
	$$ = eqlt;
}
	| UNEQ {
	//printf ("UNEQ\n");
	struct EQLTOP *eqlt = (struct EQLTOP *) malloc (sizeof (struct EQLTOP));
	eqlt->e = eNE;
	$$ = eqlt;
}
	;

%%

int yyerror (char *s) {
	return fprintf (stderr, "%s\n", s);
}
