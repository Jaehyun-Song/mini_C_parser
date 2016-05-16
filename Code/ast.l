%{
#include <stdlib.h>
#include <string.h>
#include "ast.tab.h"
%}

intnum [0-9]+
floatnum [0-9]+"."[0-9]+
id [A-Za-z_][A-Za-z0-9_]*
space [ \t\r\n]

%%

{space} {
	break;
}

"int" {
	//printf ("int\n");
	return INT;
}

"float" {
	//printf ("float\n");
	return FLOAT;
}

"return" {
	//printf ("return\n");
	return RETURN;
}

"while" {
	//printf ("while\n");
	return WHILE;
}

"do" {
	//printf ("do\n");
	return DO;
}

"for" {
	//printf ("for\n");
	return FOR;
}

"if" {
	//printf ("if\n");
	return IF;
}

"else" {
	//printf ("else\n");
	return ELSE;
}

"switch" {
	//printf ("switch\n");
	return SWITCH;
}

"case" {
	//printf ("case\n");
	return CASE;
}

"break" {
	//printf ("break\n");
	return BREAK;
}

"default" {
	//printf ("default\n");
	return DEFAULT;
}

"+" {
	//printf ("+\n");
	return PLUS;
}

"-" {
	//printf ("-\n");
	return MINU;
}

"*" {
	//printf ("*\n");
	return MULT;
}

"/" {
	//printf ("/\n");
	return DIVI;
}

"<" {
	//printf ("<\n");
	return RELT;
}

">" {
	//printf (">\n");
	return REGT;
}

"<=" {
	//printf ("<=\n");
	return RELE;
}

">=" {
	//printf (">=\n");
	return REGE;
}

"==" {
	//printf ("==\n");
	return EQUL;
}

"!=" {
	//printf ("!=\n");
	return UNEQ;
}

"=" {
	//printf ("=\n");
	return ASGN;
}

"," {
	//printf (",\n");
	return COMMA;
}

"[" {
	//printf ("[\n");
	return LSBT;
}

"]" {
	//printf ("]\n");
	return RSBT;
}

"(" {
	//printf ("(\n");
	return LBKT;
}

")" {
	//printf (")\n");
	return RBKT;
}

"{" {
	//printf ("{\n");
	return LBRA;
}

"}" {
	//printf ("}\n");
	return RBRA;
}

";" {
	//printf (";\n");
	return SEMICOL;
}

":" {
	//printf (":\n");
	return COLON;
}

{id} {
	strcpy (yylval.identifier, yytext);
	//printf ("%s\n", yytext);
	return ID;
}

{intnum} {
	yylval.intnum = atoi (yytext);
	//printf ("%d\n", atoi (yytext));
	return INTNUM;
}

{floatnum} {
	yylval.floatnum = atof (yytext);
	//printf ("%f\n", atof (yytext));
	return FLOATNUM;
}

