all:
	flex ast.l
	bison -d ast.y
	gcc lex.yy.c ast.tab.c main.c -o ASTree -lfl

clean:
	rm -rf lex.yy.c ast.tab.c ast.tab.h ASTree sample.out
