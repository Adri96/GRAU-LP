#header 
<< #include "charptr.h" >>

<<
#include "charptr.c"

int main() {
   ANTLR(expr(), stdin);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token SPACE "[\ \n]" << zzskip(); >>


input: expr "@" ;

expr: NUM ( | plus_expr | minus_expr ) ;
plus_expr: PLUS expr ;
minus_expr: MINUS expr ;

