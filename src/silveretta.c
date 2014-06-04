
#include "debug.h"
#include "read.h"
#include "print.h"
#include "eval.h"
#include "val.h"
#include "types.h"
#include "variable.h"
#include "mpc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linedit.h>
int main(int argc, char** argv) {
mpc_parser_t* long_num = mpc_new("long_num");
mpc_parser_t* symbol = mpc_new("symbol");
mpc_parser_t* list = mpc_new("list");
mpc_parser_t* sexp = mpc_new("sexp");
mpc_parser_t* silveretta = mpc_new("silveretta");

mpca_lang(MPC_LANG_DEFAULT, "                                                      \
               long_num   : /-?[0-9]+/                             ;  \
               symbol     : /[a-z]+/ | '+' | '-' | '*' | '/'       ;  \
               list       : '(' <symbol>* <sexp>* ')'              ;  \
               sexp       : <long_num> | <symbol> | <list>         ;  \
               silveretta : /^/ <sexp>* /$/                        ;  \
               ", long_num, symbol, list, sexp, silveretta);
puts("Silveretta Version 2.0a");
puts("Press Ctrl+c to Exit\n");
while (1LL) {
char* prompt = "ag>> ";
char* input = readline(prompt);

add_history(input);
mpc_result_t r;

if (mpc_parse("<stdin>", input, silveretta, (&r))) {
ag_val* read_result = ag_read((r.output));
env* e = malloc(sizeof(env));
ag_val* eval_result = ag_eval(read_result, e);

ag_print(eval_result);
mpc_ast_delete((r.output));
del_env(e);
}
else {
dputln("There was an error with the AST.");
mpc_err_print((r.error));
mpr_err_delete((r.error));
}

free(input);
}

mpc_cleanup(5LL, long_num, symbol, list, sexp, silveretta);
return EXIT_SUCCESS;
}

 