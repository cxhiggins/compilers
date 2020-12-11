/* Generated by oblink */

#include "primtab.h"
#include <stdio.h>

value *P_scratch_alloc(value *bp) {
     value *sp = bp;
     FPINIT;
     res_P(scratch_alloc(arg_I(1)));
     return sp;
}

type_I lib_open_in(type_P);

value *P_lib_open_in(value *bp) {
     value *sp = bp;
     FPINIT;
     res_I(lib_open_in(arg_P(1)));
     return sp;
}

type_V lib_close_in();

value *P_lib_close_in(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_close_in());
     return sp;
}

type_V lib_read_char(type_P);

value *P_lib_read_char(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_read_char(arg_P(1)));
     return sp;
}

type_V lib_print_num(type_I);

value *P_lib_print_num(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_print_num(arg_I(1)));
     return sp;
}

type_V lib_print_string(type_P);

value *P_lib_print_string(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_print_string(arg_P(1)));
     return sp;
}

type_V lib_print_char(type_C);

value *P_lib_print_char(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_print_char(arg_C(1)));
     return sp;
}

type_I lib_argc();

value *P_lib_argc(value *bp) {
     value *sp = bp;
     FPINIT;
     res_I(lib_argc());
     return sp;
}

type_V lib_argv(type_I, type_P);

value *P_lib_argv(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(lib_argv(arg_I(1), arg_P(2)));
     return sp;
}

type_V exit(type_I);

value *P_exit(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(exit(arg_I(1)));
     return sp;
}

void lib_gc_alloc(value *bp);

type_V gc_debug(type_P);

value *P_gc_debug(value *bp) {
     value *sp = bp;
     FPINIT;
     res_V(gc_debug(arg_P(1)));
     return sp;
}

#ifndef DYNLINK
struct primdef primtab[] = {
     { "P_scratch_alloc", P_scratch_alloc },
     { "P_lib_open_in", P_lib_open_in },
     { "P_lib_close_in", P_lib_close_in },
     { "P_lib_read_char", P_lib_read_char },
     { "P_lib_print_num", P_lib_print_num },
     { "P_lib_print_string", P_lib_print_string },
     { "P_lib_print_char", P_lib_print_char },
     { "P_lib_argc", P_lib_argc },
     { "P_lib_argv", P_lib_argv },
     { "P_exit", P_exit },
     { "lib_gc_alloc", lib_gc_alloc },
     { "P_gc_debug", P_gc_debug },
     { NULL, NULL }
};
#endif
