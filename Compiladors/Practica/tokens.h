#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: fonts.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define PLAYFONT 2
#define ABS0 3
#define ABS1 4
#define ABS2 5
#define ORD0 6
#define MULT 7
#define MOVE 8
#define AREA 9
#define ALTU 10
#define PO 11
#define PC 12
#define CO 13
#define CC 14
#define COMA 15
#define BLAU 16
#define VERM 17
#define GROC 18
#define VERD 19
#define ON 20
#define OFF 21
#define NUM 22
#define ID 23
#define SPACE 24

#ifdef __USE_PROTOS
void fonts(AST**_root);
#else
extern void fonts();
#endif

#ifdef __USE_PROTOS
void defs(AST**_root);
#else
extern void defs();
#endif

#ifdef __USE_PROTOS
void play(AST**_root);
#else
extern void play();
#endif

#ifdef __USE_PROTOS
void defs_rule(AST**_root);
#else
extern void defs_rule();
#endif

#ifdef __USE_PROTOS
void font_eval(AST**_root);
#else
extern void font_eval();
#endif

#ifdef __USE_PROTOS
void font_abs(AST**_root);
#else
extern void font_abs();
#endif

#ifdef __USE_PROTOS
void font_ord(AST**_root);
#else
extern void font_ord();
#endif

#ifdef __USE_PROTOS
void font_def(AST**_root);
#else
extern void font_def();
#endif

#ifdef __USE_PROTOS
void font_color(AST**_root);
#else
extern void font_color();
#endif

#ifdef __USE_PROTOS
void play_rule(AST**_root);
#else
extern void play_rule();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType setwd1[];
extern SetWordType zzerr3[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType zzerr6[];
extern SetWordType setwd2[];
