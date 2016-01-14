/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: fonts.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

#include <string>
#include <iostream>
#include <vector>
using namespace std;

const double PI = 3.141592653589793238463;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
  int type;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text; int type;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
#define zzSET_SIZE 4
#include "antlr.h"
#include "ast.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[25]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"PLAYFONT",
	/* 03 */	"ABS0",
	/* 04 */	"ABS1",
	/* 05 */	"ABS2",
	/* 06 */	"ORD0",
	/* 07 */	"MULT",
	/* 08 */	"MOVE",
	/* 09 */	"AREA",
	/* 10 */	"ALTU",
	/* 11 */	"PO",
	/* 12 */	"PC",
	/* 13 */	"CO",
	/* 14 */	"CC",
	/* 15 */	"COMA",
	/* 16 */	"BLAU",
	/* 17 */	"VERM",
	/* 18 */	"GROC",
	/* 19 */	"VERD",
	/* 20 */	"ON",
	/* 21 */	"OFF",
	/* 22 */	"NUM",
	/* 23 */	"ID",
	/* 24 */	"SPACE"
};
SetWordType zzerr1[4] = {0x0,0x6,0x80,0x0};
SetWordType zzerr2[4] = {0x38,0x0,0x0,0x0};
SetWordType setwd1[25] = {0x0,0x9,0xd4,0xa0,0xa0,0xa0,0x0,
	0x0,0x0,0xd2,0xd2,0x0,0xc0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0xd2,0x0};
SetWordType zzerr3[4] = {0x0,0x28,0xc0,0x0};
SetWordType zzerr4[4] = {0x0,0x20,0x80,0x0};
SetWordType zzerr5[4] = {0x0,0x0,0xf,0x0};
SetWordType zzerr6[4] = {0x0,0x0,0x30,0x0};
SetWordType setwd2[25] = {0x0,0x10,0x6,0x6,0x6,0x6,0x6,
	0x0,0x0,0x6,0x6,0x0,0x6,0x1,0x8,
	0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x10,
	0x7,0x0};
