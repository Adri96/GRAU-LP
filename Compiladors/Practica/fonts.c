/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 *
 *   antlr -gt fonts.g
 *
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
#define GENAST

#include "ast.h"

#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "mode.h"

/* MR23 In order to remove calls to PURIFY use the antlr -nopurify option */

#ifndef PCCTS_PURIFY
#define PCCTS_PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif

#include "ast.c"
zzASTgvars

ANTLR_INFO

#include <cstdlib>
#include <cmath>

//global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  attr->kind = text;
  attr->text = "";
  attr->type = type;
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->type = attr->type;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
  AST *as=new AST;
  as->kind="list";
  as->right=NULL;
  as->down=child;
  return as;
}

AST *findFont(string id) {
  AST *n = root->down->down;
  while (n != NULL and (n->kind != "=" or n->down->kind != id)) n = n->right;
  if (n == NULL) {
    cout << "NOT FOUND: " << id << " " << endl;
    return NULL;
  }
  return n->down->right;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
  AST *c=a->down;
  for (int i=0; c!=NULL && i<n; i++) c=c->right;
  return c;
}

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s) {
  if (a==NULL) return;
  
  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;
  
  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a) {
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}


//-----------------------------------------------
// Definició font
//-----------------------------------------------

struct Font {
  int radi;
  int alt;
  string col;
  Font(AST *a) {
    radi = atoi(child(a, 0)->kind.c_str());
    alt = atoi(child(a, 1)->kind.c_str());
    col = child(a, 2)->kind;
  }
};


//-----------------------------------------------
// Pila inclinacions
//-----------------------------------------------

vector<int> pila;

inline bool inclinada() {
  int inc = pila.back();
  for (int i=pila.size()-2; i>=1; --i)
  if (pila[i]!=inc) inc+=pila[i];
  return inc;
}


//-----------------------------------------------
// Càlcul àrea
//-----------------------------------------------

double calcArea(AST *a, int fac) {
  if (a->type==CO) {
    Font f(a);                                        // definicio: [ radi, altura, color ]
    if (inclinada()) {
      double lado = fac*f.alt;                        // costat triangle
      double altura = sqrt(lado*lado-f.radi*f.radi);  // altura triangle isosceles
      return f.radi*altura;                           // area triangle: (base*altura) / 2
    } else {
      double radi = f.radi;                           // radi font
      return PI*radi*radi;                            // area cercle: PI r^2
    }
  } else if (a->type==ID) {
    return calcArea(findFont(a->kind), fac);
  } else if (a->type==MULT) {
    return calcArea(child(a,1), fac*atoi(child(a,0)->kind.c_str()));
  } else if (a->type==ORD0 || a->type==ABS0) {
    double area1 = calcArea(child(a,0), fac);
    double area2 = calcArea(child(a,1), fac);
    return area1+area2;
  } else if (a->type==ABS1 || a->type==ABS2) {
    double area1 = calcArea(child(a,0), fac);
    pila.push_back(a->type==ABS1 ? 1 : -1);
    double area2 = calcArea(child(a,1), fac);
    pila.pop_back();
    return area1+area2;
  }
  return -1.0;
}

double calcArea(AST *a) {
  return calcArea(a, 1);
}


//-----------------------------------------------
// Càlcul altura
//-----------------------------------------------

const double INC = sin(PI/4.0);

double calcAltura(AST *a, int fac) {
  if (a->type==CO) {
    Font f(a);
    double altura = inclinada() ? INC*f.alt : f.alt;
    return altura*fac;
  } else if (a->type==ID) {
    return calcAltura(findFont(a->kind), fac);
  } else if (a->type==MULT) {
    return calcAltura(child(a,1), fac*atoi(child(a,0)->kind.c_str()));
  } else if (a->type==ORD0 || a->type==ABS0) {
    double alt1 = calcAltura(child(a,0), fac);
    double alt2 = calcAltura(child(a,1), fac);
    return max(alt1, alt2);
  } else if (a->type==ABS1 || a->type==ABS2) {
    double alt1 = calcAltura(child(a,0), fac);
    pila.push_back(a->type==ABS1 ? 1 : -1);
    double alt2 = calcAltura(child(a,1), fac);
    pila.pop_back();
    return max(alt1, alt2);
  }
  return -1.0;
}

double calcAltura(AST *a) {
  return calcAltura(a, 1);
}


//-----------------------------------------------
// Evalua arbre
//-----------------------------------------------

void eval() {
  AST *a = root->down->down;
  while (a != NULL) {
    if (a->type==AREA) cout << "Area: " << calcArea(a->down) << endl;
    else if (a->type==ALTU) cout << "Altura: " << calcAltura(a->down) << endl;
    a = a->right;
  }
}

int main() {
  root = NULL;
  ANTLR(fonts(&root), stdin);
  ASTPrint(root);
  pila.push_back(0);
  eval();
}

void
#ifdef __USE_PROTOS
fonts(AST**_root)
#else
fonts(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  defs(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(PLAYFONT);  zzCONSUME;
  play(zzSTR); zzlink(_root, &_sibling, &_tail);
  (*_root)=createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x1);
  }
}

void
#ifdef __USE_PROTOS
defs(AST**_root)
#else
defs(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x2) ) {
      defs_rule(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root)=createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x4);
  }
}

void
#ifdef __USE_PROTOS
play(AST**_root)
#else
play(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==NUM) ) {
      play_rule(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root)=createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x8);
  }
}

void
#ifdef __USE_PROTOS
defs_rule(AST**_root)
#else
defs_rule(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(MOVE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==AREA) ) {
        zzmatch(AREA); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {
        if ( (LA(1)==ALTU) ) {
          zzmatch(ALTU); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {zzFAIL(1,zzerr1,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
      }
    }
    zzEXIT(zztasp2);
    }
  }
  font_eval(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x10);
  }
}

void
#ifdef __USE_PROTOS
font_eval(AST**_root)
#else
font_eval(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  font_abs(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x20) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==ABS0) ) {
          zzmatch(ABS0); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {
          if ( (LA(1)==ABS1) ) {
            zzmatch(ABS1); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {
            if ( (LA(1)==ABS2) ) {
              zzmatch(ABS2); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
            }
            else {zzFAIL(1,zzerr2,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
          }
        }
        zzEXIT(zztasp3);
        }
      }
      font_abs(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x40);
  }
}

void
#ifdef __USE_PROTOS
font_abs(AST**_root)
#else
font_abs(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  font_ord(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==ORD0) ) {
      zzmatch(ORD0); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      font_ord(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x80);
  }
}

void
#ifdef __USE_PROTOS
font_ord(AST**_root)
#else
font_ord(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (setwd2[LA(1)]&0x1) ) {
    font_def(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==NUM) ) {
      zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      zzmatch(MULT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      font_ord(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==PO) ) {
        zzmatch(PO);  zzCONSUME;
        font_eval(zzSTR); zzlink(_root, &_sibling, &_tail);
        zzmatch(PC);  zzCONSUME;
      }
      else {zzFAIL(1,zzerr3,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x2);
  }
}

void
#ifdef __USE_PROTOS
font_def(AST**_root)
#else
font_def(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==CO) ) {
    zzmatch(CO); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(COMA);  zzCONSUME;
    zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(COMA);  zzCONSUME;
    font_color(zzSTR); zzlink(_root, &_sibling, &_tail);
    zzmatch(CC);  zzCONSUME;
  }
  else {
    if ( (LA(1)==ID) ) {
      zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {zzFAIL(1,zzerr4,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x4);
  }
}

void
#ifdef __USE_PROTOS
font_color(AST**_root)
#else
font_color(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==VERD) ) {
    zzmatch(VERD); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (LA(1)==BLAU) ) {
      zzmatch(BLAU); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==VERM) ) {
        zzmatch(VERM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {
        if ( (LA(1)==GROC) ) {
          zzmatch(GROC); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {zzFAIL(1,zzerr5,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x8);
  }
}

void
#ifdef __USE_PROTOS
play_rule(AST**_root)
#else
play_rule(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ON) ) {
      zzmatch(ON); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==OFF) ) {
        zzmatch(OFF); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {zzFAIL(1,zzerr6,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x10);
  }
}
