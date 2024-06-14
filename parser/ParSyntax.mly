/* File generated by the BNF Converter (bnfc 2.9.5). */

/* Parser definition for use with menhir */

%{
open AbsSyntax
open Lexing
%}

%token KW_language KW_core KW_extend KW_with KW_fn KW_return KW_generic KW_type KW_exception KW_variant KW_inline KW_throws KW_as KW_inl KW_inr KW_cons KW_false KW_true KW_unit KW_succ KW_if KW_then KW_else KW_let KW_in KW_letrec KW_cast KW_match KW_or KW_and KW_new KW_throw KW_try KW_catch KW_not KW_fix KW_fold KW_unfold KW_forall KW_Bool KW_Nat KW_Unit KW_Top KW_Bot

%token SYMB1 /* µ */
%token SYMB2 /* , */
%token SYMB3 /* ; */
%token SYMB4 /* ( */
%token SYMB5 /* ) */
%token SYMB6 /* { */
%token SYMB7 /* } */
%token SYMB8 /* [ */
%token SYMB9 /* ] */
%token SYMB10 /* = */
%token SYMB11 /* : */
%token SYMB12 /* -> */
%token SYMB13 /* => */
%token SYMB14 /* | */
%token SYMB15 /* <| */
%token SYMB16 /* |> */
%token SYMB17 /* := */
%token SYMB18 /* < */
%token SYMB19 /* <= */
%token SYMB20 /* > */
%token SYMB21 /* >= */
%token SYMB22 /* == */
%token SYMB23 /* != */
%token SYMB24 /* + */
%token SYMB25 /* - */
%token SYMB26 /* * */
%token SYMB27 /* / */
%token SYMB28 /* . */
%token SYMB29 /* List::head */
%token SYMB30 /* List::isempty */
%token SYMB31 /* List::tail */
%token SYMB32 /* panic! */
%token SYMB33 /* Nat::pred */
%token SYMB34 /* Nat::iszero */
%token SYMB35 /* Nat::rec */
%token SYMB36 /* & */

%token TOK_EOF
%token <string> TOK_Ident
%token <char>   TOK_Char
%token <float>  TOK_Double
%token <int>    TOK_Integer
%token <string> TOK_String
%token <string> TOK_StellaIdent
%token <string> TOK_ExtensionName
%token <string> TOK_MemoryAddress

%start pProgram pStellaIdent_list pLanguageDecl pExtension pExtensionName_list pExtension_list pDecl pDecl_list pLocalDecl pLocalDecl_list pAnnotation pAnnotation_list pParamDecl pParamDecl_list pReturnType pThrowType pType9 pType9_list pMatchCase pMatchCase_list pOptionalTyping pPatternData pExprData pPattern pPattern_list pLabelledPattern pLabelledPattern_list pBinding pBinding_list pExpr pExpr_list pExpr1 pPatternBinding pPatternBinding_list pExpr2 pExpr2_list pExpr3 pExpr4 pExpr5 pExpr6 pExpr7 pTypeT pType1 pType2 pType3 pTypeT_list pVariantFieldType pVariantFieldType_list pRecordFieldType pRecordFieldType_list pTyping
%type <AbsSyntax.program> pProgram
%type <AbsSyntax.stellaIdent list> pStellaIdent_list
%type <AbsSyntax.languageDecl> pLanguageDecl
%type <AbsSyntax.extension> pExtension
%type <AbsSyntax.extensionName list> pExtensionName_list
%type <AbsSyntax.extension list> pExtension_list
%type <AbsSyntax.decl> pDecl
%type <AbsSyntax.decl list> pDecl_list
%type <AbsSyntax.localDecl> pLocalDecl
%type <AbsSyntax.localDecl list> pLocalDecl_list
%type <AbsSyntax.annotation> pAnnotation
%type <AbsSyntax.annotation list> pAnnotation_list
%type <AbsSyntax.paramDecl> pParamDecl
%type <AbsSyntax.paramDecl list> pParamDecl_list
%type <AbsSyntax.returnType> pReturnType
%type <AbsSyntax.throwType> pThrowType
%type <AbsSyntax.typeT> pType9
%type <AbsSyntax.typeT list> pType9_list
%type <AbsSyntax.matchCase> pMatchCase
%type <AbsSyntax.matchCase list> pMatchCase_list
%type <AbsSyntax.optionalTyping> pOptionalTyping
%type <AbsSyntax.patternData> pPatternData
%type <AbsSyntax.exprData> pExprData
%type <AbsSyntax.pattern> pPattern
%type <AbsSyntax.pattern list> pPattern_list
%type <AbsSyntax.labelledPattern> pLabelledPattern
%type <AbsSyntax.labelledPattern list> pLabelledPattern_list
%type <AbsSyntax.binding> pBinding
%type <AbsSyntax.binding list> pBinding_list
%type <AbsSyntax.expr> pExpr
%type <AbsSyntax.expr list> pExpr_list
%type <AbsSyntax.expr> pExpr1
%type <AbsSyntax.patternBinding> pPatternBinding
%type <AbsSyntax.patternBinding list> pPatternBinding_list
%type <AbsSyntax.expr> pExpr2
%type <AbsSyntax.expr list> pExpr2_list
%type <AbsSyntax.expr> pExpr3
%type <AbsSyntax.expr> pExpr4
%type <AbsSyntax.expr> pExpr5
%type <AbsSyntax.expr> pExpr6
%type <AbsSyntax.expr> pExpr7
%type <AbsSyntax.typeT> pTypeT
%type <AbsSyntax.typeT> pType1
%type <AbsSyntax.typeT> pType2
%type <AbsSyntax.typeT> pType3
%type <AbsSyntax.typeT list> pTypeT_list
%type <AbsSyntax.variantFieldType> pVariantFieldType
%type <AbsSyntax.variantFieldType list> pVariantFieldType_list
%type <AbsSyntax.recordFieldType> pRecordFieldType
%type <AbsSyntax.recordFieldType list> pRecordFieldType_list
%type <AbsSyntax.typing> pTyping

%type <AbsSyntax.program> program
%type <AbsSyntax.stellaIdent list> stellaIdent_list
%type <AbsSyntax.languageDecl> languageDecl
%type <AbsSyntax.extension> extension
%type <AbsSyntax.extensionName list> extensionName_list
%type <AbsSyntax.extension list> extension_list
%type <AbsSyntax.decl> decl
%type <AbsSyntax.decl list> decl_list
%type <AbsSyntax.localDecl> localDecl
%type <AbsSyntax.localDecl list> localDecl_list
%type <AbsSyntax.annotation> annotation
%type <AbsSyntax.annotation list> annotation_list
%type <AbsSyntax.paramDecl> paramDecl
%type <AbsSyntax.paramDecl list> paramDecl_list
%type <AbsSyntax.returnType> returnType
%type <AbsSyntax.throwType> throwType
%type <AbsSyntax.typeT> type9
%type <AbsSyntax.typeT list> type9_list
%type <AbsSyntax.matchCase> matchCase
%type <AbsSyntax.matchCase list> matchCase_list
%type <AbsSyntax.optionalTyping> optionalTyping
%type <AbsSyntax.patternData> patternData
%type <AbsSyntax.exprData> exprData
%type <AbsSyntax.pattern> pattern
%type <AbsSyntax.pattern list> pattern_list
%type <AbsSyntax.labelledPattern> labelledPattern
%type <AbsSyntax.labelledPattern list> labelledPattern_list
%type <AbsSyntax.binding> binding
%type <AbsSyntax.binding list> binding_list
%type <AbsSyntax.expr> expr
%type <AbsSyntax.expr list> expr_list
%type <AbsSyntax.expr> expr1
%type <AbsSyntax.patternBinding> patternBinding
%type <AbsSyntax.patternBinding list> patternBinding_list
%type <AbsSyntax.expr> expr2
%type <AbsSyntax.expr list> expr2_list
%type <AbsSyntax.expr> expr3
%type <AbsSyntax.expr> expr4
%type <AbsSyntax.expr> expr5
%type <AbsSyntax.expr> expr6
%type <AbsSyntax.expr> expr7
%type <AbsSyntax.typeT> typeT
%type <AbsSyntax.typeT> type1
%type <AbsSyntax.typeT> type2
%type <AbsSyntax.typeT> type3
%type <AbsSyntax.typeT list> typeT_list
%type <AbsSyntax.variantFieldType> variantFieldType
%type <AbsSyntax.variantFieldType list> variantFieldType_list
%type <AbsSyntax.recordFieldType> recordFieldType
%type <AbsSyntax.recordFieldType list> recordFieldType_list
%type <AbsSyntax.typing> typing

%type <int> int
%type <AbsSyntax.stellaIdent> stellaIdent
%type <AbsSyntax.extensionName> extensionName
%type <AbsSyntax.memoryAddress> memoryAddress

%%

pProgram : program TOK_EOF { $1 };

pStellaIdent_list : stellaIdent_list TOK_EOF { $1 };

pLanguageDecl : languageDecl TOK_EOF { $1 };

pExtension : extension TOK_EOF { $1 };

pExtensionName_list : extensionName_list TOK_EOF { $1 };

pExtension_list : extension_list TOK_EOF { $1 };

pDecl : decl TOK_EOF { $1 };

pDecl_list : decl_list TOK_EOF { $1 };

pLocalDecl : localDecl TOK_EOF { $1 };

pLocalDecl_list : localDecl_list TOK_EOF { $1 };

pAnnotation : annotation TOK_EOF { $1 };

pAnnotation_list : annotation_list TOK_EOF { $1 };

pParamDecl : paramDecl TOK_EOF { $1 };

pParamDecl_list : paramDecl_list TOK_EOF { $1 };

pReturnType : returnType TOK_EOF { $1 };

pThrowType : throwType TOK_EOF { $1 };

pType9 : type9 TOK_EOF { $1 };

pType9_list : type9_list TOK_EOF { $1 };

pMatchCase : matchCase TOK_EOF { $1 };

pMatchCase_list : matchCase_list TOK_EOF { $1 };

pOptionalTyping : optionalTyping TOK_EOF { $1 };

pPatternData : patternData TOK_EOF { $1 };

pExprData : exprData TOK_EOF { $1 };

pPattern : pattern TOK_EOF { $1 };

pPattern_list : pattern_list TOK_EOF { $1 };

pLabelledPattern : labelledPattern TOK_EOF { $1 };

pLabelledPattern_list : labelledPattern_list TOK_EOF { $1 };

pBinding : binding TOK_EOF { $1 };

pBinding_list : binding_list TOK_EOF { $1 };

pExpr : expr TOK_EOF { $1 };

pExpr_list : expr_list TOK_EOF { $1 };

pExpr1 : expr1 TOK_EOF { $1 };

pPatternBinding : patternBinding TOK_EOF { $1 };

pPatternBinding_list : patternBinding_list TOK_EOF { $1 };

pExpr2 : expr2 TOK_EOF { $1 };

pExpr2_list : expr2_list TOK_EOF { $1 };

pExpr3 : expr3 TOK_EOF { $1 };

pExpr4 : expr4 TOK_EOF { $1 };

pExpr5 : expr5 TOK_EOF { $1 };

pExpr6 : expr6 TOK_EOF { $1 };

pExpr7 : expr7 TOK_EOF { $1 };

pTypeT : typeT TOK_EOF { $1 };

pType1 : type1 TOK_EOF { $1 };

pType2 : type2 TOK_EOF { $1 };

pType3 : type3 TOK_EOF { $1 };

pTypeT_list : typeT_list TOK_EOF { $1 };

pVariantFieldType : variantFieldType TOK_EOF { $1 };

pVariantFieldType_list : variantFieldType_list TOK_EOF { $1 };

pRecordFieldType : recordFieldType TOK_EOF { $1 };

pRecordFieldType_list : recordFieldType_list TOK_EOF { $1 };

pTyping : typing TOK_EOF { $1 };

program : languageDecl extension_list decl_list { AProgram ($1, $2, $3) }
  ;

stellaIdent_list : /* empty */ { []  }
  | stellaIdent { (fun x -> [x]) $1 }
  | stellaIdent SYMB2 stellaIdent_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

languageDecl : KW_language KW_core SYMB3 { LanguageCore  }
  ;

extension : KW_extend KW_with extensionName_list { AnExtension $3 }
  ;

extensionName_list : /* empty */ { []  }
  | extensionName { (fun x -> [x]) $1 }
  | extensionName SYMB2 extensionName_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

extension_list : /* empty */ { []  }
  | extension SYMB3 extension_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

decl : annotation_list KW_fn stellaIdent SYMB4 paramDecl_list SYMB5 returnType throwType SYMB6 decl_list KW_return expr SYMB7 { DeclFun ($1, $3, $5, $7, $8, $10, $12) }
  | annotation_list KW_generic KW_fn stellaIdent SYMB8 stellaIdent_list SYMB9 SYMB4 paramDecl_list SYMB5 returnType throwType SYMB6 decl_list KW_return expr SYMB7 { DeclFunGeneric ($1, $4, $6, $9, $11, $12, $14, $16) }
  | KW_type stellaIdent SYMB10 typeT { DeclTypeAlias ($2, $4) }
  | KW_exception KW_type SYMB10 typeT { DeclExceptionType $4 }
  | KW_exception KW_variant stellaIdent SYMB11 typeT { DeclExceptionVariant ($3, $5) }
  ;

decl_list : /* empty */ { []  }
  | decl decl_list { (fun (x,xs) -> x::xs) ($1, $2) }
  ;

localDecl : decl { ALocalDecl $1 }
  ;

localDecl_list : /* empty */ { []  }
  | localDecl SYMB3 localDecl_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

annotation : KW_inline { InlineAnnotation  }
  ;

annotation_list : /* empty */ { []  }
  | annotation annotation_list { (fun (x,xs) -> x::xs) ($1, $2) }
  ;

paramDecl : stellaIdent SYMB11 typeT { AParamDecl ($1, $3) }
  ;

paramDecl_list : /* empty */ { []  }
  | paramDecl { (fun x -> [x]) $1 }
  | paramDecl SYMB2 paramDecl_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

returnType : /* empty */ { NoReturnType  }
  | SYMB12 typeT { SomeReturnType $2 }
  ;

throwType : /* empty */ { NoThrowType  }
  | KW_throws type9_list { SomeThrowType $2 }
  ;

type9 : typeT {  $1 }
  ;

type9_list : type9 { (fun x -> [x]) $1 }
  | type9 SYMB2 type9_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

matchCase : pattern SYMB13 expr { AMatchCase ($1, $3) }
  ;

matchCase_list : /* empty */ { []  }
  | matchCase { (fun x -> [x]) $1 }
  | matchCase SYMB14 matchCase_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

optionalTyping : /* empty */ { NoTyping  }
  | SYMB11 typeT { SomeTyping $2 }
  ;

patternData : /* empty */ { NoPatternData  }
  | SYMB10 pattern { SomePatternData $2 }
  ;

exprData : /* empty */ { NoExprData  }
  | SYMB10 expr { SomeExprData $2 }
  ;

pattern : pattern KW_as typeT { PatternAsc ($1, $3) }
  | SYMB15 stellaIdent patternData SYMB16 { PatternVariant ($2, $3) }
  | KW_inl SYMB4 pattern SYMB5 { PatternInl $3 }
  | KW_inr SYMB4 pattern SYMB5 { PatternInr $3 }
  | SYMB6 pattern_list SYMB7 { PatternTuple $2 }
  | SYMB6 labelledPattern_list SYMB7 { PatternRecord $2 }
  | SYMB8 pattern_list SYMB9 { PatternList $2 }
  | KW_cons SYMB4 pattern SYMB2 pattern SYMB5 { PatternCons ($3, $5) }
  | SYMB4 pattern SYMB2 pattern SYMB5 { patternCons ($2, $4) }
  | KW_false { PatternFalse  }
  | KW_true { PatternTrue  }
  | KW_unit { PatternUnit  }
  | int { PatternInt $1 }
  | KW_succ SYMB4 pattern SYMB5 { PatternSucc $3 }
  | stellaIdent { PatternVar $1 }
  | SYMB4 pattern SYMB5 {  $2 }
  ;

pattern_list : /* empty */ { []  }
  | pattern { (fun x -> [x]) $1 }
  | pattern SYMB2 pattern_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

labelledPattern : stellaIdent SYMB10 pattern { ALabelledPattern ($1, $3) }
  ;

labelledPattern_list : labelledPattern { (fun x -> [x]) $1 }
  | labelledPattern SYMB2 labelledPattern_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

binding : stellaIdent SYMB10 expr { ABinding ($1, $3) }
  ;

binding_list : binding { (fun x -> [x]) $1 }
  | binding SYMB2 binding_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

expr : expr1 SYMB3 expr { Sequence ($1, $3) }
  | expr1 SYMB3 {  $1 }
  | KW_let patternBinding_list KW_in expr { Let ($2, $4) }
  | KW_letrec patternBinding_list KW_in expr { LetRec ($2, $4) }
  | KW_generic SYMB8 stellaIdent_list SYMB9 expr { TypeAbstraction ($3, $5) }
  | expr1 {  $1 }
  ;

expr_list : /* empty */ { []  }
  | expr { (fun x -> [x]) $1 }
  | expr SYMB2 expr_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

expr1 : expr2 SYMB17 expr1 { Assign ($1, $3) }
  | KW_if expr1 KW_then expr1 KW_else expr1 { If ($2, $4, $6) }
  | expr2 {  $1 }
  ;

patternBinding : pattern SYMB10 expr { APatternBinding ($1, $3) }
  ;

patternBinding_list : patternBinding { (fun x -> [x]) $1 }
  | patternBinding SYMB2 patternBinding_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

expr2 : expr3 SYMB18 expr3 { LessThan ($1, $3) }
  | expr3 SYMB19 expr3 { LessThanOrEqual ($1, $3) }
  | expr3 SYMB20 expr3 { GreaterThan ($1, $3) }
  | expr3 SYMB21 expr3 { GreaterThanOrEqual ($1, $3) }
  | expr3 SYMB22 expr3 { Equal ($1, $3) }
  | expr3 SYMB23 expr3 { NotEqual ($1, $3) }
  | expr3 {  $1 }
  ;

expr2_list : expr2 SYMB3 { (fun x -> [x]) $1 }
  | expr2 SYMB3 expr2_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

expr3 : expr3 KW_as type2 { TypeAsc ($1, $3) }
  | expr3 KW_cast KW_as type2 { TypeCast ($1, $4) }
  | KW_fn SYMB4 paramDecl_list SYMB5 SYMB6 KW_return expr SYMB7 { Abstraction ($3, $7) }
  | SYMB15 stellaIdent exprData SYMB16 { Variant ($2, $3) }
  | KW_match expr2 SYMB6 matchCase_list SYMB7 { Match ($2, $4) }
  | SYMB8 expr_list SYMB9 { List $2 }
  | expr3 SYMB24 expr4 { Add ($1, $3) }
  | expr3 SYMB25 expr4 { Subtract ($1, $3) }
  | expr3 KW_or expr4 { LogicOr ($1, $3) }
  | expr4 {  $1 }
  ;

expr4 : expr4 SYMB26 expr5 { Multiply ($1, $3) }
  | expr4 SYMB27 expr5 { Divide ($1, $3) }
  | expr4 KW_and expr5 { LogicAnd ($1, $3) }
  | expr5 {  $1 }
  ;

expr5 : KW_new SYMB4 expr SYMB5 { Ref $3 }
  | SYMB26 expr5 { Deref $2 }
  | expr6 {  $1 }
  ;

expr6 : expr6 SYMB4 expr_list SYMB5 { Application ($1, $3) }
  | expr6 SYMB8 typeT_list SYMB9 { TypeApplication ($1, $3) }
  | expr6 SYMB28 stellaIdent { DotRecord ($1, $3) }
  | expr6 SYMB28 int { DotTuple ($1, $3) }
  | SYMB6 expr_list SYMB7 { Tuple $2 }
  | SYMB6 binding_list SYMB7 { Record $2 }
  | KW_cons SYMB4 expr SYMB2 expr SYMB5 { ConsList ($3, $5) }
  | SYMB29 SYMB4 expr SYMB5 { Head $3 }
  | SYMB30 SYMB4 expr SYMB5 { IsEmpty $3 }
  | SYMB31 SYMB4 expr SYMB5 { Tail $3 }
  | SYMB32 { Panic  }
  | KW_throw SYMB4 expr SYMB5 { Throw $3 }
  | KW_try SYMB6 expr SYMB7 KW_catch SYMB6 pattern SYMB13 expr SYMB7 { TryCatch ($3, $7, $9) }
  | KW_try SYMB6 expr SYMB7 KW_with SYMB6 expr SYMB7 { TryWith ($3, $7) }
  | KW_inl SYMB4 expr SYMB5 { Inl $3 }
  | KW_inr SYMB4 expr SYMB5 { Inr $3 }
  | KW_succ SYMB4 expr SYMB5 { Succ $3 }
  | KW_not SYMB4 expr SYMB5 { LogicNot $3 }
  | SYMB33 SYMB4 expr SYMB5 { Pred $3 }
  | SYMB34 SYMB4 expr SYMB5 { IsZero $3 }
  | KW_fix SYMB4 expr SYMB5 { Fix $3 }
  | SYMB35 SYMB4 expr SYMB2 expr SYMB2 expr SYMB5 { NatRec ($3, $5, $7) }
  | KW_fold SYMB8 typeT SYMB9 expr7 { Fold ($3, $5) }
  | KW_unfold SYMB8 typeT SYMB9 expr7 { Unfold ($3, $5) }
  | expr7 {  $1 }
  ;

expr7 : KW_true { ConstTrue  }
  | KW_false { ConstFalse  }
  | KW_unit { ConstUnit  }
  | int { ConstInt $1 }
  | memoryAddress { ConstMemory $1 }
  | stellaIdent { Var $1 }
  | SYMB4 expr SYMB5 {  $2 }
  ;

typeT : KW_fn SYMB4 typeT_list SYMB5 SYMB12 typeT { TypeFun ($3, $6) }
  | KW_forall stellaIdent_list SYMB28 typeT { TypeForAll ($2, $4) }
  | SYMB1 stellaIdent SYMB28 typeT { TypeRec ($2, $4) }
  | type1 {  $1 }
  ;

type1 : type2 SYMB24 type2 { TypeSum ($1, $3) }
  | type2 {  $1 }
  ;

type2 : SYMB6 typeT_list SYMB7 { TypeTuple $2 }
  | SYMB6 recordFieldType_list SYMB7 { TypeRecord $2 }
  | SYMB15 variantFieldType_list SYMB16 { TypeVariant $2 }
  | SYMB8 typeT SYMB9 { TypeList $2 }
  | type3 {  $1 }
  ;

type3 : KW_Bool { TypeBool  }
  | KW_Nat { TypeNat  }
  | KW_Unit { TypeUnit  }
  | KW_Top { TypeTop  }
  | KW_Bot { TypeBottom  }
  | SYMB36 type2 { TypeRef $2 }
  | stellaIdent { TypeVar $1 }
  | SYMB4 typeT SYMB5 {  $2 }
  ;

typeT_list : /* empty */ { []  }
  | typeT { (fun x -> [x]) $1 }
  | typeT SYMB2 typeT_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

variantFieldType : stellaIdent optionalTyping { AVariantFieldType ($1, $2) }
  ;

variantFieldType_list : /* empty */ { []  }
  | variantFieldType { (fun x -> [x]) $1 }
  | variantFieldType SYMB2 variantFieldType_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

recordFieldType : stellaIdent SYMB11 typeT { ARecordFieldType ($1, $3) }
  ;

recordFieldType_list : recordFieldType { (fun x -> [x]) $1 }
  | recordFieldType SYMB2 recordFieldType_list { (fun (x,xs) -> x::xs) ($1, $3) }
  ;

typing : expr SYMB11 typeT { ATyping ($1, $3) }
  ;

int :  TOK_Integer  { $1 };
stellaIdent : TOK_StellaIdent { StellaIdent ($1)};
extensionName : TOK_ExtensionName { ExtensionName ($1)};
memoryAddress : TOK_MemoryAddress { MemoryAddress ($1)};

