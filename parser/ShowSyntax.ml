(* File generated by the BNF Converter (bnfc 2.9.5). *)

(* show functions *)

(* use string buffers for efficient string concatenations *)
type showable = Buffer.t -> unit

let show (s : showable) : string =
  let init_size = 16 in
  (* you may want to adjust this *)
  let b = Buffer.create init_size in
  s b;
  Buffer.contents b

let emptyS : showable = fun buf -> ()
let c2s (c : char) : showable = fun buf -> Buffer.add_char buf c
let s2s (s : string) : showable = fun buf -> Buffer.add_string buf s

let ( >> ) (s1 : showable) (s2 : showable) : showable =
 fun buf ->
  s1 buf;
  s2 buf

let showChar (c : char) : showable =
 fun buf -> Buffer.add_string buf ("'" ^ Char.escaped c ^ "'")

let showString (s : string) : showable =
 fun buf -> Buffer.add_string buf ("\"" ^ String.escaped s ^ "\"")

let showList (showFun : 'a -> showable) (xs : 'a list) : showable =
 fun buf ->
  let rec f ys =
    match ys with
    | [] -> ()
    | [ y ] -> showFun y buf
    | y :: ys ->
        showFun y buf;
        Buffer.add_string buf "; ";
        f ys
  in
  Buffer.add_char buf '[';
  f xs;
  Buffer.add_char buf ']'

let showInt (i : int) : showable = s2s (string_of_int i)
let showFloat (f : float) : showable = s2s (string_of_float f)

let rec showStellaIdent (AbsSyntax.StellaIdent i) : showable =
  s2s "StellaIdent " >> showString i

let rec showExtensionName (AbsSyntax.ExtensionName i) : showable =
  s2s "ExtensionName " >> showString i

let rec showMemoryAddress (AbsSyntax.MemoryAddress i) : showable =
  s2s "MemoryAddress " >> showString i

let rec showProgram (e : AbsSyntax.program) : showable =
  match e with
  | AbsSyntax.AProgram (languagedecl, extensions, decls) ->
      s2s "AProgram" >> c2s ' ' >> c2s '('
      >> showLanguageDecl languagedecl
      >> s2s ", "
      >> showList showExtension extensions
      >> s2s ", " >> showList showDecl decls >> c2s ')'

and showLanguageDecl (e : AbsSyntax.languageDecl) : showable =
  match e with AbsSyntax.LanguageCore -> s2s "LanguageCore"

and showExtension (e : AbsSyntax.extension) : showable =
  match e with
  | AbsSyntax.AnExtension extensionnames ->
      s2s "AnExtension" >> c2s ' ' >> c2s '('
      >> showList showExtensionName extensionnames
      >> c2s ')'

and showDecl (e : AbsSyntax.decl) : showable =
  match e with
  | AbsSyntax.DeclFun
      (annotations, stellaident, paramdecls, returntype, throwtype, decls, expr)
    ->
      s2s "DeclFun" >> c2s ' ' >> c2s '('
      >> showList showAnnotation annotations
      >> s2s ", "
      >> showStellaIdent stellaident
      >> s2s ", "
      >> showList showParamDecl paramdecls
      >> s2s ", " >> showReturnType returntype >> s2s ", "
      >> showThrowType throwtype >> s2s ", " >> showList showDecl decls
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.DeclFunGeneric
      ( annotations,
        stellaident,
        stellaidents,
        paramdecls,
        returntype,
        throwtype,
        decls,
        expr ) ->
      s2s "DeclFunGeneric" >> c2s ' ' >> c2s '('
      >> showList showAnnotation annotations
      >> s2s ", "
      >> showStellaIdent stellaident
      >> s2s ", "
      >> showList showStellaIdent stellaidents
      >> s2s ", "
      >> showList showParamDecl paramdecls
      >> s2s ", " >> showReturnType returntype >> s2s ", "
      >> showThrowType throwtype >> s2s ", " >> showList showDecl decls
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.DeclTypeAlias (stellaident, type') ->
      s2s "DeclTypeAlias" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showTypeT type' >> c2s ')'
  | AbsSyntax.DeclExceptionType type' ->
      s2s "DeclExceptionType" >> c2s ' ' >> c2s '(' >> showTypeT type'
      >> c2s ')'
  | AbsSyntax.DeclExceptionVariant (stellaident, type') ->
      s2s "DeclExceptionVariant" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showTypeT type' >> c2s ')'

and showLocalDecl (e : AbsSyntax.localDecl) : showable =
  match e with
  | AbsSyntax.ALocalDecl decl ->
      s2s "ALocalDecl" >> c2s ' ' >> c2s '(' >> showDecl decl >> c2s ')'

and showAnnotation (e : AbsSyntax.annotation) : showable =
  match e with AbsSyntax.InlineAnnotation -> s2s "InlineAnnotation"

and showParamDecl (e : AbsSyntax.paramDecl) : showable =
  match e with
  | AbsSyntax.AParamDecl (stellaident, type') ->
      s2s "AParamDecl" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showTypeT type' >> c2s ')'

and showReturnType (e : AbsSyntax.returnType) : showable =
  match e with
  | AbsSyntax.NoReturnType -> s2s "NoReturnType"
  | AbsSyntax.SomeReturnType type' ->
      s2s "SomeReturnType" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'

and showThrowType (e : AbsSyntax.throwType) : showable =
  match e with
  | AbsSyntax.NoThrowType -> s2s "NoThrowType"
  | AbsSyntax.SomeThrowType types ->
      s2s "SomeThrowType" >> c2s ' ' >> c2s '(' >> showList showTypeT types
      >> c2s ')'

and showTypeT (e : AbsSyntax.typeT) : showable =
  match e with
  | AbsSyntax.TypeFun (types, type') ->
      s2s "TypeFun" >> c2s ' ' >> c2s '(' >> showList showTypeT types
      >> s2s ", " >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeForAll (stellaidents, type') ->
      s2s "TypeForAll" >> c2s ' ' >> c2s '('
      >> showList showStellaIdent stellaidents
      >> s2s ", " >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeRec (stellaident, type') ->
      s2s "TypeRec" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeSum (type'0, type') ->
      s2s "TypeSum" >> c2s ' ' >> c2s '(' >> showTypeT type'0 >> s2s ", "
      >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeTuple types ->
      s2s "TypeTuple" >> c2s ' ' >> c2s '(' >> showList showTypeT types
      >> c2s ')'
  | AbsSyntax.TypeRecord recordfieldtypes ->
      s2s "TypeRecord" >> c2s ' ' >> c2s '('
      >> showList showRecordFieldType recordfieldtypes
      >> c2s ')'
  | AbsSyntax.TypeVariant variantfieldtypes ->
      s2s "TypeVariant" >> c2s ' ' >> c2s '('
      >> showList showVariantFieldType variantfieldtypes
      >> c2s ')'
  | AbsSyntax.TypeList type' ->
      s2s "TypeList" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeBool -> s2s "TypeBool"
  | AbsSyntax.TypeNat -> s2s "TypeNat"
  | AbsSyntax.TypeUnit -> s2s "TypeUnit"
  | AbsSyntax.TypeTop -> s2s "TypeTop"
  | AbsSyntax.TypeBottom -> s2s "TypeBottom"
  | AbsSyntax.TypeRef type' ->
      s2s "TypeRef" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeVar stellaident ->
      s2s "TypeVar" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> c2s ')'

and showMatchCase (e : AbsSyntax.matchCase) : showable =
  match e with
  | AbsSyntax.AMatchCase (pattern, expr) ->
      s2s "AMatchCase" >> c2s ' ' >> c2s '(' >> showPattern pattern >> s2s ", "
      >> showExpr expr >> c2s ')'

and showOptionalTyping (e : AbsSyntax.optionalTyping) : showable =
  match e with
  | AbsSyntax.NoTyping -> s2s "NoTyping"
  | AbsSyntax.SomeTyping type' ->
      s2s "SomeTyping" >> c2s ' ' >> c2s '(' >> showTypeT type' >> c2s ')'

and showPatternData (e : AbsSyntax.patternData) : showable =
  match e with
  | AbsSyntax.NoPatternData -> s2s "NoPatternData"
  | AbsSyntax.SomePatternData pattern ->
      s2s "SomePatternData" >> c2s ' ' >> c2s '(' >> showPattern pattern
      >> c2s ')'

and showExprData (e : AbsSyntax.exprData) : showable =
  match e with
  | AbsSyntax.NoExprData -> s2s "NoExprData"
  | AbsSyntax.SomeExprData expr ->
      s2s "SomeExprData" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'

and showPattern (e : AbsSyntax.pattern) : showable =
  match e with
  | AbsSyntax.PatternAsc (pattern, type') ->
      s2s "PatternAsc" >> c2s ' ' >> c2s '(' >> showPattern pattern >> s2s ", "
      >> showTypeT type' >> c2s ')'
  | AbsSyntax.PatternVariant (stellaident, patterndata) ->
      s2s "PatternVariant" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", "
      >> showPatternData patterndata
      >> c2s ')'
  | AbsSyntax.PatternInl pattern ->
      s2s "PatternInl" >> c2s ' ' >> c2s '(' >> showPattern pattern >> c2s ')'
  | AbsSyntax.PatternInr pattern ->
      s2s "PatternInr" >> c2s ' ' >> c2s '(' >> showPattern pattern >> c2s ')'
  | AbsSyntax.PatternTuple patterns ->
      s2s "PatternTuple" >> c2s ' ' >> c2s '('
      >> showList showPattern patterns
      >> c2s ')'
  | AbsSyntax.PatternRecord labelledpatterns ->
      s2s "PatternRecord" >> c2s ' ' >> c2s '('
      >> showList showLabelledPattern labelledpatterns
      >> c2s ')'
  | AbsSyntax.PatternList patterns ->
      s2s "PatternList" >> c2s ' ' >> c2s '('
      >> showList showPattern patterns
      >> c2s ')'
  | AbsSyntax.PatternCons (pattern0, pattern) ->
      s2s "PatternCons" >> c2s ' ' >> c2s '(' >> showPattern pattern0
      >> s2s ", " >> showPattern pattern >> c2s ')'
  | AbsSyntax.PatternFalse -> s2s "PatternFalse"
  | AbsSyntax.PatternTrue -> s2s "PatternTrue"
  | AbsSyntax.PatternUnit -> s2s "PatternUnit"
  | AbsSyntax.PatternInt integer ->
      s2s "PatternInt" >> c2s ' ' >> c2s '(' >> showInt integer >> c2s ')'
  | AbsSyntax.PatternSucc pattern ->
      s2s "PatternSucc" >> c2s ' ' >> c2s '(' >> showPattern pattern >> c2s ')'
  | AbsSyntax.PatternVar stellaident ->
      s2s "PatternVar" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> c2s ')'

and showLabelledPattern (e : AbsSyntax.labelledPattern) : showable =
  match e with
  | AbsSyntax.ALabelledPattern (stellaident, pattern) ->
      s2s "ALabelledPattern" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showPattern pattern >> c2s ')'

and showBinding (e : AbsSyntax.binding) : showable =
  match e with
  | AbsSyntax.ABinding (stellaident, expr) ->
      s2s "ABinding" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showExpr expr >> c2s ')'

and showExpr (e : AbsSyntax.expr) : showable =
  match e with
  | AbsSyntax.Sequence (expr0, expr) ->
      s2s "Sequence" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Assign (expr0, expr) ->
      s2s "Assign" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.If (expr0, expr1, expr) ->
      s2s "If" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr1 >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.Let (patternbindings, expr) ->
      s2s "Let" >> c2s ' ' >> c2s '('
      >> showList showPatternBinding patternbindings
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.LetRec (patternbindings, expr) ->
      s2s "LetRec" >> c2s ' ' >> c2s '('
      >> showList showPatternBinding patternbindings
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.TypeAbstraction (stellaidents, expr) ->
      s2s "TypeAbstraction" >> c2s ' ' >> c2s '('
      >> showList showStellaIdent stellaidents
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.LessThan (expr0, expr) ->
      s2s "LessThan" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.LessThanOrEqual (expr0, expr) ->
      s2s "LessThanOrEqual" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.GreaterThan (expr0, expr) ->
      s2s "GreaterThan" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.GreaterThanOrEqual (expr0, expr) ->
      s2s "GreaterThanOrEqual" >> c2s ' ' >> c2s '(' >> showExpr expr0
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.Equal (expr0, expr) ->
      s2s "Equal" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.NotEqual (expr0, expr) ->
      s2s "NotEqual" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.TypeAsc (expr, type') ->
      s2s "TypeAsc" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showTypeT type' >> c2s ')'
  | AbsSyntax.TypeCast (expr, type') ->
      s2s "TypeCast" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showTypeT type' >> c2s ')'
  | AbsSyntax.Abstraction (paramdecls, expr) ->
      s2s "Abstraction" >> c2s ' ' >> c2s '('
      >> showList showParamDecl paramdecls
      >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.Variant (stellaident, exprdata) ->
      s2s "Variant" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showExprData exprdata >> c2s ')'
  | AbsSyntax.Match (expr, matchcases) ->
      s2s "Match" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showList showMatchCase matchcases
      >> c2s ')'
  | AbsSyntax.List exprs ->
      s2s "List" >> c2s ' ' >> c2s '(' >> showList showExpr exprs >> c2s ')'
  | AbsSyntax.Add (expr0, expr) ->
      s2s "Add" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Subtract (expr0, expr) ->
      s2s "Subtract" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.LogicOr (expr0, expr) ->
      s2s "LogicOr" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Multiply (expr0, expr) ->
      s2s "Multiply" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Divide (expr0, expr) ->
      s2s "Divide" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.LogicAnd (expr0, expr) ->
      s2s "LogicAnd" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Ref expr ->
      s2s "Ref" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Deref expr ->
      s2s "Deref" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Application (expr, exprs) ->
      s2s "Application" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showList showExpr exprs >> c2s ')'
  | AbsSyntax.TypeApplication (expr, types) ->
      s2s "TypeApplication" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showList showTypeT types >> c2s ')'
  | AbsSyntax.DotRecord (expr, stellaident) ->
      s2s "DotRecord" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showStellaIdent stellaident
      >> c2s ')'
  | AbsSyntax.DotTuple (expr, integer) ->
      s2s "DotTuple" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showInt integer >> c2s ')'
  | AbsSyntax.Tuple exprs ->
      s2s "Tuple" >> c2s ' ' >> c2s '(' >> showList showExpr exprs >> c2s ')'
  | AbsSyntax.Record bindings ->
      s2s "Record" >> c2s ' ' >> c2s '('
      >> showList showBinding bindings
      >> c2s ')'
  | AbsSyntax.ConsList (expr0, expr) ->
      s2s "ConsList" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Head expr ->
      s2s "Head" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.IsEmpty expr ->
      s2s "IsEmpty" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Tail expr ->
      s2s "Tail" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Panic -> s2s "Panic"
  | AbsSyntax.Throw expr ->
      s2s "Throw" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.TryCatch (expr0, pattern, expr) ->
      s2s "TryCatch" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showPattern pattern >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.TryWith (expr0, expr) ->
      s2s "TryWith" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Inl expr ->
      s2s "Inl" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Inr expr ->
      s2s "Inr" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Succ expr ->
      s2s "Succ" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.LogicNot expr ->
      s2s "LogicNot" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Pred expr ->
      s2s "Pred" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.IsZero expr ->
      s2s "IsZero" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.Fix expr ->
      s2s "Fix" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
  | AbsSyntax.NatRec (expr0, expr1, expr) ->
      s2s "NatRec" >> c2s ' ' >> c2s '(' >> showExpr expr0 >> s2s ", "
      >> showExpr expr1 >> s2s ", " >> showExpr expr >> c2s ')'
  | AbsSyntax.Fold (type', expr) ->
      s2s "Fold" >> c2s ' ' >> c2s '(' >> showTypeT type' >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.Unfold (type', expr) ->
      s2s "Unfold" >> c2s ' ' >> c2s '(' >> showTypeT type' >> s2s ", "
      >> showExpr expr >> c2s ')'
  | AbsSyntax.ConstTrue -> s2s "ConstTrue"
  | AbsSyntax.ConstFalse -> s2s "ConstFalse"
  | AbsSyntax.ConstUnit -> s2s "ConstUnit"
  | AbsSyntax.ConstInt integer ->
      s2s "ConstInt" >> c2s ' ' >> c2s '(' >> showInt integer >> c2s ')'
  | AbsSyntax.ConstMemory memoryaddress ->
      s2s "ConstMemory" >> c2s ' ' >> c2s '('
      >> showMemoryAddress memoryaddress
      >> c2s ')'
  | AbsSyntax.Var stellaident ->
      s2s "Var" >> c2s ' ' >> c2s '(' >> showStellaIdent stellaident >> c2s ')'

and showPatternBinding (e : AbsSyntax.patternBinding) : showable =
  match e with
  | AbsSyntax.APatternBinding (pattern, expr) ->
      s2s "APatternBinding" >> c2s ' ' >> c2s '(' >> showPattern pattern
      >> s2s ", " >> showExpr expr >> c2s ')'

and showVariantFieldType (e : AbsSyntax.variantFieldType) : showable =
  match e with
  | AbsSyntax.AVariantFieldType (stellaident, optionaltyping) ->
      s2s "AVariantFieldType" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", "
      >> showOptionalTyping optionaltyping
      >> c2s ')'

and showRecordFieldType (e : AbsSyntax.recordFieldType) : showable =
  match e with
  | AbsSyntax.ARecordFieldType (stellaident, type') ->
      s2s "ARecordFieldType" >> c2s ' ' >> c2s '('
      >> showStellaIdent stellaident
      >> s2s ", " >> showTypeT type' >> c2s ')'

and showTyping (e : AbsSyntax.typing) : showable =
  match e with
  | AbsSyntax.ATyping (expr, type') ->
      s2s "ATyping" >> c2s ' ' >> c2s '(' >> showExpr expr >> s2s ", "
      >> showTypeT type' >> c2s ')'
