#!/bin/sh

CURRENT=$(dirname $(readlink -f "$0"))

INPUT1="
 (Program
  (FunctionDeclaration
   (TypeName Int)
   (FunctionName fun1)
   (Block
    (DeclareAssignment (TypeName Int) (VarName var1) 0)
    (IfClause
     true
     (Block
      (Assignment (VarName var1) 2)
      (DeclareAssignment (TypeName Int) (VarName var1) 1)
      (Assignment (VarName var1) 3)))
    (Assignment (VarName var1) 4)
    (ReturnClause 0))))
"

INPUT2="
(Program
  (FunctionDeclaration
   (TypeName Int)
   (FunctionName fun1)
   (Argument (TypeName Bool) (VarName var1))
   (Block
    (Assignment (VarName var1) true)
    (DeclareAssignment (TypeName Int) (VarName var1) 0)
    (WhileClause
     true
     (Block
      (Assignment (VarName var1) 2)
      (DeclareAssignment (TypeName Bool) (VarName var1) true)))
    (ReturnClause 0))))
"

INPUT3="
(Program
  (FunctionDeclaration
   (TypeName Int)
   (FunctionName fun1)
   (Argument (TypeName Bool) (VarName var1))
   (Block

    (Assignment (VarName var1) (BoolLiteral true))
    (DeclareAssignment (TypeName Int) (VarName var1) (IntLiteral 0))
    (WhileClause
     (BoolLiteral true)
     (Block
      (Assignment (VarName var1) (IntLiteral 2))
      (DeclareAssignment (TypeName Int) (VarName var2) (VarUse (VarName var1)))
      (Assignment (VarUse (VarName var2)) (VarUse (VarName var1)))
      (DeclareAssignment (TypeName Bool) (VarName var1) (BoolLiteral true))
      (DeclareAssignment (TypeName Bool) (VarName var3) (IntLiteral 0))
      ;; (Assignment (VarName var2) (VarUse (VarName var1)))
      ;; (Assignment (VarName var2) (Plus (VarUse (VarName var2)) (VarUse (VarName var2))))
      ;; (Or (VarUse (VarName var2)) (VarUse (VarName var2)))
      ;; (Plus (VarUse (VarName var2)) (VarUse (VarName var2)))
      (ReturnClause (VarUse (VarName var1))) ;; Illegal
      (Plus (VarUse (VarName var2)) (Plus (VarUse (VarName var2)) (VarUse (VarName var2))))
      (Plus (VarUse (VarName var2)) (Or (VarUse (VarName var1)) (VarUse (VarName var1))))
      ))
    (ReturnClause (IntLiteral 0)))))
"

STRATEGY="
(topdown
decls
decls2*
builtins
doubleVars
alphaUnique
returnAlphaUnique
undoubleVars
typecheckAddBuiltins
typecheckGround
typecheckGroundF*
typecheckFunctionCall
typecheckAssignment
typecheckReturn
typecheckControls
unalpha
)
"

exec zc \
	rules= "$CURRENT/Cimplified.szcalc" \
	strategy= "$STRATEGY" \
	"$INPUT3"
