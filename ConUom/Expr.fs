namespace ConUom

type Expr =
    | Const of decimal
    | Var
    | Product of Expr * Expr
    | Quotient of Expr * Expr

module Expr =

    let rec eval n = function
        | Const c -> c
        | Var -> n
        | Product (exprA, exprB) ->
            eval n exprA * eval n exprB
        | Quotient (exprA, exprB) ->
            eval n exprA / eval n exprB
