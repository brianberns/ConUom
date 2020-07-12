namespace ConUom

open MathNet.Numerics

[<StructuredFormatDisplay("{String}")>]
type Expr =
    | Const of BigRational
    | Var
    | Sum of Expr * Expr
    | Difference of Expr * Expr
    | Product of Expr * Expr
    | Quotient of Expr * Expr

    member this.String =

        let toChar = function
            | Sum _ -> '+'
            | Difference _ -> '-'
            | Product _ -> '*'
            | Quotient _ -> '/'
            | _ -> failwith "Unexpected"

        match this with
            | Const n -> sprintf "%A" n
            | Var -> "x"
            | Sum (exprA, exprB)
            | Difference (exprA, exprB)
            | Product (exprA, exprB)
            | Quotient (exprA, exprB) ->
                sprintf "(%A %c %A)" exprA (toChar this) exprB

module Expr =

    let eval n expr =

        let op = function
            | Sum _ -> (+)
            | Difference _ -> (-)
            | Product _ -> (*)
            | Quotient _ -> (/)
            | _ -> failwith "Unexpected"

        let rec loop = function
            | Const c -> c
            | Var -> n
            | Sum (exprA, exprB)
            | Difference (exprA, exprB)
            | Product (exprA, exprB)
            | Quotient (exprA, exprB) as expr ->
                (op expr) (loop exprA) (loop exprB)

        loop expr

    let invert expr =

        let opposite = function
            | Sum _ -> Difference
            | Difference _ -> Sum
            | Product _ -> Quotient
            | Quotient _ -> Product
            | _ -> failwith "Unexpected"
        
        let rec containsVar = function
            | Const _ -> false
            | Var -> true
            | Sum (exprA, exprB)
            | Difference (exprA, exprB)
            | Product (exprA, exprB)
            | Quotient (exprA, exprB) ->
                containsVar exprA || containsVar exprB

        let rec loop left right =
            match left with
                | Const c -> failwith "Unexpected"
                | Var -> right
                | Sum (leftA, leftB)
                | Difference (leftA, leftB)
                | Product (leftA, leftB)
                | Quotient (leftA, leftB) ->
                    let opp = opposite left
                    match containsVar leftA, containsVar leftB with
                        | true, false ->
                            loop leftA (opp (right, leftB))
                        | false, true ->
                            loop leftB (opp (right, leftA))
                        | _ -> failwith "Unexpected"

        loop expr Var

    let subst outer inner =

        let same = function
            | Sum _ -> Sum
            | Difference _ -> Difference
            | Product _ -> Product
            | Quotient _ -> Quotient
            | _ -> failwith "Unexpected"

        let rec loop = function
            | Const _ as expr -> expr
            | Var -> inner
            | Sum (exprA, exprB)
            | Difference (exprA, exprB)
            | Product (exprA, exprB)
            | Quotient (exprA, exprB) as expr ->
                (same expr) (loop exprA, loop exprB)

        loop outer
