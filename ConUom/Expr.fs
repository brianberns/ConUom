namespace ConUom

type Expr =
    | Const of decimal
    | Var
    | Sum of Expr * Expr
    | Difference of Expr * Expr
    | Product of Expr * Expr
    | Quotient of Expr * Expr

module Expr =

    let op = function
        | Sum _ -> (+)
        | Difference _ -> (-)
        | Product _ -> (*)
        | Quotient _ -> (/)
        | _ -> failwith "Unexpected"

    let rec eval n = function
        | Const c -> c
        | Var -> n
        | Sum (exprA, exprB)
        | Difference (exprA, exprB)
        | Product (exprA, exprB)
        | Quotient (exprA, exprB) as expr ->
            let op = op expr
            op (eval n exprA) (eval n exprB)

    let invert expr =
        
        let rec containsVar = function
            | Const _ -> false
            | Var -> true
            | Sum (exprA, exprB)
            | Difference (exprA, exprB)
            | Product (exprA, exprB)
            | Quotient (exprA, exprB) ->
                containsVar exprA || containsVar exprB

        let invert = function
            | Sum _ -> Difference
            | Difference _ -> Sum
            | Product _ -> Quotient
            | Quotient _ -> Product
            | _ -> failwith "Unexpected"

        let rec loop left right =
            match left with
                | Const c -> failwith "Unexpected"
                | Var -> right
                | Sum (leftA, leftB)
                | Difference (leftA, leftB)
                | Product (leftA, leftB)
                | Quotient (leftA, leftB) ->
                    let inv = invert left
                    match containsVar leftA, containsVar leftB with
                        | true, false ->
                            loop leftA (inv (right, leftB))
                        | false, true ->
                            loop leftB (inv (right, leftA))
                        | _ -> failwith "Unexpected"

        loop expr Var
