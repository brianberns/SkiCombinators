namespace Ski

/// https://en.wikipedia.org/wiki/SKI_combinator_calculus
[<StructuredFormatDisplay("{String}")>]
type Ski =
    | I
    | K
    | S
    | Node of Ski * Ski

    member ski.String =
        match ski with
            | I -> "I"
            | K -> "K"
            | S -> "S"
            | Node (x, I) -> $"{x.String}I"
            | Node (x, K) -> $"{x.String}K"
            | Node (x, S) -> $"{x.String}S"
            | Node (x, y) -> $"({x.String}{y.String})"

    override this.ToString() =
        this.String

module Ski =

    let rec eval = function
        | I -> I
        | K -> K
        | S -> S
        | Node (I, x) -> eval x
        | Node (Node (K, x), _) -> eval x
        | Node (Node (Node (S, x), y), z) ->
            eval (Node (Node (x, z), Node (y, z)))
        | Node (x, y) as node ->
            let x' = eval x
            let y' = eval y
            if x' = x && y' = y then node
            else eval (Node (x', y'))
