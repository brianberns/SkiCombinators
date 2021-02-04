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
            | Node (x, y) -> $"{x.String}({y.String})"

    override this.ToString() =
        this.String

module Ski =

    let eval ski =

        let rec loop seen ski =

            if seen |> Set.contains ski then ski
            else
                let loop = loop (seen |> Set.add ski)
                match ski with
                    | I -> I
                    | K -> K
                    | S -> S
                    | Node (I, x) -> loop x
                    | Node (Node (K, x), _) -> loop x
                    | Node (Node (Node (S, x), y), z) ->
                        loop (Node (Node (x, z), Node (y, z)))
                    | Node (x, y) ->
                        let x' = loop x
                        let y' = loop y
                        loop (Node (x', y'))

        loop Set.empty ski
