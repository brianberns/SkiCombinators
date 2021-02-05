namespace Ski

/// https://en.wikipedia.org/wiki/SKI_combinator_calculus
[<StructuredFormatDisplay("{String}")>]
type Ski =
    | I
    | K
    | S
    | Node of Ski * Ski
    | Variable of name : string

    member ski.String =
        match ski with
            | I -> "I"
            | K -> "K"
            | S -> "S"
            | Node (x, I) -> $"{x.String}I"
            | Node (x, K) -> $"{x.String}K"
            | Node (x, S) -> $"{x.String}S"
            | Node (x, Variable name) -> $"{x.String}{name}"
            | Node (x, y) -> $"{x.String}({y.String})"
            | Variable str -> str

    override this.ToString() =
        this.String

module Ski =

    let eval ski =

        let rec loop seen ski =
            if seen |> Set.contains ski then ski
            else
                let loop' = seen |> Set.add ski |> loop
                match ski with
                    | Node (I, x) -> loop' x               // identity: fun x -> x
                    | Node (Node (K, x), _) -> loop' x     // constant: fun x y -> x
                    | Node (Node (Node (S, x), y), z) ->   // substitution: fun x y z -> x z (y z)
                        loop' (Node (Node (x, z), Node (y, z)))
                    | Node (x, y) ->
                        let x' = loop' x
                        let y' = loop' y
                        loop' (Node (x', y'))
                    | _ -> ski

        loop Set.empty ski

    let parse (str : string) =

        let fromChar = function
            | 'I' -> I
            | 'K' -> K
            | 'S' -> S
            | c -> Variable (string c)

        let step nodeOpts = function
            | '(' ->
                None :: nodeOpts
            | ')' ->
                match nodeOpts with
                    | Some y :: Some x :: tail ->
                        Some (Node (x, y)) :: tail
                    | Some x :: None :: tail ->
                        Some x :: tail
                    | _ -> failwith "Unexpected"
            | c ->
                match nodeOpts with
                    | Some node :: tail ->
                        Some (Node (node, fromChar c)) :: tail
                    | None :: tail ->
                        Some (fromChar c) :: tail
                    | [] -> failwith "Unexpected"

        ([None], str)
            ||> Seq.fold step
            |> List.exactlyOne
            |> Option.get
