namespace Ski

/// A combinatory logic equivalent to the untyped lambda
/// calculus.
/// https://en.wikipedia.org/wiki/SKI_combinator_calculus
[<StructuredFormatDisplay("{String}")>]
type Ski =

    /// Identity combinator.
    | I

    /// Constant combinator.
    | K

    /// "Substitution" combinator.
    | S

    /// Internal node.
    | Node of Ski * Ski

    /// Arbitrary variable.
    | Variable of char

    /// Pretty-printing.
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
            | Variable c -> string c

    /// Pretty-printing.
    override this.ToString() =
        this.String

module Ski =

    /// Evaluates the given combinator.
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

    /// Pretty-printing.
    let toString (ski : Ski) =
        ski.String

    /// Tries to parse a combinator from the given string.
    let tryParse (str : string) =

        let fromChar = function
            | 'I' -> I
            | 'K' -> K
            | 'S' -> S
            | c -> Variable c

        let step nodeOpts = function

                // start a new level
            | '(' ->
                Ok (None :: nodeOpts)

                // finish the current level
            | ')' ->
                match nodeOpts with

                        // combine current level with previous level
                    | Some y :: Some x :: tail ->
                        Ok (Some (Node (x, y)) :: tail)

                        // replace previous level
                    | Some x :: None :: tail ->
                        Ok (Some x :: tail)

                    | _ -> Error "Extra closing paren"

                // process the given character
            | c ->
                match nodeOpts with

                        // combine character with current node
                    | Some node :: tail ->
                        Ok (Some (Node (node, fromChar c)) :: tail)

                        // create a new node
                    | None :: tail ->
                        Ok (Some (fromChar c) :: tail)

                    | [] -> Error "Unexpected"

        let str = if isNull str then "" else str
        let result =
            (Ok [None], str)
                ||> Seq.fold (fun result c ->
                    match result with
                        | Ok nodeOpts -> step nodeOpts c
                        | _ -> result)
        match result with
            | Ok nodeOpts ->
                match nodeOpts with
                    | [ nodeOpt ] ->
                        nodeOpt
                            |> Option.map Ok
                            |> Option.defaultWith (fun () ->
                                Error "Empty expression")
                    | [] -> Error "Unexpected"
                    | _ -> Error "Missing closing paren"
            | Error msg -> Error msg
