namespace Ski.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Ski

[<TestClass>]
type UnitTest () =

    let testParse str f =
        match Ski.tryParse str with
            | Ok ski -> f ski
            | Error msg -> Assert.Fail(msg)

    [<TestMethod>]
    member __.Parse() =

        [
            "S", S
            "SK", Node (S, K)
            "SKI", Node (Node (S, K), I)
            "(SK)I", Node (Node (S, K), I)
            "S(KI)", Node (S, Node (K, I))
        ]
            |> Seq.iter (fun (str, expected) ->
                Assert.AreEqual(
                    (Ok expected : Result<_, string>),
                    Ski.tryParse str))

        [
            "I)", "Extra closing paren"
            "S(K))I", "Extra closing paren"
            "(I", "Missing closing paren"
            "S((K)I", "Missing closing paren"
            "", "Empty expression"
        ]
            |> Seq.iter (fun (str, expected) ->
                Assert.AreEqual(
                    (Error expected : Result<Ski, _>),
                    Ski.tryParse str))

        let str = "S(S(SI(K(S(S(KS)K)(K(S(S(KS)(S(KK)S))(K(S(KK)(S(S(KS)K)))))))))(K(S(SI(K(KI)))(K(KI)))))(KK)"
        testParse str (fun ski ->
            Assert.AreEqual(str, ski.String))

    [<TestMethod>]
    member __.SKSK() =
        testParse "SKSK" (fun ski ->
            Assert.AreEqual(K, Ski.eval ski))

    [<TestMethod; Timeout(1000)>]
    member __.SIISII() =
        testParse "SII(SII)" (fun ski ->
            Assert.AreEqual(ski, Ski.eval ski))

    /// https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
    /// http://wiki.c2.com/?EssAndKayCombinators
    /// https://en.wikipedia.org/wiki/Church_encoding
    [<TestMethod>]
    member __.Succ() =

            // SKI encoding of Church successor function
            // succ n f x = f((nf)x)
        let succ = "S(S(KS)K)"

            // Church encoding of Peano numbers
        let rec nat = function
            | 0 -> "x"
            | 1 -> "fx"
            | n when n > 1 -> $"f({nat (n - 1)})"
            | _ -> failwith "Unexpected"

            // test the successor function
        let test n str =
            testParse $"({succ})({str})fx" (fun ski ->
                let actual = Ski.eval ski
                Assert.AreEqual(nat (n + 1), actual.String))
        test 0 "KI"
        test 1 "I"
        test 2 "S(S(KS)K)I"
