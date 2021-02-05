namespace Ski.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Ski

[<TestClass>]
type UnitTest () =

    [<TestMethod>]
    member __.Parse() =
        Assert.AreEqual(S, Ski.parse "S")
        Assert.AreEqual(Node (S, K), Ski.parse "SK")
        Assert.AreEqual(Node (Node (S, K), I), Ski.parse "SKI")
        Assert.AreEqual(Node (Node (S, K), I), Ski.parse "(SK)I")
        Assert.AreEqual(Node (S, Node (K, I)), Ski.parse "S(KI)")
        let str = "S(S(SI(K(S(S(KS)K)(K(S(S(KS)(S(KK)S))(K(S(KK)(S(S(KS)K)))))))))(K(S(SI(K(KI)))(K(KI)))))(KK)"
        Assert.AreEqual(str, $"{Ski.parse str}")

    [<TestMethod>]
    member __.SKSK() =
        let sksk = "SKSK" |> Ski.parse
        Assert.AreEqual(K, Ski.eval sksk)

    [<TestMethod; Timeout(1000)>]
    member __.SIISII() =
        let siisii = "SII(SII)" |> Ski.parse
        Assert.AreEqual(siisii, Ski.eval siisii)

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
            let actual = $"({succ})({str})fx" |> Ski.parse |> Ski.eval
            Assert.AreEqual(nat (n + 1), actual.String)
        test 0 "KI"
        test 1 "I"
        test 2 "S(S(KS)K)I"
