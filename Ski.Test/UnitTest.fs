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
    [<TestMethod>]
    member __.Succ() =
        let zero = "KI" |> Ski.parse
        let one = "I" |> Ski.parse
        let two = "S(S(KS)K)I" |> Ski.parse
        let succ = "S(S(KS)K)" |> Ski.parse
        Assert.AreEqual(one, Node(succ, zero) |> Ski.eval)
