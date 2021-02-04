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

    [<TestMethod>]
    member __.SKSK() =
        let sksk = Node (Node (Node (S, K), S), K)
        Assert.AreEqual("SKSK", $"{sksk}")
        Assert.AreEqual(K, Ski.eval sksk)

    [<TestMethod; Timeout(1000)>]
    member __.SIISII() =
        let sii = Node (Node (S, I), I)
        let siisii = Node (sii, sii)
        Assert.AreEqual("SII(SII)", $"{siisii}")
        Assert.AreEqual(siisii, Ski.eval siisii)

    /// https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
    [<TestMethod>]
    member __.Succ() =
        let zero = Node (K, I)
        let one = I
        let succ =
            Node (
                S,
                Node (
                    Node (
                        S,
                        Node(K, S)),
                    K))
        Assert.AreEqual("S(S(KS)K)", $"{succ}")
        let two = Node (succ, I)
        Assert.AreEqual(one, Node(succ, zero) |> Ski.eval)
