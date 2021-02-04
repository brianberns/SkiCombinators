namespace Ski.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Ski

[<TestClass>]
type UnitTest () =

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
