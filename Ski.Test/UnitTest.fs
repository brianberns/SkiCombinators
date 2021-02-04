namespace Ski.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Ski

[<TestClass>]
type UnitTest () =

    [<TestMethod>]
    member __.SKSK () =
        let sksk = Node (Node (Node (S, K), S), K)
        Assert.AreEqual("SKSK", $"{sksk}")
        Assert.AreEqual(K, Ski.eval sksk)
