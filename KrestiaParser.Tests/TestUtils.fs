module KrestiaParser.Tests.TestUtils

open NUnit.Framework

let fail (message: string): 'a =
   Assert.Fail(message)
   failwith message