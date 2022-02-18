module KrestiaParser.Tests.TestUtils

open NUnit.Framework

let fail (message: string): 'a =
   Assert.Fail(message)
   failwith message

type OptionTestBuilder (errorMessage: string) =
   member _.Bind (option, f) =
      option
      |> Option.defaultWith (fun () -> fail errorMessage)
      |> f
   
   member _.Zero () = ()

let optionTest errorMessage = OptionTestBuilder errorMessage