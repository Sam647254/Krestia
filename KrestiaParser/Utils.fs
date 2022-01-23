module KrestiaParser.Utils

open Microsoft.FSharp.Reflection

type ResultBuilder() =
   member _.Bind(result, f) =
      Result.bind f result
   
   member _.Return t = Ok t
   
   member _.ReturnFrom r = r

let result = ResultBuilder()

type OptionBuilder() =
   member _.Bind(option, f) =
      Option.bind f option

   member _.Return = Some
   
   member _.ReturnFrom o = o

let option = OptionBuilder()

let (<|>) r1 r2 =
   match r1 with
   | Ok _ -> r1
   | _ -> r2