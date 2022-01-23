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

type State<'state, 'result> = State of ('state -> 'result * 'state)

let runState (State s: State<'state, 'result>) = s

type StateBuilder() =
   member _.Bind(State state: State<'s, 'a>, f: 'a -> State<'s, 'b>): State<'s, 'b> =
      State (fun s' ->
         let result, state' = state s'
         let (State f') = f result
         f' state')
   
   member _.Return a = State (fun s -> (a, s))

let withState = StateBuilder()

let getState = State (fun s -> (s, s))