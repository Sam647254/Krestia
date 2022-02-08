module KrestiaParser.Utils

open Microsoft.FSharp.Core

type ResultBuilder() =
   member _.Bind(result, f) = Result.bind f result

   member _.Return t = Ok t

   member _.ReturnFrom r = r

let result = ResultBuilder()

type OptionBuilder() =
   member _.Bind(option, f) = Option.bind f option

   member _.Return v = Some v

   member _.ReturnFrom o = o

let option = OptionBuilder()

let (<|>) r1 r2 =
   match r1 with
   | Ok _ -> r1
   | _ -> r2

let nonEmpty list1 list2 =
   match list1 with
   | [] -> list2
   | _ -> list1

type OptionState<'state, 'result> = OptionState of ('state -> Option<'result * 'state>)

let runState (OptionState s: OptionState<'state, 'result>) = s

let lift option =
   match option with
   | Some v -> OptionState(fun s -> Some(v, s))
   | None -> OptionState(fun _ -> None)

type StateBuilder() =
   member _.Bind(OptionState state: OptionState<'s, 'a>, f: 'a -> OptionState<'s, 'b>) : OptionState<'s, 'b> =
      OptionState
         (fun s' ->
            option {
               let! result, state' = state s'
               let (OptionState f') = f result
               return! f' state'
            })

   member _.Return a = OptionState(fun s -> Some(a, s))

   member _.ReturnFrom a = a

   member _.Combine(state1, state2) =
      OptionState
         (fun s ->
            runState state1 s
            |> Option.orElse (runState state2 s))

   member _.Zero() = OptionState(fun _ -> None)

   member _.Delay result2 = result2 ()

let withState = StateBuilder()

let getState = OptionState(fun s -> Some(s, s))

let putState value = OptionState(fun _ -> Some((), value))

let guard condition : OptionState<'a, unit> =
   if condition then
      OptionState(fun s -> Some((), s))
   else
      OptionState(fun _ -> None)

let rec catOptions list =
   let rec catOptionsAcc list' remaining =
      match remaining with
      | [] -> list'
      | first :: rest ->
         match first with
         | Some v -> catOptionsAcc (v :: list') rest
         | None -> catOptionsAcc list' rest

   catOptionsAcc [] list
