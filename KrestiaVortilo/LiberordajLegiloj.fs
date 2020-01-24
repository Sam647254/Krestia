namespace KrestiaVortilo

open Strukturo
open FSharpx.Collections

module LiberordajLegiloj =
   let legi (vortoj: string list) (objektoj: Deque<Vorto>) (agoj: Deque<BazaVorto>)
            (legitajFrazoj: Frazo list) (lastaVorto: string option) =
      match vortoj with
      | unua :: restantaj -> failwith "???"
      | [] -> legitajFrazoj |> Ok

   let libereLegi (eniro: string): Result<List<Frazo>, string> =
      legi (List.ofArray (eniro.Split(' '))) Deque.empty Deque.empty [] None