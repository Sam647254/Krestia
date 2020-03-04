namespace KrestiaVortilo

open FSharpx.Collections
open Malinflektado

module Sintaksanalizilo2 =
   type Verbo = Verbo of MalinflektitaVorto

   type Argumento = Argumento of MalinflektitaVorto

   type Predikato = Predikato2 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo> }

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty }

   let kategorigi sintaksanalizilo vortoj =
      vortoj
      |> List.fold (fun sintaksanaliziloAk sekvaVorto ->
            match sintaksanaliziloAk with
            | Ok(sintaksanalizilo) ->
               if ĉuPredikataVorto sekvaVorto then
                  { sintaksanalizilo with Verboj = sintaksanalizilo.Verboj.Conj(Verbo sekvaVorto) } |> Ok
               elif ĉuArgumentaVorto sekvaVorto then
                  { sintaksanalizilo with Argumentoj = sintaksanalizilo.Argumentoj.Conj(Argumento sekvaVorto) } |> Ok
               else Error(sprintf "Ne povas kategorigi %s" sekvaVorto.BazaVorto)
            | Error(_) -> sintaksanaliziloAk) (Ok sintaksanalizilo)

   let legiFrazon (eniro: string): Result<Predikato, string> =
      let vortoj = eniro.Split(' ')
      let sintaksanalizilo = kreiSintaksanalizilon
      failwith "???"
