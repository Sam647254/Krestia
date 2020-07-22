namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =

   type ModifeblaVorto =
      { Kapo: MalinflektitaVorto
        Modifantoj: HashSet<Modifanto> }

   type Argumento =
      | PlenaArgumento of ModifeblaVorto
      | Eco of eco: Argumento * de: Argumento

   and Rezulto =
      { Frazoj: Predikato list
        Argumentoj: Argumento list }

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let frazoj = List<Predikato>()
      let atendantajPridirantoj = Queue<Modifanto>()
      let atendantajAntaŭajEcoj = Queue<Argumento>()
      let mutable lastaLegitaArgumento: Argumento option = None

      member this.Legi(): Result<Rezulto, Eraro> =
         this.LegiSekvan()
         |> Result.map (fun () ->
               { Frazoj = frazoj |> List.ofSeq
                 Argumentoj = argumentoj |> List.ofSeq })

      member private this.LegiSekvan(): Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuArgumentaVorto sekva then
               this.LegiPlenaArgumenton()
               |> Result.bind (fun argumento ->
                     argumentoj.AddLast(argumento) |> ignore
                     this.LegiSekvan())
            elif ĉuAntaŭEco sekva then
               this.LegiPlenaArgumenton()
               |> Result.bind (fun eco ->
                     match eco with
                     | PlenaArgumento (argumento) ->
                        argumento.Modifantoj.UnionWith(atendantajPridirantoj :> IEnumerable<Modifanto>)
                     | _ -> ()
                     atendantajPridirantoj.Clear()
                     atendantajAntaŭajEcoj.Enqueue(eco)
                     lastaLegitaArgumento <- Some eco
                     this.LegiSekvan())
            elif ĉuAntaŭModifantaVorto sekva then
               failwith "???"
            elif ĉuMalantaŭModifantaVorto sekva then
               let pridiranto = this.LegiMalantaŭanPridiranton()
               match lastaLegitaArgumento with
               | Some(a) ->
                  match a with
                  | PlenaArgumento(argumento) -> argumento.Modifantoj.Add(pridiranto) |> ignore
                  | _ -> failwith "Nur povas aldoni modifanton al PlenaArgumento"
               | None ->
                  match argumentoj.Last.Value with
                  | PlenaArgumento(argumento) -> argumento.Modifantoj.Add(pridiranto) |> ignore
                  | _ -> failwith "Nur povas aldoni modifanton al PlenaArgumento"
               this.LegiSekvan()
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiPlenaArgumenton(): Result<Argumento, Eraro> =
         let argumento = enira.Dequeue()
         if not (ĉuArgumentaVorto argumento) then
            Error(Eraro(argumento.OriginalaVorto, sprintf "%s is not a valid argument" argumento.OriginalaVorto.Vorto))
         else
            let novaArgumento =
               { Kapo = argumento
                 Modifantoj = HashSet(atendantajPridirantoj) }
               |> PlenaArgumento

            atendantajPridirantoj.Clear()
            lastaLegitaArgumento <- Some novaArgumento
            novaArgumento |> Ok

      member private this.LegiMalantaŭanPridiranton() = enira.Dequeue() |> Pridiranto
