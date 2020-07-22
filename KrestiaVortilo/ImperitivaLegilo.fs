namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =
   type Rezulto =
      { Frazoj: Predikato list
        Argumentoj: Argumento list }

   let plenaArgumento (argumento): Argumento =
      { Vorto =
           { Kapo = argumento
             Modifantoj = HashSet() } }

   let plenaModifitaArgumento argumento (modifantoj: Modifanto seq): Argumento =
      { Vorto =
           { Kapo = argumento
             Modifantoj = HashSet(modifantoj) } }

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
               this.LegiArgumenton()
               |> Result.bind (fun argumento ->
                     argumentoj.AddLast(argumento) |> ignore
                     this.LegiSekvan())
            elif ĉuAntaŭEco sekva then
               this.LegiArgumenton()
               |> Result.bind (fun eco ->
                     eco.Vorto.Modifantoj.UnionWith(atendantajPridirantoj :> IEnumerable<Modifanto>)
                     atendantajPridirantoj.Clear()
                     atendantajAntaŭajEcoj.Enqueue(eco)
                     lastaLegitaArgumento <- Some eco
                     this.LegiSekvan())
            elif ĉuAntaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               atendantajPridirantoj.Enqueue(pridiranto)
               this.LegiSekvan()
            elif ĉuMalantaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               match lastaLegitaArgumento with
               | Some (a) -> a.Vorto.Modifantoj.Add(pridiranto) |> ignore
               | None -> failwith "No argument to modify"
               this.LegiSekvan()
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let sekva = enira.Dequeue()
         if ĉuDifinitaKlaso sekva then
            this.LegiPlenanArgumenton(sekva) |> Ok
         elif ĉuAntaŭModifantaVorto sekva then
            atendantajPridirantoj.Enqueue(Pridiranto(sekva))
            this.LegiArgumenton()
         elif ĉuAntaŭEco sekva then
            let eco =
               plenaModifitaArgumento sekva atendantajPridirantoj

            lastaLegitaArgumento <- Some eco
            this.LegiArgumenton()
            |> Result.map (fun de -> eco.Vorto.Modifantoj.Add(EcoDe de) |> ignore; eco)
         elif ĉuMalantaŭModifantaVorto sekva then
            failwith "???"
            this.LegiArgumenton()
         elif ĉuMalantaŭEco sekva then
            match lastaLegitaArgumento with
            | Some (a) ->
               a.Vorto.Modifantoj.Add(Pridiranto(sekva))
               |> ignore
            | None -> failwith "No argument to associate with"
            this.LegiArgumenton()
         else
            failwith "Unexpected input"

      member private this.LegiPlenanArgumenton(sekva) =
         let novaArgumento =
            let argumento =
               plenaModifitaArgumento sekva atendantajPridirantoj

            lastaLegitaArgumento <- Some argumento
            if atendantajAntaŭajEcoj.Count > 0 then
               atendantajAntaŭajEcoj
               |> Seq.reduce (fun ak sek ->
                     ak.Vorto.Modifantoj.Add(EcoDe sek) |> ignore
                     sek)
               |> (fun ĉiujEcoj ->
                  argumento.Vorto.Modifantoj.Add(EcoDe(ĉiujEcoj))
                  |> ignore
                  argumento)
            else
               argumento

         atendantajPridirantoj.Clear()
         atendantajAntaŭajEcoj.Clear()
         novaArgumento

      member private this.LegiPridiranton() = enira.Dequeue() |> Pridiranto

      member private this.LegiĈiujnMalantaŭajnModifantojn() =
         let modifantoj = HashSet()
         while enira.Count > 0
               && ĉuMalantaŭModifantaVorto (enira.Peek()) do
            let sekva = enira.Dequeue()
            modifantoj.Add(Pridiranto(sekva)) |> ignore
         modifantoj
