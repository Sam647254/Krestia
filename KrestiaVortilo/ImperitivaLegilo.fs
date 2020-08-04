namespace KrestiaVortilo

open System.Collections
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

   type private AtendantaVerbo =
      { Verbo: Verbo
        Valenco: int
        AktualajArgumentoj: LinkedList<Argumento> }

   type private LastaLegitaModifeblaVorto =
      | ModifeblaVerbo of Verbo
      | ModifeblaArgumento of Argumento

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = Queue<Argumento>()
      let frazoj = Queue<Predikato>()
      let atendantajPridirantoj = LinkedList<Modifanto>()
      let atendantajAntaŭajEcoj = LinkedList<Argumento>()
      let atendantajPredikatoj = LinkedList<AtendantaVerbo>()
      let mutable lastaModifeblaVorto: LastaLegitaModifeblaVorto option = None
      let mutable lastaArgumento: Argumento option = None

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
                     if atendantajPredikatoj.Count > 0 then
                        let sekvaPredikato = atendantajPredikatoj.First.Value
                        sekvaPredikato.AktualajArgumentoj.AddLast(argumento)
                        |> ignore
                        if sekvaPredikato.AktualajArgumentoj.Count = sekvaPredikato.Valenco then
                           frazoj.Enqueue
                              ({ Kapo = sekvaPredikato.Verbo
                                 Argumentoj = List.ofSeq sekvaPredikato.AktualajArgumentoj })
                           atendantajPredikatoj.RemoveFirst()
                     else
                        argumentoj.Enqueue(argumento)
                     this.LegiSekvan())
            elif ĉuAntaŭEco sekva then
               this.LegiArgumenton()
               |> Result.bind (fun eco ->
                     eco.Vorto.Modifantoj.UnionWith(atendantajPridirantoj :> IEnumerable<Modifanto>)
                     atendantajPridirantoj.Clear()
                     atendantajAntaŭajEcoj.AddLast(eco) |> ignore
                     lastaModifeblaVorto <- Some(ModifeblaArgumento eco)
                     this.LegiSekvan())
            elif ĉuAntaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               atendantajPridirantoj.AddLast(pridiranto)
               |> ignore
               this.LegiSekvan()
            elif ĉuMalantaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               this.AldoniPridiranton(pridiranto)
               this.LegiSekvan()
            elif ĉuPredikataVorto sekva then
               let verbo = this.LegiPredikaton()
               let valenco = valencoDe sekva

               if argumentoj.Count >= valenco then
                  let argumentoj =
                     seq { 1 .. valenco }
                     |> Seq.fold (fun ak _ ->
                           let sekvaArgumento = argumentoj.Dequeue()
                           sekvaArgumento :: ak) []

                  frazoj.Enqueue
                     ({ Kapo = verbo
                        Argumentoj = argumentoj })
                  |> ignore
                  this.LegiSekvan()
               else
                  atendantajPredikatoj.AddLast
                     ({ Verbo = verbo
                        Valenco = valenco
                        AktualajArgumentoj = LinkedList(argumentoj) })
                  |> ignore
                  argumentoj.Clear()
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
            atendantajPridirantoj.AddLast(Pridiranto(sekva))
            |> ignore
            this.LegiArgumenton()
         elif ĉuAntaŭEco sekva then
            let eco =
               plenaModifitaArgumento sekva atendantajPridirantoj

            lastaModifeblaVorto <- Some(ModifeblaArgumento eco)
            lastaArgumento <- Some eco
            this.LegiArgumenton()
            |> Result.map (fun de ->
                  eco.Vorto.Modifantoj.Add(EcoDe de) |> ignore
                  eco)
         elif ĉuMalantaŭModifantaVorto sekva then
            failwith "TODO"
            this.LegiArgumenton()
         elif ĉuMalantaŭEco sekva then
            let argumento = argumento sekva []
            match lastaArgumento with
            | Some (a) -> a.Vorto.Modifantoj.Add(EcoDe(argumento)) |> ignore
            | None -> failwith "No argument to associate with"
            lastaModifeblaVorto <- Some(ModifeblaArgumento argumento)
            lastaArgumento <- Some argumento
            this.LegiArgumenton()
         else
            failwith "Unexpected input"

      member private this.LegiPlenanArgumenton(sekva) =
         let novaArgumento =
            let argumento =
               plenaModifitaArgumento sekva atendantajPridirantoj

            lastaModifeblaVorto <- Some(ModifeblaArgumento argumento)
            lastaArgumento <- Some argumento
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

      member private this.AldoniPridiranton(pridiranto) =
         if atendantajAntaŭajEcoj.Count > 0 then
            atendantajAntaŭajEcoj.Last.Value.Vorto.Modifantoj.Add(pridiranto)
            |> ignore
         elif atendantajPredikatoj.Count > 0 then
            atendantajPredikatoj.Last.Value.Verbo.Vorto.Modifantoj.Add(pridiranto)
            |> ignore
         else
            match lastaModifeblaVorto with
            | Some (vorto) ->
               match vorto with
               | ModifeblaVerbo (verbo) -> verbo.Vorto.Modifantoj.Add(pridiranto)
               | ModifeblaArgumento (argumento) -> argumento.Vorto.Modifantoj.Add(pridiranto)
               |> ignore
            | None -> failwith "No argument to modify"

      member private this.LegiPredikaton() =
         let novaVerbo = verbo (enira.Dequeue()) (List.ofSeq atendantajPridirantoj)
         atendantajPridirantoj.Clear()
         lastaModifeblaVorto <- Some <| ModifeblaVerbo novaVerbo
         novaVerbo

      member private this.LegiPridiranton(): Modifanto = enira.Dequeue() |> Pridiranto

      member private this.LegiĈiujnMalantaŭajnModifantojn() =
         let modifantoj = HashSet()
         while enira.Count > 0
               && ĉuMalantaŭModifantaVorto (enira.Peek()) do
            let sekva = enira.Dequeue()
            modifantoj.Add(Pridiranto(sekva)) |> ignore
         modifantoj
