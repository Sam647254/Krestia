namespace KrestiaVortilo

open System.Collections
open System.Collections.Generic
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo
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

   type private AtendantaPredikato =
      { Verbo: Verbo
        Valenco: int }

   type private LastaLegitaModifeblaVorto =
      | ModifeblaVerbo of Verbo
      | ModifeblaArgumento of Argumento

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let atendantajPridirantoj = LinkedList<Modifanto>()
      let atendantajPredikatoj = LinkedList<AtendantaPredikato>()
      let mutable lastaModifeblaVorto: LastaLegitaModifeblaVorto option = None
      let mutable lastaArgumento: Argumento option = None

      member this.Legi(): Result<Rezulto, Eraro> =
         this.LegiSekvan()
         |> Result.bind this.LegiFrazojn
      
      member this.LegiFrazojn(): Result<Rezulto, Eraro> =
         atendantajPredikatoj
         |> Seq.fold (fun listo sekva ->
            listo
            |> Result.bind (fun listo ->
               seq { 1..sekva.Valenco }
               |> Seq.fold (fun ak _ ->
                  ak
                  |> Result.bind (fun ak ->
                     if argumentoj.Count = 0 then
                        Error(Eraro(sekva.Verbo.Vorto.Kapo.OriginalaVorto, sprintf "Not enough arguments"))
                     else
                        let argumento = argumentoj.First.Value
                        argumentoj.RemoveFirst()
                        Ok(argumento :: ak))
                  ) (Ok [])
               |> Result.map List.rev
               |> Result.map (fun argumentoj -> { Kapo = sekva.Verbo; Argumentoj = argumentoj } :: listo))) (Ok [])
         |> Result.map List.rev
         |> Result.map (fun frazoj ->
            { Frazoj = frazoj
              Argumentoj = argumentoj |> List.ofSeq })
         
      member this.LegiLokalanFrazon(): Result<Predikato, Eraro> = failwith "TODO"

      member private this.LegiSekvan(): Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuMalantaŭEco sekva then
               let eco = this.LegiMalantaŭanEcon()
               if argumentoj.Count = 0 then
                  Error(Eraro(sekva.OriginalaVorto, sprintf "%s has nothing to associate with" sekva.OriginalaVorto.Vorto))
               else
                  let lasta = argumentoj.Last.Value
                  eco.Vorto.Modifantoj.Add(EcoDe(lasta)) |> ignore
                  argumentoj.RemoveLast()
                  argumentoj.AddLast(eco) |> ignore
                  this.LegiSekvan()
            elif ĉuArgumentaVorto sekva then
               this.LegiArgumenton()
               |> Result.bind (fun argumento ->
                     argumentoj.AddLast(argumento) |> ignore
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
               let bezonitajArgumentojKvanto = bezonitaArgumentoKvantode sekva
               let bezonitajArgumentoj = 
                  if bezonitajArgumentojKvanto > 0 then
                     seq { 1..bezonitajArgumentojKvanto }
                     |> Seq.fold (fun listo _ ->
                        this.LegiArgumenton()
                        |> Result.bind (fun argumento ->
                           listo
                           |> Result.map (fun l -> argumento :: l))) (Ok [])
                     |> Result.map List.rev
                  elif bezonitajArgumentojKvanto = -1 then
                     let argumento = argumentoj.Last.Value
                     argumentoj.RemoveLast()
                     Ok [argumento]
                  else
                     Ok []
               bezonitajArgumentoj
               |> Result.bind (fun argumentoj ->
                  atendantajPredikatoj.AddLast
                     ({ Verbo = verbo
                        Valenco = valenco })
                  |> ignore
                  this.LegiSekvan())
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let sekva = enira.Dequeue()
         if ĉuDifinita sekva then
            this.LegiPlenanArgumenton(sekva)
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
            this.AldoniPridiranton(Pridiranto(sekva))
            this.LegiArgumenton()
         else
            failwith "Unexpected input"

      member private this.LegiPlenanArgumenton(sekva) =
         let novaArgumento =
            this.LegiModifantojnPor sekva
            |> Result.map (fun pliajModifantoj ->
               let argumento =
                  plenaModifitaArgumento sekva (Seq.append pliajModifantoj atendantajPridirantoj)

               lastaModifeblaVorto <- Some(ModifeblaArgumento argumento)
               lastaArgumento <- Some argumento
               argumento)
         atendantajPridirantoj.Clear()
         novaArgumento

      member private this.AldoniPridiranton(pridiranto) =
         if atendantajPredikatoj.Count > 0 then
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
      
      member private this.LegiMalantaŭanEcon(): Argumento = argumento (enira.Dequeue()) []

      member private this.LegiĈiujnMalantaŭajnModifantojn() =
         let modifantoj = HashSet()
         while enira.Count > 0
               && ĉuMalantaŭModifantaVorto (enira.Peek()) do
            let sekva = enira.Dequeue()
            modifantoj.Add(Pridiranto(sekva)) |> ignore
         modifantoj

      member private this.LegiModifantojnPor(vorto: MalinflektitaVorto): Result<Modifanto list, Eraro> =
         vorto.InflekcioŜtupoj
         |> List.fold (fun ak sek ->
            ak
            |> Result.bind (fun listo ->
               let modifanto =
                  match sek with
                  | Nebazo(_, inflekcio, _) -> Ok None
                  | Bazo(vorttipo, _, _) ->
                     match vorttipo with
                     | AntaŭNombrigeblaEco
                     | AntaŭNenombrigeblaEco ->
                        this.LegiArgumenton()
                        |> Result.map (fun argumento ->
                           Some(EcoDe(argumento)))
                     | _ -> Ok None
               modifanto
               |> Result.map (fun modifanto ->
                  modifanto
                  |> Option.map (fun modifanto -> modifanto :: listo)
                  |> Option.defaultValue listo))
            ) (Ok [])