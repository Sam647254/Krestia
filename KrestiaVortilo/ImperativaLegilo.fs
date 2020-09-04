namespace KrestiaVortilo

open System
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
      { Kapo = argumento
        Modifantoj = HashSet() }
      |> ArgumentaVorto

   let plenaModifitaArgumento argumento (modifantoj: Modifanto seq): Argumento =
      { Kapo = argumento
        Modifantoj = HashSet(modifantoj) }
      |> ArgumentaVorto

   type private AtendantaPredikato = { Verbo: Verbo; Valenco: int }

   type private LastaLegitaModifeblaVorto =
      | ModifeblaVerbo of Verbo
      | ModifeblaArgumento of Argumento
   
   type private Konteksto =
      { Argumentoj : LinkedList<Argumento>
        AtendantajPridirantoj : LinkedList<Modifanto>
        AtendantajPredikatoj : LinkedList<AtendantaPredikato>
        LastaModifeblaVorto : LinkedList<LastaLegitaModifeblaVorto>
        LastaModifeblaVerbo : LinkedList<Verbo>
        LastaModifeblaArgumento : LinkedList<Argumento> }
  
   let private konteksto =
      { Argumentoj = LinkedList<Argumento>()
        AtendantajPridirantoj = LinkedList<Modifanto>()
        AtendantajPredikatoj = LinkedList<AtendantaPredikato>()
        LastaModifeblaVorto = LinkedList<LastaLegitaModifeblaVorto>()
        LastaModifeblaVerbo = LinkedList<Verbo>()
        LastaModifeblaArgumento = LinkedList<Argumento>() }

   type ImperativaLegilo(enira: Queue<MalinflektitaVorto>) =

      member this.Legi(): Result<Rezulto, Eraro> =
         let bazaKonteksto = konteksto
         this.LegiSekvan bazaKonteksto
         |> Result.bind (fun _ ->
            if enira.Count > 0 then
               this.LegiSekvan bazaKonteksto
            else
               Ok())
         |> Result.bind (fun _ -> this.LegiFrazojn konteksto)

      member private this.LegiFrazojn konteksto: Result<Rezulto, Eraro> =
         konteksto.AtendantajPredikatoj
         |> Seq.fold (fun listo sekva ->
               listo
               |> Result.bind (fun listo ->
                     seq { 1 .. sekva.Valenco }
                     |> Seq.fold (fun ak _ ->
                           ak
                           |> Result.bind (fun ak ->
                                 if konteksto.Argumentoj.Count = 0 then
                                    Error(Eraro(sekva.Verbo.Vorto.Kapo.OriginalaVorto, sprintf "Not enough arguments"))
                                 else
                                    let argumento = konteksto.Argumentoj.First.Value
                                    konteksto.Argumentoj.RemoveFirst()
                                    Ok(argumento :: ak))) (Ok [])
                     |> Result.map List.rev
                     |> Result.map (fun argumentoj ->
                           { Kapo = sekva.Verbo
                             Argumentoj = argumentoj }
                           :: listo))) (Ok [])
         |> Result.map List.rev
         |> Result.map (fun frazoj ->
               { Frazoj = frazoj
                 Argumentoj = konteksto.Argumentoj |> List.ofSeq })

      member private this.LegiLokalanFrazon(): Result<Predikato, Eraro> = failwith "TODO"

      member private this.LegiSekvan konteksto: Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuArgumentaVorto sekva then
               this.LegiArgumenton konteksto
               |> Result.bind (fun argumento ->
                     konteksto.Argumentoj.AddLast(argumento) |> ignore |> Ok)
            elif ĉuAntaŭModifantaVorto sekva then
               this.LegiPridiranton konteksto
               |> Result.bind (fun pridiranto ->
                     konteksto.AtendantajPridirantoj.AddLast(pridiranto)
                     |> ignore |> Ok)
            elif ĉuMalantaŭModifantaVorto sekva then
               let lastaVorto = konteksto.LastaModifeblaVorto.Last.Value
               this.LegiPridiranton konteksto
               |> Result.bind (fun pridiranto ->
                     let modifotaVorto =
                        if ĉuAntaŭEco sekva then lastaVorto else konteksto.LastaModifeblaVorto.Last.Value

                     this.AldoniPridiranton pridiranto modifotaVorto
                     Ok())
            elif ĉuPredikataVorto sekva then
               this.LegiPredikaton konteksto
               |> Result.bind (fun verbo ->
                     let valenco = valencoDe sekva
                     konteksto.AtendantajPredikatoj.AddLast({ Verbo = verbo; Valenco = valenco })
                     |> ignore
                     |> Ok)
            elif ĉuMalantaŭModifanto sekva then
               this.LegiMalantaŭModifanton konteksto
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton konteksto: Result<Argumento, Eraro> =
         let sekva = enira.Peek()
         if ĉuCifero sekva.OriginalaVorto.Vorto then
            this.LegiNombron true
            |> Result.map (fun ciferoj ->
                  let nombro = Decimal.Parse(ciferoj)
                  let argumento = Nombro nombro
                  konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento argumento) |> ignore
                  argumento)
         elif ĉuDifinita sekva || ĉuLokokupilo sekva.BazaVorto then
            this.LegiPlenanArgumenton (enira.Dequeue()) konteksto
         elif ĉuAntaŭModifantaVorto sekva then
            this.LegiPridiranton konteksto
            |> Result.bind (fun pridiranto ->
                  konteksto.AtendantajPridirantoj.AddLast(pridiranto)
                  |> ignore
                  this.LegiArgumenton konteksto)
         elif ĉuMalantaŭModifantaVorto sekva then
            this.LegiPridiranton konteksto
            |> Result.bind (fun pridiranto ->
                  this.AldoniPridiranton pridiranto (konteksto.LastaModifeblaVorto.Last.Value)
                  this.LegiArgumenton konteksto)
         else
            failwith "Unexpected input"

      member private this.LegiPlenanArgumenton sekva konteksto =
         let novaArgumento =
            this.LegiModifantojnPor sekva konteksto
            |> Result.map (fun pliajModifantoj ->
                  let argumento =
                     plenaModifitaArgumento sekva (Seq.append pliajModifantoj konteksto.AtendantajPridirantoj)

                  konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento argumento) |> ignore
                  argumento)

         konteksto.AtendantajPridirantoj.Clear()
         novaArgumento

      member private this.LegiNombron ĉuKomenca =
         let sekva = enira.Dequeue()
         if ĉuFinaCifero sekva.BazaVorto then
            if ĉuKomenca then
               komencajFinajCiferoj
               |> Map.tryFind sekva.BazaVorto
               |> Option.defaultValue finajCiferoj.[sekva.BazaVorto]
               |> Ok
            else
               finajCiferoj.[sekva.BazaVorto] |> Ok
         elif ĉuNefinaCifero sekva.BazaVorto then
            let cifero =
               if ĉuKomenca then
                  komencajNefinajCiferoj
                  |> Map.tryFind sekva.BazaVorto
                  |> Option.defaultValue (nefinajCiferoj.[sekva.BazaVorto])
               else
                  nefinajCiferoj.[sekva.BazaVorto]

            this.LegiNombron false
            |> Result.map (fun restantaj -> cifero + restantaj)
         else
            Error(Eraro(sekva.OriginalaVorto, "Could not read digit"))

      member private this.AldoniPridiranton (pridiranto: Modifanto) vorto =
         match vorto with
         | ModifeblaVerbo (verbo) -> verbo.Vorto.Modifantoj.Add(pridiranto) |> ignore
         | ModifeblaArgumento (argumento) ->
            match argumento with
            | ArgumentaVorto (vorto) -> vorto.Modifantoj.Add(pridiranto)
            | _ -> failwith "???"
            |> ignore

      member private this.LegiPredikaton konteksto =
         let sekva = enira.Dequeue()
         this.LegiModifantojnPor sekva konteksto
         |> Result.map (fun modifantoj ->
               let novaVerbo =
                  verbo
                     sekva
                     (Seq.append modifantoj konteksto.AtendantajPridirantoj
                      |> List.ofSeq)

               konteksto.AtendantajPridirantoj.Clear()
               konteksto.LastaModifeblaVorto.AddLast(ModifeblaVerbo novaVerbo) |> ignore
               konteksto.LastaModifeblaVerbo.AddLast(novaVerbo) |> ignore
               novaVerbo)

      member private this.LegiPridiranton konteksto: Result<Modifanto, Eraro> =
         let sekva = enira.Dequeue()
         this.LegiModifantojnPor sekva konteksto
         |> Result.map (fun modifantoj -> Pridiranto <| argumento sekva modifantoj)

      member private this.LegiModifantojnPor (vorto: MalinflektitaVorto) konteksto: Result<Modifanto list, Eraro> =
         vorto.InflekcioŜtupoj
         |> List.fold (fun ak sek ->
               ak
               |> Result.bind (fun listo ->
                     let modifanto =
                        match sek with
                        | Nebazo (vorttipo, inflekcio, _) -> Ok None
                        | Bazo (vorttipo, _, _) ->
                           match vorttipo with
                           | AntaŭNombrigeblaEco
                           | AntaŭNenombrigeblaEco ->
                              if ĉuEcoHavaĵo vorto then
                                 Ok None
                              else
                                 let rezulto =
                                    this.LegiArgumenton konteksto
                                    |> Result.map (fun argumento -> Some(EcoDe(argumento)))

                                 rezulto
                           | MalantaŭNombrigeblaEco
                           | MalantaŭNenombrigeblaEco ->
                              if konteksto.Argumentoj.Count = 0 then
                                 Error(Eraro(vorto.OriginalaVorto, "No precedent argument to associate with"))
                              else
                                 let argumento = konteksto.Argumentoj.Last.Value
                                 konteksto.Argumentoj.RemoveLast()
                                 match konteksto.LastaModifeblaVorto.Last.Value with
                                 | ModifeblaArgumento (_) -> konteksto.LastaModifeblaVorto.RemoveLast() |> ignore
                                 | _ -> ()
                                 Ok(Some(EcoDe argumento))
                           | _ -> Ok None

                     modifanto
                     |> Result.map (fun modifanto ->
                           modifanto
                           |> Option.map (fun modifanto -> modifanto :: listo)
                           |> Option.defaultValue listo))) (Ok [])

      member private this.LegiMalantaŭModifanton konteksto =
         let sekva = enira.Dequeue()
         match sekva.BazaVorto with
         | "nomil" ->
            this.LegiLokalanFrazon()
            |> Result.bind (fun frazo ->
                  let nomil = Nomil(frazo)
                  (if konteksto.LastaModifeblaVerbo.Count = 0
                   then Error(Eraro(sekva.OriginalaVorto, "No verb to modify"))
                   else
                      let lastaVerbo = konteksto.LastaModifeblaVerbo.Last.Value
                      konteksto.LastaModifeblaVerbo.RemoveLast()
                      Ok(lastaVerbo))
                  |> Result.map (fun lastaVerbo -> lastaVerbo.Vorto.Modifantoj.Add(nomil) |> ignore))
         | _ -> failwith "Unexpected input"

   let legiImperative (eniro: string) =
      prepariEniron eniro false
      |> Result.bind (fun vortoj -> ImperativaLegilo(Queue(vortoj)).Legi())

   let proveLegiNombron (eniro: string) =
      legiImperative eniro
      |> Result.map (fun rezulto ->
            List.tryHead rezulto.Argumentoj
            |> Option.bind (fun argumento ->
                  match argumento with
                  | Nombro (nombro) -> Some nombro
                  | _ -> None))
