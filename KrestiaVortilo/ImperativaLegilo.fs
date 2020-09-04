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

   type ImperativaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let atendantajPridirantoj = LinkedList<Modifanto>()
      let atendantajPredikatoj = LinkedList<AtendantaPredikato>()
      let lastaModifeblaVorto = Stack<LastaLegitaModifeblaVorto>()
      let lastaModifeblaVerbo = Stack<Verbo>()
      let lastaModifeblaArgumento = Stack<Argumento>()
      let mutable lastaArgumento: Argumento option = None

      member this.Legi(): Result<Rezulto, Eraro> =
         this.LegiSekvan() |> Result.bind this.LegiFrazojn

      member private this.LegiFrazojn(): Result<Rezulto, Eraro> =
         atendantajPredikatoj
         |> Seq.fold (fun listo sekva ->
               listo
               |> Result.bind (fun listo ->
                     seq { 1 .. sekva.Valenco }
                     |> Seq.fold (fun ak _ ->
                           ak
                           |> Result.bind (fun ak ->
                                 if argumentoj.Count = 0 then
                                    Error(Eraro(sekva.Verbo.Vorto.Kapo.OriginalaVorto, sprintf "Not enough arguments"))
                                 else
                                    let argumento = argumentoj.First.Value
                                    argumentoj.RemoveFirst()
                                    Ok(argumento :: ak))) (Ok [])
                     |> Result.map List.rev
                     |> Result.map (fun argumentoj ->
                           { Kapo = sekva.Verbo
                             Argumentoj = argumentoj }
                           :: listo))) (Ok [])
         |> Result.map List.rev
         |> Result.map (fun frazoj ->
               { Frazoj = frazoj
                 Argumentoj = argumentoj |> List.ofSeq })

      member private this.LegiLokalanFrazon(): Result<Predikato, Eraro> = failwith "TODO"

      member private this.LegiSekvan(): Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuArgumentaVorto sekva then
               this.LegiArgumenton()
               |> Result.bind (fun argumento ->
                     argumentoj.AddLast(argumento) |> ignore
                     this.LegiSekvan())
            elif ĉuAntaŭModifantaVorto sekva then
               this.LegiPridiranton()
               |> Result.bind (fun pridiranto ->
                     atendantajPridirantoj.AddLast(pridiranto)
                     |> ignore
                     this.LegiSekvan())
            elif ĉuMalantaŭModifantaVorto sekva then
               let lastaVorto = lastaModifeblaVorto.Peek()
               this.LegiPridiranton()
               |> Result.bind (fun pridiranto ->
                     let modifotaVorto =
                        if ĉuAntaŭEco sekva then lastaVorto else lastaModifeblaVorto.Peek()

                     this.AldoniPridiranton pridiranto modifotaVorto
                     this.LegiSekvan())
            elif ĉuPredikataVorto sekva then
               this.LegiPredikaton()
               |> Result.bind (fun verbo ->
                     let valenco = valencoDe sekva
                     atendantajPredikatoj.AddLast({ Verbo = verbo; Valenco = valenco })
                     |> ignore
                     this.LegiSekvan())
            elif ĉuMalantaŭModifanto sekva then
               this.LegiMalantaŭModifanton()
               |> Result.bind this.LegiSekvan
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let sekva = enira.Peek()
         if ĉuCifero sekva.OriginalaVorto.Vorto then
            this.LegiNombron true
            |> Result.map (fun ciferoj ->
                  let nombro = Decimal.Parse(ciferoj)
                  let argumento = Nombro nombro
                  lastaModifeblaVorto.Push(ModifeblaArgumento argumento)
                  lastaArgumento <- Some argumento
                  argumento)
         elif ĉuDifinita sekva || ĉuLokokupilo sekva.BazaVorto then
            this.LegiPlenanArgumenton(enira.Dequeue())
         elif ĉuAntaŭModifantaVorto sekva then
            this.LegiPridiranton()
            |> Result.bind (fun pridiranto ->
                  atendantajPridirantoj.AddLast(pridiranto)
                  |> ignore
                  this.LegiArgumenton())
         elif ĉuMalantaŭModifantaVorto sekva then
            this.LegiPridiranton()
            |> Result.bind (fun pridiranto ->
                  this.AldoniPridiranton pridiranto (lastaModifeblaVorto.Peek())
                  this.LegiArgumenton())
         else
            failwith "Unexpected input"

      member private this.LegiPlenanArgumenton sekva =
         let novaArgumento =
            this.LegiModifantojnPor sekva
            |> Result.map (fun pliajModifantoj ->
                  let argumento =
                     plenaModifitaArgumento sekva (Seq.append pliajModifantoj atendantajPridirantoj)

                  lastaModifeblaVorto.Push(ModifeblaArgumento argumento)
                  lastaArgumento <- Some argumento
                  argumento)

         atendantajPridirantoj.Clear()
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

      member private this.LegiPredikaton() =
         let sekva = enira.Dequeue()
         this.LegiModifantojnPor sekva
         |> Result.map (fun modifantoj ->
               let novaVerbo =
                  verbo
                     sekva
                     (Seq.append modifantoj atendantajPridirantoj
                      |> List.ofSeq)

               atendantajPridirantoj.Clear()
               lastaModifeblaVorto.Push(ModifeblaVerbo novaVerbo)
               lastaModifeblaVerbo.Push(novaVerbo)
               novaVerbo)

      member private this.LegiPridiranton(): Result<Modifanto, Eraro> =
         let sekva = enira.Dequeue()
         this.LegiModifantojnPor sekva
         |> Result.map (fun modifantoj -> Pridiranto <| argumento sekva modifantoj)

      member private this.LegiModifantojnPor(vorto: MalinflektitaVorto): Result<Modifanto list, Eraro> =
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
                                    this.LegiArgumenton()
                                    |> Result.map (fun argumento -> Some(EcoDe(argumento)))

                                 rezulto
                           | MalantaŭNombrigeblaEco
                           | MalantaŭNenombrigeblaEco ->
                              if argumentoj.Count = 0 then
                                 Error(Eraro(vorto.OriginalaVorto, "No precedent argument to associate with"))
                              else
                                 let argumento = argumentoj.Last.Value
                                 argumentoj.RemoveLast()
                                 match lastaModifeblaVorto.Peek() with
                                 | ModifeblaArgumento (_) -> lastaModifeblaVorto.Pop() |> ignore
                                 | _ -> ()
                                 Ok(Some(EcoDe argumento))
                           | _ -> Ok None

                     modifanto
                     |> Result.map (fun modifanto ->
                           modifanto
                           |> Option.map (fun modifanto -> modifanto :: listo)
                           |> Option.defaultValue listo))) (Ok [])

      member private this.LegiMalantaŭModifanton() =
         let sekva = enira.Dequeue()
         match sekva.BazaVorto with
         | "nomil" ->
            this.LegiLokalanFrazon()
            |> Result.bind (fun frazo ->
                  let nomil = Nomil(frazo)
                  (if lastaModifeblaVerbo.Count = 0
                   then Error(Eraro(sekva.OriginalaVorto, "No verb to modify"))
                   else Ok(lastaModifeblaVerbo.Pop()))
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
