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

   let plenaModifitaArgumento argumento (modifantoj: Modifanto seq) =
      let modifeblaVorto =
         { Kapo = argumento
           Modifantoj = HashSet(modifantoj) }
      ArgumentaVorto modifeblaVorto, modifeblaVorto

   type private AtendantaPredikato = { Verbo: Verbo; Valenco: int }

   type private LastaLegitaModifeblaVorto =
      | ModifeblaVerbo of Verbo
      | ModifeblaArgumento of Argumento

   type private Konteksto =
      { Argumentoj: LinkedList<Argumento>
        AtendantajPridirantoj: LinkedList<Modifanto>
        AtendantajPredikatoj: LinkedList<AtendantaPredikato>
        LastaModifeblaVorto: LinkedList<LastaLegitaModifeblaVorto>
        LastaModifeblaVerbo: LinkedList<Verbo>
        LastaModifeblaArgumento: LinkedList<Argumento>
        LegitajModifeblajVortoj: LinkedList<ModifeblaVorto> }

   type ImperativaLegilo(enira: Queue<MalinflektitaVorto>) =

      member this.Legi(): Result<Rezulto, Eraro> =
         let konteksto =
            { Argumentoj = LinkedList<Argumento>()
              AtendantajPridirantoj = LinkedList<Modifanto>()
              AtendantajPredikatoj = LinkedList<AtendantaPredikato>()
              LastaModifeblaVorto = LinkedList<LastaLegitaModifeblaVorto>()
              LastaModifeblaVerbo = LinkedList<Verbo>()
              LastaModifeblaArgumento = LinkedList<Argumento>()
              LegitajModifeblajVortoj = LinkedList<ModifeblaVorto>() }

         let rec legiAk (): Result<Rezulto, Eraro> =
            this.LegiSekvan konteksto
            |> Result.bind (fun _ -> if enira.Count > 0 then legiAk () else this.LegiFrazojn konteksto)

         legiAk ()

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

      member private this.LegiLokalanFrazon bazaKonteksto: Result<Predikato, Eraro> =
         let konteksto =
            { Argumentoj = LinkedList<Argumento>()
              AtendantajPridirantoj = bazaKonteksto.AtendantajPridirantoj
              AtendantajPredikatoj = LinkedList<AtendantaPredikato>()
              LastaModifeblaVorto = bazaKonteksto.LastaModifeblaVorto
              LastaModifeblaVerbo = bazaKonteksto.LastaModifeblaVerbo
              LastaModifeblaArgumento = bazaKonteksto.LastaModifeblaArgumento
              LegitajModifeblajVortoj = bazaKonteksto.LegitajModifeblajVortoj }

         let rec legiAk () =
            this.LegiSekvan konteksto
            |> Result.bind (fun _ ->
                  if konteksto.AtendantajPredikatoj.Count = 0
                     || konteksto.Argumentoj.Count < konteksto.AtendantajPredikatoj.First.Value.Valenco then
                     legiAk ()
                  else
                     let predikato =
                        { Kapo = konteksto.AtendantajPredikatoj.First.Value.Verbo
                          Argumentoj = List.ofSeq konteksto.Argumentoj }

                     konteksto.Argumentoj
                     |> Seq.map bazaKonteksto.Argumentoj.AddLast
                     |> ignore

                     Ok predikato)

         legiAk ()

      member private this.LegiSekvan konteksto: Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuArgumentaVorto sekva then
               this.LegiArgumenton konteksto
               |> Result.bind (fun argumento ->
                     konteksto.Argumentoj.AddLast(argumento)
                     |> ignore
                     |> Ok)
            elif ĉuAntaŭModifantaVorto sekva then
               this.LegiPridiranton konteksto
               |> Result.bind (fun pridiranto ->
                     konteksto.AtendantajPridirantoj.AddLast(pridiranto)
                     |> ignore
                     |> Ok)
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
            failwith "Unexpected end of input"

      member private this.LegiArgumenton konteksto: Result<Argumento, Eraro> =
         let sekva = enira.Peek()
         if ĉuCifero sekva.OriginalaVorto.Vorto then
            this.LegiNombron true
            |> Result.map (fun ciferoj ->
                  let nombro = Decimal.Parse(ciferoj)
                  Nombro nombro)
         elif ĉuDifinita sekva then
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
         elif ĉuMalantaŭModifanto sekva then
            this.LegiMalantaŭModifanton konteksto
            |> Result.bind (fun () ->
               this.LegiArgumenton konteksto)
         else
            Error(Eraro(sekva.OriginalaVorto, "Unexpected input"))

      member private this.LegiPlenanArgumenton sekva konteksto =
         let novaArgumento =
            if sekva.BazaVorto = "mine" then
               this.LegiLokalanFrazon konteksto
               |> Result.map (fun frazo -> Mine(sekva, frazo))
            elif sekva.BazaVorto = "ene" then
               this.LegiLokalanFrazon konteksto
               |> Result.map (fun frazo -> Ene(sekva, frazo))
            elif sekva.BazaVorto = "keni" then
               let keni = senmodifantaVorto sekva
               let keniArgumento = ArgumentaVorto keni
               konteksto.LastaModifeblaArgumento.AddLast(keniArgumento) |> ignore
               konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento keniArgumento) |> ignore
               konteksto.LegitajModifeblajVortoj.AddLast(keni) |> ignore
               this.LegiArgumenton konteksto
               |> Result.bind (fun argumento1 ->
                  this.LegiArgumenton konteksto
                  |> Result.map (fun argumento2 ->
                     
                     Keni(keni, argumento1, argumento2)))
            elif sekva.BazaVorto = "pini" then
               let pini = senmodifantaVorto sekva
               let piniArgumento = ArgumentaVorto pini
               konteksto.LastaModifeblaArgumento.AddLast(piniArgumento) |> ignore
               konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento piniArgumento) |> ignore
               konteksto.LegitajModifeblajVortoj.AddLast(pini) |> ignore
               this.LegiArgumenton konteksto
               |> Result.bind (fun argumento1 ->
                  this.LegiArgumenton konteksto
                  |> Result.bind (fun argumento2 ->
                     this.LegiArgumenton konteksto
                     |> Result.map (fun argumento3 ->
                     
                        Pini(pini, argumento1, argumento2, argumento3))))
            else
               this.LegiModifantojnPor sekva konteksto
               |> Result.map (fun pliajModifantoj ->
                     let (argumento, vorto) =
                        plenaModifitaArgumento sekva (Seq.append pliajModifantoj konteksto.AtendantajPridirantoj)

                     konteksto.AtendantajPridirantoj.Clear()
                     konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento argumento)
                     |> ignore
                     konteksto.LastaModifeblaArgumento.AddLast argumento
                     |> ignore
                     konteksto.LegitajModifeblajVortoj.AddLast vorto |> ignore
                     argumento)

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
         | ModifeblaArgumento (argumento) -> this.AldoniModifantonAlArgumento pridiranto argumento


      member private this.AldoniModifantonAlArgumento pridiranto vorto =
         match vorto with
         | ArgumentaVorto vorto -> vorto.Modifantoj.Add(pridiranto)
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
               konteksto.LastaModifeblaVorto.AddLast(ModifeblaVerbo novaVerbo)
               |> ignore
               konteksto.LastaModifeblaVerbo.AddLast(novaVerbo)
               |> ignore
               novaVerbo)

      member private this.LegiPridiranton konteksto: Result<Modifanto, Eraro> =
         let sekva = enira.Dequeue()
         this.LegiModifantojnPor sekva konteksto
         |> Result.map (fun modifantoj ->
            let (argumento, vorto) = plenaModifitaArgumento sekva modifantoj
            konteksto.LegitajModifeblajVortoj.AddLast vorto |> ignore
            Pridiranto argumento)

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
                                 | ModifeblaArgumento (_) ->
                                    konteksto.LastaModifeblaVorto.RemoveLast()
                                    |> ignore
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
         | _ when modifantojDePredikatoKunFrazo.ContainsKey(sekva.BazaVorto) ->
            let modifanto = modifantojDePredikatoKunFrazo.[sekva.BazaVorto]
            (if konteksto.LastaModifeblaVerbo.Count = 0 then
               Error(Eraro(sekva.OriginalaVorto, "No verb to modify"))
             else
                let lastaVerbo = konteksto.LastaModifeblaVerbo.Last.Value
                konteksto.LastaModifeblaVerbo.RemoveLast()
                Ok(lastaVerbo))
            |> Result.bind (fun lastaVerbo ->
                  this.LegiLokalanFrazon konteksto
                  |> Result.map (fun frazo ->
                        let plenaModifanto = modifanto frazo
                        lastaVerbo.Vorto.Modifantoj.Add(plenaModifanto) |> ignore))
         | "nivoral" ->
            konteksto.LastaModifeblaVerbo.Last.Value.Vorto.Modifantoj.Add(Nivoral)
            |> ignore
            |> Ok
         | "sivil" ->
            konteksto.LastaModifeblaVerbo.Last.Value.Vorto.Modifantoj.Add(Sivil)
            |> ignore
            |> Ok
         | "borol" ->
            this.AldoniModifantonAlArgumento Borol konteksto.LastaModifeblaArgumento.Last.Value
            |> Ok
         | "nil" ->
            let lastaVorto = konteksto.LastaModifeblaVorto.Last.Value
            match lastaVorto with
            | ModifeblaArgumento _ -> konteksto.LastaModifeblaArgumento.RemoveLast()
            | ModifeblaVerbo _ -> konteksto.LastaModifeblaVerbo.RemoveLast()
            konteksto.LastaModifeblaVorto.RemoveLast()
            konteksto.LegitajModifeblajVortoj.RemoveLast()
            |> Ok
         | _ when modifantojDeVortoKunArgumento.ContainsKey(sekva.BazaVorto) ->
            let modifanto = modifantojDeVortoKunArgumento.[sekva.BazaVorto]
            let lastaVorto = konteksto.LegitajModifeblajVortoj.Last.Value
            this.LegiArgumenton konteksto
            |> Result.map (fun argumento ->
               let novaModifanto = modifanto argumento
               lastaVorto.Modifantoj.Add(novaModifanto) |> ignore)
         | _ -> Error(Eraro(sekva.OriginalaVorto, "Unexpected input"))
   
      member private this.LegitajVortoj konteksto =
         let mutable vorto = konteksto.LegitajModifeblajVortoj.Last
         seq {
            for _ in 0..konteksto.LegitajModifeblajVortoj.Count do
               yield vorto.Value
               vorto <- vorto.Previous
         }

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
