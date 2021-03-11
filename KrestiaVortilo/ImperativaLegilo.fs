namespace KrestiaVortilo

open System
open System.Collections.Generic
open System.IO
open System.Net
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo
open KrestiaVortilo.Sintaksanalizilo2
open Iloj

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
      | ModifeblaNombro of Argumento

   type private Konteksto =
      { Argumentoj: LinkedList<Argumento>
        AtendantajPridirantoj: LinkedList<Modifanto>
        AtendantajModifantoj: LinkedList<Modifanto * MalinflektitaVorto>
        AtendantajPredikatoj: LinkedList<AtendantaPredikato>
        LastaModifeblaVorto: LinkedList<LastaLegitaModifeblaVorto>
        LastaModifeblaVerbo: LinkedList<Verbo>
        LastaModifeblaArgumento: LinkedList<Argumento>
        LegitajModifeblajVortoj: LinkedList<ModifeblaVorto>
        LegitajNombroj: LinkedList<Argumento> }

   type ModifantoEnVortaro =
      { PlenaVorto: string
        ModifeblajVorttipoj: Set<Vorttipo>
        ModifantoInflekcioj: Inflekcio list }

   let private vortarajVorttipoj =
      [ 'N', NombrigeblaKlaso
        'n', NenombrigeblaKlaso
        'C', Cifero
        '0', MalplenaVerbo
        '1', NetransitivaVerbo
        '2', TransitivaVerbo
        '3', DutransitivaVerbo
        '4', OblikaNetransitivaVerbo
        '5', OblikaTransitivaVerbo
        '6', NedirektaNetransitivaVerbo
        '7', NedirektaTransitivaVerbo ]
      |> Map.ofList

   let private vortarajInflekcioj =
      [ 'D', Difinito
        'N', SolaFormo
        'F', Inflekcio.FremdaVorto
        'P', Predikato
        'C', Inflekcio.Cifero ]
      |> Map.ofList

   let alportiModifantojn =
      async {
         let peto =
            WebRequest.Create("https://raw.githubusercontent.com/Sam647254/Krestia/master/vortaro.kv")

         let! respondo = peto.AsyncGetResponse()

         return
            seq {
               use stream = respondo.GetResponseStream()
               use streamReader = new StreamReader(stream)
               let mutable vico = streamReader.ReadLine()

               while vico <> null do
                  yield vico
                  vico <- streamReader.ReadLine()
            }
      }
      |> Async.RunSynchronously
      |> Seq.choose (fun vico ->
            let partoj = vico.Split('|')

            if not (partoj.[0].EndsWith("l"))
               || partoj.[0].StartsWith("h") then
               None
            else
               let vorttipoj =
                  partoj.[5].ToCharArray()
                  |> Seq.map (fun c ->
                        Map.tryFind c vortarajVorttipoj
                        |> Option.defaultWith (fun () -> failwith (sprintf "Nevalida: %c" c)))

               let inflekcioj =
                  partoj.[6].ToCharArray()
                  |> Seq.map (fun c ->
                        Map.tryFind c vortarajInflekcioj
                        |> Option.defaultWith (fun () -> failwith (sprintf "Nevalida: %c" c)))

               { PlenaVorto = partoj.[0]
                 ModifeblajVorttipoj = Set.ofSeq vorttipoj
                 ModifantoInflekcioj = List.ofSeq inflekcioj }
               |> Some)

   let validajModifantoj =
      alportiModifantojn
      |> Seq.map (fun m -> m.PlenaVorto, m)
      |> Map.ofSeq

   let ĉuPovasModifi (modifanto: ModifantoEnVortaro) (vorto: ModifeblaVorto) =
      vorto.Kapo.InflekcioŜtupoj
      |> List.exists (fun ŝtupo ->
            match ŝtupo with
            | Nebazo (_, i, _, _) ->
               egalajVorttipoj.TryFind i
               |> Option.map modifanto.ModifeblajVorttipoj.Contains
               |> Option.defaultValue false
            | Bazo (v, _, _) -> modifanto.ModifeblajVorttipoj.Contains v)

   let ĉuHavasInflekcion (vorto: Argumento) inflekcio =
      match vorto with
      | ArgumentaVorto argumento ->
         match List.last argumento.Kapo.InflekcioŜtupoj with
         | Nebazo (_, i, _, _)
         | Bazo (_, i, _) ->
            i = inflekcio
            || (inflekcio = Difinito && i = Inflekcio.FremdaVorto)
      | ArgumentaNombro n -> inflekcio = Inflekcio.Cifero

   type ImperativaLegilo(enira: Queue<MalinflektitaVorto>) =

      member this.Legi(): Result<Rezulto, Eraro> =
         let konteksto =
            { Argumentoj = LinkedList<Argumento>()
              AtendantajPridirantoj = LinkedList<Modifanto>()
              AtendantajModifantoj = LinkedList<Modifanto * MalinflektitaVorto>()
              AtendantajPredikatoj = LinkedList<AtendantaPredikato>()
              LastaModifeblaVorto = LinkedList<LastaLegitaModifeblaVorto>()
              LastaModifeblaVerbo = LinkedList<Verbo>()
              LastaModifeblaArgumento = LinkedList<Argumento>()
              LegitajModifeblajVortoj = LinkedList<ModifeblaVorto>()
              LegitajNombroj = LinkedList<Argumento>() }

         let rec legiAk (): Result<Rezulto, Eraro> =
            this.LegiSekvan konteksto
            |> Result.bind (fun _ -> if enira.Count > 0 then legiAk () else this.LegiFrazojn konteksto)

         legiAk ()

      member private this.LegiFrazojn konteksto: Result<Rezulto, Eraro> =
         rezulto {
            let! frazoj =
               konteksto.AtendantajPredikatoj
               |> foldR (fun listo sekva ->
                     seq { 1 .. sekva.Valenco }
                     |> foldR (fun ak _ ->
                           if konteksto.Argumentoj.Count = 0 then
                              Error
                              <| Eraro(sekva.Verbo.Vorto.Kapo.OriginalaVorto, sprintf "Not enough arguments")
                           else
                              let argumento = konteksto.Argumentoj.First.Value
                              konteksto.Argumentoj.RemoveFirst()
                              Ok(argumento :: ak)) []
                     |> Result.map List.rev
                     |> Result.map (fun argumentoj ->
                           { Kapo = sekva.Verbo
                             Argumentoj = argumentoj }
                           :: listo)) []
               |> Result.map List.rev

            if konteksto.AtendantajPridirantoj.Count > 0 then
               return!
                  Error
                  <| Eraro(malplenaEniraVorto, "Leftover modifiers")
            else
               return
                  { Frazoj = frazoj
                    Argumentoj = konteksto.Argumentoj |> List.ofSeq }
         }

      member private this.LegiLokalanFrazon bazaKonteksto: Result<Predikato, Eraro> =
         let konteksto =
            { bazaKonteksto with
                 Argumentoj = LinkedList<Argumento>()
                 AtendantajPredikatoj = LinkedList<AtendantaPredikato>() }

         let rec legiAk () =
            rezulto {
               let! _ = this.LegiSekvan konteksto

               if konteksto.AtendantajPredikatoj.Count = 0
                  || konteksto.Argumentoj.Count < konteksto.AtendantajPredikatoj.First.Value.Valenco then
                  return! legiAk ()
               else
                  let predikato =
                     { Kapo = konteksto.AtendantajPredikatoj.First.Value.Verbo
                       Argumentoj = List.ofSeq konteksto.Argumentoj }

                  konteksto.Argumentoj
                  |> Seq.map bazaKonteksto.Argumentoj.AddLast
                  |> ignore

                  return predikato
            }

         legiAk ()

      member private this.LegiSekvan konteksto: Result<unit, Eraro> =
         rezulto {
            if enira.Count > 0 then
               let sekva = enira.Peek()

               if ĉuArgumentaVorto sekva then
                  let! argumento = this.LegiArgumenton konteksto true
                  konteksto.Argumentoj.AddLast(argumento) |> ignore
               elif ĉuAntaŭModifantaVorto sekva then
                  let! pridiranto = this.LegiPridiranton konteksto

                  konteksto.AtendantajPridirantoj.AddLast(pridiranto)
                  |> ignore
               elif ĉuMalantaŭModifantaVorto sekva then
                  let lastaVorto = konteksto.LastaModifeblaVorto.Last.Value
                  let! pridiranto = this.LegiPridiranton konteksto

                  let modifotaVorto =
                     if ĉuAntaŭEco sekva then lastaVorto else konteksto.LastaModifeblaVorto.Last.Value

                  this.AldoniPridiranton pridiranto modifotaVorto
               elif ĉuPredikataVorto sekva then
                  let! verbo = this.LegiPredikaton konteksto
                  let valenco = valencoDe sekva

                  konteksto.AtendantajPredikatoj.AddLast({ Verbo = verbo; Valenco = valenco })
                  |> ignore
               elif ĉuMalantaŭModifanto sekva then
                  return! this.LegiMalantaŭanModifanton konteksto
               elif ĉuAntaŭModifanto sekva then
                  return! this.LegiAntaŭanModifanton konteksto
               else
                  return!
                     Error
                     <| Eraro(sekva.OriginalaVorto, sprintf "Unexpected input: %s" sekva.OriginalaVorto.Vorto)
            else
               return!
                  Error
                  <| Eraro(malplenaEniraVorto, "Unexpected end of input")
         }

      member private this.LegiArgumenton konteksto uziModifantojn: Result<Argumento, Eraro> =
         if enira.Count = 0 then
            Error(Eraro(malplenaEniraVorto, "Unexpected end of input"))
         else
            let sekva = enira.Peek()

            if ĉuCifero sekva.OriginalaVorto.Vorto then
               this.LegiNombron true
               |> Result.map (fun (vortoj, ciferoj) ->
                     let nombro =
                        ArgumentaNombro
                           { Nombro = List.rev vortoj
                             Valuo = Decimal.Parse(ciferoj)
                             Operacioj = Queue<Modifanto>() }

                     konteksto.LegitajNombroj.AddLast(nombro) |> ignore

                     konteksto.LastaModifeblaVorto.AddLast(ModifeblaNombro nombro)
                     |> ignore

                     nombro)
            elif ĉuDifinita sekva then
               this.LegiPlenanArgumenton (enira.Dequeue()) konteksto uziModifantojn
            elif ĉuAntaŭModifantaVorto sekva then
               this.LegiPridiranton konteksto
               |> Result.bind (fun pridiranto ->
                     konteksto.AtendantajPridirantoj.AddLast(pridiranto)
                     |> ignore

                     this.LegiArgumenton konteksto uziModifantojn)
            elif ĉuMalantaŭModifantaVorto sekva then
               this.LegiPridiranton konteksto
               |> Result.bind (fun pridiranto ->
                     this.AldoniPridiranton pridiranto (konteksto.LastaModifeblaVorto.Last.Value)
                     this.LegiArgumenton konteksto uziModifantojn)
            elif ĉuMalantaŭModifanto sekva then
               this.LegiMalantaŭanModifanton konteksto
               |> Result.bind (fun () -> this.LegiArgumenton konteksto uziModifantojn)
            else
               Error(Eraro(sekva.OriginalaVorto, "Unexpected input"))

      member private this.LegiPlenanArgumenton sekva konteksto uziModifantojn =
         let novaArgumento =
            let (argumento, vorto) =
               plenaModifitaArgumento sekva konteksto.AtendantajPridirantoj

            (if uziModifantojn then
               let modifantoj =
                  konteksto.AtendantajModifantoj
                  |> Seq.filter (fun (_, v) ->
                        let enVortaro =
                           validajModifantoj.[v.BazaVorto.Substring(0, v.BazaVorto.Length - 1)
                                              + "l"]

                        ĉuPovasModifi enVortaro vorto)
                  |> List.ofSeq

               modifantoj
               |> List.map (fun m -> konteksto.AtendantajModifantoj.Remove(m))
               |> ignore

               modifantoj
             else
                [])
            |> List.map (fun (m, _) -> vorto.Modifantoj.Add(m))
            |> ignore

            konteksto.AtendantajPridirantoj.Clear()

            konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento argumento)
            |> ignore

            konteksto.LastaModifeblaArgumento.AddLast argumento
            |> ignore

            konteksto.LegitajModifeblajVortoj.AddLast vorto
            |> ignore

            this.LegiModifantojnPor sekva konteksto
            |> Result.map (fun pliajModifantoj ->
                  pliajModifantoj
                  |> List.map (fun m ->
                        printf "%O" m
                        vorto.Modifantoj.Add(m))
                  |> ignore

                  argumento)

         novaArgumento

      member private this.LegiNombron ĉuKomenca =
         let sekva = enira.Dequeue()

         if ĉuFinaCifero sekva.BazaVorto then
            if ĉuKomenca then
               komencajFinajCiferoj
               |> Map.tryFind sekva.BazaVorto
               |> Option.defaultValue finajCiferoj.[sekva.BazaVorto]
               |> (fun ciferoj -> ([ sekva ], ciferoj))
               |> Ok
            else
               ([ sekva ], finajCiferoj.[sekva.BazaVorto]) |> Ok
         elif ĉuNefinaCifero sekva.BazaVorto then
            let cifero =
               if ĉuKomenca then
                  komencajNefinajCiferoj
                  |> Map.tryFind sekva.BazaVorto
                  |> Option.defaultValue (nefinajCiferoj.[sekva.BazaVorto])
               else
                  nefinajCiferoj.[sekva.BazaVorto]

            this.LegiNombron false
            |> Result.map (fun (vortoj, restantaj) -> (sekva :: vortoj, cifero + restantaj))
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

               konteksto.LegitajModifeblajVortoj.AddLast(novaVerbo.Vorto)
               |> ignore

               novaVerbo)

      member private this.LegiPridiranton konteksto: Result<Modifanto, Eraro> =
         let sekva = enira.Dequeue()

         this.LegiModifantojnPor sekva konteksto
         |> Result.map (fun modifantoj ->
               let (argumento, vorto) = plenaModifitaArgumento sekva modifantoj

               konteksto.LegitajModifeblajVortoj.AddLast vorto
               |> ignore

               Pridiranto argumento)

      member private this.LegiModifantojnPor (vorto: MalinflektitaVorto) konteksto: Result<Modifanto list, Eraro> =
         vorto.InflekcioŜtupoj
         |> List.fold (fun ak sek ->
               ak
               |> Result.bind (fun listo ->
                     let modifanto =
                        match sek with
                        | Nebazo _ -> Ok None
                        | Bazo (vorttipo, _, bazaVorto) ->
                           match vorttipo with
                           | AntaŭNombrigeblaEco
                           | AntaŭNenombrigeblaEco ->
                              if ĉuEcoHavaĵo vorto then
                                 Ok None
                              else
                                 let rezulto =
                                    this.LegiArgumenton konteksto true
                                    |> Result.map (EcoDe >> Some)

                                 rezulto
                           | MalantaŭNombrigeblaEco
                           | MalantaŭNenombrigeblaEco ->
                              if konteksto.Argumentoj.Count = 0 then
                                 Error(Eraro(vorto.OriginalaVorto, "No precedent argument to associate with"))
                              else
                                 let argumento = konteksto.Argumentoj.Last.Value
                                 konteksto.Argumentoj.RemoveLast()

                                 match konteksto.LastaModifeblaVorto.Last.Value with
                                 | ModifeblaArgumento (_) -> konteksto.LastaModifeblaVorto.RemoveLast()
                                 | _ -> ()

                                 Ok(Some(EcoDe argumento))
                           | NenombrigeblaKlaso ->
                              if bazaVorto = "mine" then
                                 this.LegiLokalanFrazon konteksto
                                 |> Result.map (Mine >> Some)
                              elif bazaVorto = "ene" then
                                 this.LegiLokalanFrazon konteksto
                                 |> Result.map (Ene >> Some)
                              elif bazaVorto = "keni" then
                                 let keni = senmodifantaVorto vorto
                                 let keniArgumento = ArgumentaVorto keni

                                 konteksto.LastaModifeblaArgumento.AddLast(keniArgumento)
                                 |> ignore

                                 konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento keniArgumento)
                                 |> ignore

                                 konteksto.LegitajModifeblajVortoj.AddLast(keni)
                                 |> ignore

                                 this.LegiArgumenton konteksto true
                                 |> Result.bind (fun argumento1 ->
                                       this.LegiArgumenton konteksto true
                                       |> Result.map (fun argumento2 -> Keni(argumento1, argumento2) |> Some))
                              elif bazaVorto = "pini" then
                                 let pini = senmodifantaVorto vorto
                                 let piniArgumento = ArgumentaVorto pini

                                 konteksto.LastaModifeblaArgumento.AddLast(piniArgumento)
                                 |> ignore

                                 konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento piniArgumento)
                                 |> ignore

                                 konteksto.LegitajModifeblajVortoj.AddLast(pini)
                                 |> ignore

                                 this.LegiArgumenton konteksto true
                                 |> Result.bind (fun argumento1 ->
                                       this.LegiArgumenton konteksto true
                                       |> Result.bind (fun argumento2 ->
                                             this.LegiArgumenton konteksto true
                                             |> Result.map (fun argumento3 ->
                                                   Pini(argumento1, argumento2, argumento3) |> Some)))
                              else
                                 Ok None
                           | _ -> Ok None

                     modifanto
                     |> Result.map (fun modifanto ->
                           modifanto
                           |> Option.map (fun modifanto -> modifanto :: listo)
                           |> Option.defaultValue listo))) (Ok [])

      member private this.LegiMalantaŭanModifanton konteksto: Result<unit, Eraro> =
         let legitajVortoj =
            konteksto.LegitajModifeblajVortoj
            |> List.ofSeq
            |> List.rev

         let legitajNombroj =
            konteksto.LegitajNombroj |> List.ofSeq |> List.rev

         this.LegiModifanton konteksto
         |> Result.bind (fun (modifanto, sekva) ->
               match modifanto with
               | Nil -> Ok()
               | _ ->
                  let enVortaro =
                     validajModifantoj.[sekva.OriginalaVorto.Vorto]

                  if enVortaro.ModifeblajVorttipoj.Contains(Cifero) then
                     legitajNombroj
                     |> List.tryHead
                     |> Option.map (fun nombro ->
                           match nombro with
                           | ArgumentaNombro nombro -> nombro.Operacioj.Enqueue(modifanto)
                           | _ -> ())
                     |> Option.map Ok
                     |> Option.defaultValue (Error(Eraro(sekva.OriginalaVorto, "No number to attach to")))
                  else
                     this.TroviModifeblanVortoPor enVortaro legitajVortoj
                     |> Option.map (fun modifotaVorto -> modifotaVorto.Modifantoj.Add(modifanto) |> ignore)
                     |> Option.map Ok
                     |> Option.defaultValue (Error(Eraro(sekva.OriginalaVorto, "no word to modify"))))

      member private this.LegiAntaŭanModifanton konteksto: Result<unit, Eraro> =
         this.LegiModifanton konteksto
         |> Result.map (fun (modifanto, vorto) ->
               konteksto.AtendantajModifantoj.AddLast((modifanto, vorto))
               |> ignore)

      member private this.LegiModifanton konteksto: Result<(Modifanto * MalinflektitaVorto), Eraro> =
         let sekva = enira.Dequeue()

         validajModifantoj.TryFind
            (if sekva.BazaVorto.EndsWith("l") then
               sekva.BazaVorto
             else
                sekva.BazaVorto.Substring(0, sekva.BazaVorto.Length - 1)
                + "l")
         |> Option.map (fun legitaModifanto ->
               match legitaModifanto.ModifantoInflekcioj with
               | [ Predikato ] ->
                  rezulto {
                     let! frazo = this.LegiLokalanFrazon konteksto
                     return ModifantoKunFrazo(sekva, frazo)
                  }
               | _ when legitaModifanto.PlenaVorto = "nil" ->
                  let lastaVorto = konteksto.LastaModifeblaVorto.Last.Value

                  match lastaVorto with
                  | ModifeblaArgumento _ ->
                     konteksto.LastaModifeblaArgumento.RemoveLast()
                     konteksto.LegitajModifeblajVortoj.RemoveLast()
                  | ModifeblaVerbo _ ->
                     konteksto.LegitajModifeblajVortoj.RemoveLast()
                     konteksto.LastaModifeblaVerbo.RemoveLast()
                  | ModifeblaNombro _ -> konteksto.LegitajNombroj.RemoveLast()

                  konteksto.LastaModifeblaVorto.RemoveLast()
                  Ok(Nil)
               | _ ->
                  this.LegiModifantajnArgumentojnPor legitaModifanto konteksto
                  |> Result.map (fun argumentojn ->
                        if legitaModifanto.ModifeblajVorttipoj = Set.ofList [ Cifero ]
                        then Operaciilo(sekva, argumentojn)
                        else ModifantoKunArgumentoj(sekva, argumentojn)))
         |> Option.map (Result.map (fun modifanto -> (modifanto, sekva)))
         |> Option.defaultValue (Error(Eraro(sekva.OriginalaVorto, "Unrecognized modifier")))

      member private this.TroviModifeblanVortoPor (modifanto: ModifantoEnVortaro) vortoj =
         vortoj |> Seq.tryFind (ĉuPovasModifi modifanto)

      member private this.LegiModifantajnArgumentojnPor modifanto konteksto =
         modifanto.ModifantoInflekcioj
         |> List.fold (fun ak sek ->
               match ak with
               | Ok listo ->
                  this.LegiArgumenton konteksto false
                  |> Result.bind (fun argumento ->
                        if ĉuHavasInflekcion argumento sek then
                           Ok(argumento :: listo)
                        else
                           Error
                              (Eraro
                                 (this.OriginalaVortoDe argumento,
                                  sprintf "does not have the expected inflection %O" sek)))
               | Error _ -> ak) (Ok [])

      member private this.OriginalaVortoDe argumento =
         match argumento with
         | ArgumentaVorto a -> a.Kapo.OriginalaVorto
         | ArgumentaNombro n -> failwith "TODO"

   let legiImperative (eniro: string) =
      prepariEniron eniro false
      |> Result.bind (fun vortoj -> ImperativaLegilo(Queue(vortoj)).Legi())

   let proveLegiNombron (eniro: string) =
      legiImperative eniro
      |> Result.bind (fun rezulto ->
            if rezulto.Argumentoj.Length > 1 then
               Error(Eraro(malplenaEniraVorto, "More than one number given"))
            else
               List.tryHead rezulto.Argumentoj
               |> Option.map (fun argumento ->
                     match argumento with
                     | ArgumentaNombro (nombro) -> Ok nombro
                     | _ -> Error(Eraro(malplenaEniraVorto, "No number given")))
               |> Option.defaultValue (Error(Eraro(malplenaEniraVorto, "No number given"))))

   let rec kalkuli eniraArgumento: Result<double, Eraro> =
      match eniraArgumento with
      | ArgumentaNombro nombro ->
         match nombro.Operacioj.Count with
         | 0 -> Ok(Decimal.ToDouble(nombro.Valuo))
         | _ ->
            nombro.Operacioj
            |> Seq.fold (fun ak sek ->
                  ak
                  |> Result.bind (fun ak ->
                        match sek with
                        | Operaciilo (o, nombroj) ->
                           match o.BazaVorto with
                           | _ when binarajOperaciioj.ContainsKey(o.BazaVorto) ->
                              let operaciilo = binarajOperaciioj.[o.BazaVorto]

                              List.head nombroj
                              |> kalkuli
                              |> Result.map (operaciilo ak)
                           | _ -> Error(Eraro(o.OriginalaVorto, "Unsupported operation"))
                        | _ -> Error(Eraro(malplenaEniraVorto, sprintf "Not an operator: %O" sek))))
                  (Ok(Decimal.ToDouble(nombro.Valuo)))

      | _ -> Error(Eraro(malplenaEniraVorto, sprintf "Not a math expression: %O" eniraArgumento))
