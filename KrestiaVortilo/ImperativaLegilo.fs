namespace KrestiaVortilo

open System
open System.Collections.Generic
open System.IO
open System.Net
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
      
   type ModifantoEnVortaro =
      { PlenaVorto : string
        ModifeblajVorttipoj : Set<Vorttipo>
        ModifantoInflekcioj : Inflekcio list }
  
   let private vortarajVorttipoj =
      [ 'N', NombrigeblaKlaso
        'n', NenombrigeblaKlaso
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
        'P', Predikato ]
      |> Map.ofList
   
   let alportiModifantojn =
      async {
         let peto = WebRequest.Create("https://raw.githubusercontent.com/Sam647254/Krestia/master/vortaro.kv")
         let! respondo = peto.AsyncGetResponse()
         return seq {
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
         if not(partoj.[0].EndsWith("l")) || partoj.[0].StartsWith("h") then
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
            |> Some
         )
   
   let validajModifantoj =
      alportiModifantojn
      |> Seq.map (fun m -> m.PlenaVorto, m)
      |> Map.ofSeq

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
            |> Result.bind (fun () -> this.LegiArgumenton konteksto)
         else
            Error(Eraro(sekva.OriginalaVorto, "Unexpected input"))

      member private this.LegiPlenanArgumenton sekva konteksto =
         let novaArgumento =
               let (argumento, vorto) = plenaModifitaArgumento sekva konteksto.AtendantajPridirantoj
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
                        | Nebazo (_, _, _) ->
                           Ok None
                        | Bazo (vorttipo, _, bazaVorto) ->
                           match vorttipo with
                           | AntaŭNombrigeblaEco
                           | AntaŭNenombrigeblaEco ->
                              if ĉuEcoHavaĵo vorto then
                                 Ok None
                              else
                                 let rezulto =
                                    this.LegiArgumenton konteksto
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
                                 | ModifeblaArgumento (_) ->
                                    konteksto.LastaModifeblaVorto.RemoveLast()
                                    |> ignore
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
                                 this.LegiArgumenton konteksto
                                 |> Result.bind (fun argumento1 ->
                                       this.LegiArgumenton konteksto
                                       |> Result.map (fun argumento2 ->
                                             Keni(argumento1, argumento2) |> Some))
                              elif bazaVorto = "pini" then
                                    let pini = senmodifantaVorto vorto
                                    let piniArgumento = ArgumentaVorto pini
                                    konteksto.LastaModifeblaArgumento.AddLast(piniArgumento)
                                    |> ignore
                                    konteksto.LastaModifeblaVorto.AddLast(ModifeblaArgumento piniArgumento)
                                    |> ignore
                                    konteksto.LegitajModifeblajVortoj.AddLast(pini)
                                    |> ignore
                                    this.LegiArgumenton konteksto
                                    |> Result.bind (fun argumento1 ->
                                          this.LegiArgumenton konteksto
                                          |> Result.bind (fun argumento2 ->
                                                this.LegiArgumenton konteksto
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

      member private this.LegiMalantaŭModifanton konteksto =
         let sekva = enira.Dequeue()
         validajModifantoj.TryFind sekva.BazaVorto
         |> Option.map (fun legitaModifanto ->
               match legitaModifanto.ModifantoInflekcioj with
               | [ Predikato ] ->
                  this.LegiLokalanFrazon konteksto
                  |> Result.map (fun frazo ->
                     let plenaModifanto = ModifantoKunFrazo(sekva, frazo)
                     failwith "TODO")
               | _ when modifantojDePredikatoKunFrazo.Contains(sekva.BazaVorto) ->
                  (if konteksto.LastaModifeblaVerbo.Count = 0 then
                     Error(Eraro(sekva.OriginalaVorto, "No verb to modify"))
                   else
                      let lastaVerbo = konteksto.LastaModifeblaVerbo.Last.Value
                      konteksto.LastaModifeblaVerbo.RemoveLast()
                      Ok(lastaVerbo))
                  |> Result.bind (fun lastaVerbo ->
                        this.LegiLokalanFrazon konteksto
                        |> Result.map (fun frazo ->
                              let plenaModifanto = ModifantoKunFrazo(sekva, frazo)
                              lastaVerbo.Vorto.Modifantoj.Add(plenaModifanto)
                              |> ignore))
               | _ when modifantojDeVerboj.Contains(sekva.BazaVorto) ->
                  konteksto.LastaModifeblaVerbo.Last.Value.Vorto.Modifantoj.Add(modifanto sekva)
                  |> ignore
                  |> Ok
               | _ when modifantojDeKlasoj.Contains(sekva.BazaVorto) ->
                  // TODO: Trovi ĉu estas klaso
                  this.AldoniModifantonAlArgumento (modifanto sekva) konteksto.LastaModifeblaArgumento.Last.Value
                  |> Ok
               | _ when legitaModifanto.PlenaVorto = "nil" ->
                  let lastaVorto = konteksto.LastaModifeblaVorto.Last.Value
                  match lastaVorto with
                  | ModifeblaArgumento _ -> konteksto.LastaModifeblaArgumento.RemoveLast()
                  | ModifeblaVerbo _ -> konteksto.LastaModifeblaVerbo.RemoveLast()
                  konteksto.LastaModifeblaVorto.RemoveLast()
                  konteksto.LegitajModifeblajVortoj.RemoveLast()
                  |> Ok
               | _ when modifantojDeVortoKunArgumento.Contains(sekva.BazaVorto) ->
                  let lastaVorto =
                     konteksto.LegitajModifeblajVortoj.Last.Value

                  this.LegiArgumenton konteksto
                  |> Result.map (fun argumento ->
                        let novaModifanto = Modifanto1(sekva, argumento)
                        lastaVorto.Modifantoj.Add(novaModifanto) |> ignore)
               | _ -> Error(Eraro(sekva.OriginalaVorto, "Unexpected input"))
            )
         |> Option.defaultValue (Error(Eraro(sekva.OriginalaVorto, "Unrecognized modifier")))

      member private this.LegitajVortoj konteksto =
         let mutable vorto = konteksto.LegitajModifeblajVortoj.Last
         seq {
            for _ in 0 .. konteksto.LegitajModifeblajVortoj.Count do
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
