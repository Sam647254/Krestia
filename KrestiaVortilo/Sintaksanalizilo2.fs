namespace KrestiaVortilo

open System.Collections.Generic
open FSharpx.Collections

open Vorttipo
open Malinflektado
open Sintaksanalizilo

module Sintaksanalizilo2 =
   [<CustomEquality; NoComparison>]
   type ModifeblaVorto =
      { Kapo: MalinflektitaVorto
        Modifantoj: HashSet<Modifanto> }
      
      override this.Equals(alia) =
         match alia with
         | :? ModifeblaVorto as aliaVorto ->
            aliaVorto.Kapo = this.Kapo && aliaVorto.Modifantoj.SetEquals(this.Modifantoj)
         | _ -> false
         
      override this.GetHashCode() =
         hash this.Kapo
   and Argumento = { Vorto: ModifeblaVorto }
      
   and Verbo = { Vorto: ModifeblaVorto }

   and AntaŭeModifitaVorto =
      | PredikataVorto of Verbo
      | ArgumentaVorto of Argumento

   and Modifanto =
      | Pridiranto of MalinflektitaVorto
      | EcoDe of Argumento
      | Mel of Argumento
      | Sonol of Argumento
      | Nival

   and PredikataVerboModifanto = | Nevil

   and Parvorto =
      | Vol
      | Del
      | Nal
      
   type Predikato =
      { Kapo: Verbo
        Argumentoj: Argumento list }

   type AtendantaPlurvorto =
      | AtendantaParvorto of argumento: Argumento * parvorto: Parvorto
      | AtendantaModifanto of Modifanto

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo>
        AtendantajFrazoj: (Argumento -> Argumento) list
        KonstruontajModifantoj: (EniraVorto * (Argumento -> Sintaksanalizilo -> Result<Sintaksanalizilo, Eraro>)) list
        LastaArgumento: Argumento option }

   type Analizejo =
      { Argumentoj: Deque<Argumento>
        Frazoj: Predikato list
        RestantajVortoj: MalinflektitaVorto list }

   type AnaziloRezulto =
      { Frazoj: Predikato list
        RestantajVortoj: Argumento list }

   let parvortoj =
      [ "vol", Vol; "del", Del; "nal", Nal ]
      |> Map.ofList

   let modifantoj1DeKlasoj =
      [ "mel", Mel; "sonol", Sonol ] |> Map.ofList

   let modifantojDePredikataVerboj = [ "nevil", Nevil ] |> Map.ofList

   let plenaArgumento vorto = failwith "forigi"

   let plenaVerbo vorto = failwith "forigi"
   
   let argumento vorto (modifantoj: Modifanto list): Argumento =
      { Vorto = { Kapo = vorto
                  Modifantoj = HashSet(modifantoj) } }
   
   let verbo vorto (modifantoj: Modifanto list): Verbo =
      { Vorto = { Kapo = vorto
                  Modifantoj = HashSet(modifantoj) } }
      
   let valencoDe (vorto: MalinflektitaVorto) =
      vorto.InflekcioŜtupoj
      |> List.fold (fun ak sek ->
         match sek with
         | Nebazo(_, inflekcio, _) ->
            match inflekcio with
            | PartaUjo1 | PartaUjo2 | PartaUjo3 -> ak - 1
            | _ -> ak
         | Bazo(vorttipo, inflekcio, _) ->
            match vorttipo with
            | MalplenaVerbo -> ak
            | NetransitivaVerbo -> ak + 1
            | TransitivaVerbo -> ak + 2
            | DutransitivaVerbo -> ak + 3
            | NombrigeblaKlaso
            | NenombrigeblaKlaso
            | AntaŭNombrigeblaEco
            | AntaŭNenombrigeblaEco
            | MalantaŭNombrigeblaEco
            | MalantaŭNenombrigeblaEco ->
                match inflekcio with
                | PredikativoEsti -> ak + 1
                | _ -> 0
            | _ -> 0) 0

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty
        AtendantajFrazoj = []
        LastaArgumento = None
        KonstruontajModifantoj = [] }

   let kreiRezulton = { Frazoj = []; RestantajVortoj = [] }

   let rec aldoniModifanton argumento modifanto = failwith "forigi"
   
   let lastaArgumentoDe sintaksanalizilo =
      sintaksanalizilo.LastaArgumento
      |> Option.map (fun a ->
            (a,
             { sintaksanalizilo with
                  LastaArgumento = None }))

   let purigiLastanArgumenton sintaksanalizilo =
      match sintaksanalizilo.LastaArgumento with
      | Some (lastaArgumento) ->
         if sintaksanalizilo.AtendantajFrazoj.IsEmpty then
            { sintaksanalizilo with
                 Argumentoj = sintaksanalizilo.Argumentoj.Conj(lastaArgumento)
                 LastaArgumento = None }
         else
            let modifitaArgumento =
               sintaksanalizilo.AtendantajFrazoj
               |> List.fold (fun ak sekva -> sekva ak) lastaArgumento

            { sintaksanalizilo with
                 Argumentoj = sintaksanalizilo.Argumentoj.Conj(modifitaArgumento)
                 AtendantajFrazoj = []
                 LastaArgumento = None }
      | None -> sintaksanalizilo

   let aldoniArgumenton sintaksanalizilo argumento =
      let purigita = purigiLastanArgumenton sintaksanalizilo
      { purigita with
           LastaArgumento = Some argumento }

   let anstataŭigiLastanArgumenton sintaksanalizilo argumento =
      { sintaksanalizilo with
           LastaArgumento = Some argumento }

   let aldoniModifantonAlLastaVerbo sintaksanalizilo modifanto: Result<Sintaksanalizilo, Eraro> =
      failwith "forigi"

   let forigiRepetajnVortojn (vortoj: EniraVorto list): EniraVorto list =
      vortoj
      |> List.fold<EniraVorto, EniraVorto list> (fun ak sek ->
            if List.isEmpty ak
               || sek.Vorto
               <> (List.head ak |> (fun v -> v.Vorto)) then
               sek :: ak
            else
               ak) []
      |> List.rev

   let finiKategorigadon (sintaksanalizilo: Sintaksanalizilo) =
      if sintaksanalizilo.KonstruontajModifantoj.Length > 0 then
         let (vorto, _) =
            sintaksanalizilo.KonstruontajModifantoj.Head

         Error(vorto, sprintf "%s needs additional helping words" vorto.Vorto)
      else
         Ok sintaksanalizilo

   let kategorigi sintaksanalizilo vortoj: Result<Sintaksanalizilo, Eraro> =
      failwith "forigi"

   let rec legiFrazojn (sintaksanalizilo: Sintaksanalizilo) (rezulto: AnaziloRezulto): Result<AnaziloRezulto, Eraro> =
      failwith "forigi"

   let iĝiEnEnirajVortoj ĉuTesto (eniro: string) =
      eniro.Split('\n')
      |> List.ofArray
      |> List.map (fun vico -> vico.Split(' '))
      |> List.map List.ofArray
      |> List.mapi (fun vico vortojDeVico ->
            vortojDeVico
            |> List.fold (fun (pozo, ak) sekva ->
                  (pozo + String.length sekva + 1,
                   { Vico = if ĉuTesto then 0 else vico
                     Pozo = if ĉuTesto then 0 else pozo
                     Vorto = sekva }
                   :: ak)) (0, [])
            |> fun (_, listo) -> listo
            |> List.rev)
      |> List.concat

   let analizi (eniro: string) ĉuTesto: Result<AnaziloRezulto, Eraro> =
      eniro
      |> iĝiEnEnirajVortoj ĉuTesto
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
      |> Result.bind (kategorigi kreiSintaksanalizilon)
      |> Result.bind (fun sintaksanalizilo ->
            let rezulto = kreiRezulton
            legiFrazojn sintaksanalizilo rezulto)

   let legiPridiranton (analizejo: Analizejo) = failwith "???"

   let proviLegiArgumentajnModifantojn (restantajVortoj: MalinflektitaVorto list) =
      failwith "???"

   let proviLegiPridirantajnModifantojn (restantajVortoj: MalinflektitaVorto list) = [], restantajVortoj

   let proviLegiAntaŭanModifanton (analizejo: Analizejo) =
      match analizejo.RestantajVortoj with
      | sekva :: restanta ->
         if ĉuAntaŭModifantaVorto sekva then
            (Pridiranto sekva,
             { analizejo with
                  RestantajVortoj = restanta })
            |> Some
         else
            None
      | [] -> None

   let rec legiSekvanArgumenton (analizejo: Analizejo): Result<Argumento * Analizejo, Eraro> =
      failwith "???"

   let legiArgumenton (analizejo: Analizejo): Result<Analizejo, Eraro> =
      legiSekvanArgumenton analizejo
      |> Result.map (fun (argumento, restanta) ->
            { restanta with
                 Argumentoj = restanta.Argumentoj.Conj(argumento) })

   let legiSekvanSolanKlason (analizejo: Analizejo): Result<Predikato * Analizejo, Eraro> =
      failwith "???"
   
   let legiSolanKlason (analizejo: Analizejo): Result<Analizejo, Eraro> =
      legiSekvanSolanKlason analizejo
      |> Result.map (fun (klaso, restanta) ->
         { restanta with Frazoj = klaso :: restanta.Frazoj })

   // TODO: Bezonas purigadon
   let rec legiAntaŭeModifitanVortonAk (analizejo: Analizejo): Result<AntaŭeModifitaVorto * Analizejo, Eraro> =
      failwith "forigi"
   
   let legiArgumentojnPor verbo restanta = [], restanta

   let legiAntaŭeModifitanVorton (analizejo: Analizejo): Result<Analizejo, Eraro> =
      legiAntaŭeModifitanVortonAk analizejo
      |> Result.map (fun (vorto, restanta) ->
            match vorto with
            | PredikataVorto (verbo) ->
               { restanta with
                    Frazoj = { Kapo = verbo; Argumentoj = [] } :: analizejo.Frazoj }
            | ArgumentaVorto (argumento) ->
               { restanta with
                    Argumentoj = analizejo.Argumentoj.Conj(argumento) })

   let rec legiSekvan (analizejo: Analizejo) =
      match analizejo.RestantajVortoj with
      | sekva :: _ ->
         if ĉuArgumentaVorto sekva then
            legiArgumenton analizejo
         elif ĉuSolaArgumento sekva then
            legiSolanKlason analizejo
         elif ĉuAntaŭModifantaVorto sekva then
            legiAntaŭeModifitanVorton analizejo
         else
            Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
            |> Error
         |> Result.bind (fun restanta -> legiSekvan restanta)
      | [] -> Ok analizejo

   let legiPerAnalizejo (vortoj: MalinflektitaVorto list) =
      let analizejo =
         { RestantajVortoj = vortoj
           Frazoj = []
           Argumentoj = Deque.empty }

      legiSekvan analizejo

   let prepariEniron (eniro: string) ĉuTesto =
      eniro
      |> iĝiEnEnirajVortoj ĉuTesto
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
