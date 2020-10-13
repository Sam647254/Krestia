namespace KrestiaVortilo

open System
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
            aliaVorto.Kapo = this.Kapo
            && aliaVorto.Modifantoj.SetEquals(this.Modifantoj)
         | _ -> false

      override this.GetHashCode() = hash this.Kapo

      override this.ToString() =
         sprintf "%O<%O>" this.Kapo (List.ofSeq this.Modifantoj)

   and Argumento =
      | ArgumentaVorto of ModifeblaVorto
      | Nombro of decimal

      override this.ToString() =
         match this with
         | ArgumentaVorto av -> av.ToString()
         | Nombro n -> n.ToString()

   and Verbo =
      { Vorto: ModifeblaVorto }

      override this.ToString() = this.Vorto.ToString()

   and Modifanto =
      | Pridiranto of Argumento
      | EcoDe of Argumento
      | Mine of Predikato
      | Ene of Predikato
      | Keni of Argumento * Argumento
      | Pini of Argumento * Argumento * Argumento
      | ModifantoKunArgumentoj of MalinflektitaVorto * Argumento list
      | ModifantoKunFrazo of MalinflektitaVorto * Predikato

      override this.ToString() =
         match this with
         | Pridiranto a -> sprintf "Pridiranto(%s)" (a.ToString())
         | EcoDe a -> sprintf "EcoDe(%s)" (a.ToString())
         | ModifantoKunArgumentoj(m, a) -> sprintf "%s(%O)" m.BazaVorto a
         | ModifantoKunFrazo(m, p) -> sprintf "%s(%O)" m.BazaVorto p
         | Ene p
         | Mine p -> sprintf "(%O)" p
         | Keni(a1, a2) -> sprintf "(%O, %O)" a1 a2
         | Pini(a1, a2, a3) -> sprintf "(%O, %O, %O)" a1 a2 a3

   and Predikato =
      { Kapo: Verbo
        Argumentoj: Argumento list }

      override this.ToString() =
         sprintf "%O(%O)" this.Kapo this.Argumentoj

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

   let modifantoj1DeKlasoj =
      [ "mel"; "sonol" ] |> Set.ofList

   let modifantojDePredikatoKunFrazo =
      [ "nomil"; "kerel" ] |> Set.ofList
   
   let modifantojDeVerboj =
      [ "nivoral"; "sivil" ] |> Set.ofList
      
   let modifantojDeKlasoj =
      [ "borol" ] |> Set.ofList

   let modifantojDeVortoKunArgumento =
      [ "sonol"
        "mel"
        "nel"
        "vol"
        "ponel" ]
      |> Set.ofList

   let senmodifantaVorto vorto =
      { Kapo = vorto
        Modifantoj = HashSet<Modifanto>() }

   let modifeblaVorto vorto (modifantoj: Modifanto list) =
      { Kapo = vorto
        Modifantoj = HashSet<Modifanto>(modifantoj) }
  
   let modifanto vorto = ModifantoKunArgumentoj (vorto, [])

   let plenaArgumento vorto = failwith "forigi"

   let plenaVerbo vorto = failwith "forigi"

   let argumento vorto (modifantoj: Modifanto seq): Argumento =
      { Kapo = vorto
        Modifantoj = HashSet(modifantoj) }
      |> ArgumentaVorto

   let pridiranto vorto = Pridiranto(argumento vorto [])

   let verbo vorto (modifantoj: Modifanto list): Verbo =
      { Vorto =
           { Kapo = vorto
             Modifantoj = HashSet(modifantoj) } }

   let valencoDe (vorto: MalinflektitaVorto) =
      vorto.InflekcioŜtupoj
      |> List.fold (fun ak sek ->
            match sek with
            | Nebazo (_, inflekcio, _) ->
               match inflekcio with
               | PartaUjo1
               | PartaUjo2
               | PartaUjo3
               | Imperativo
               | Hortativo -> ak - 1
               | PredikativoEsti -> ak + 1
               | _ -> ak
            | Bazo (vorttipo, inflekcio, _) ->
               match vorttipo with
               | MalplenaVerbo -> ak
               | NetransitivaVerbo
               | OblikaNetransitivaVerbo
               | NedirektaNetransitivaVerbo -> ak + 1
               | TransitivaVerbo
               | OblikaTransitivaVerbo
               | NedirektaTransitivaVerbo -> ak + 2
               | DutransitivaVerbo -> ak + 3
               | NombrigeblaKlaso
               | NenombrigeblaKlaso
               | AntaŭNombrigeblaEco
               | AntaŭNenombrigeblaEco
               | MalantaŭNombrigeblaEco
               | MalantaŭNenombrigeblaEco ->
                  match inflekcio with
                  | PredikativoEsti -> ak + 1
                  | _ -> ak
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

   let aldoniModifantonAlLastaVerbo sintaksanalizilo modifanto: Result<Sintaksanalizilo, Eraro> = failwith "forigi"

   let forigiRepetajnVortojn (vortoj: EniraVorto list): EniraVorto list =
      vortoj
      |> List.fold<EniraVorto, EniraVorto list> (fun ak sek ->
            if List.isEmpty ak
               || sek.Vorto
               <> (List.head ak |> (fun v -> v.Vorto))
               || ĉuCifero sek.Vorto then
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

   let kategorigi sintaksanalizilo vortoj: Result<Sintaksanalizilo, Eraro> = failwith "forigi"

   let rec legiFrazojn (sintaksanalizilo: Sintaksanalizilo) (rezulto: AnaziloRezulto): Result<AnaziloRezulto, Eraro> =
      failwith "forigi"

   let iĝiEnEnirajVortoj ĉuTesto (eniro: string) =
      eniro.Split([ "\r\n"; "\n" ] |> Array.ofList, StringSplitOptions.None)
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

   let proviLegiArgumentajnModifantojn (restantajVortoj: MalinflektitaVorto list) = failwith "???"

   let proviLegiPridirantajnModifantojn (restantajVortoj: MalinflektitaVorto list) = [], restantajVortoj

   let proviLegiAntaŭanModifanton (analizejo: Analizejo) = failwith "forigi"

   let rec legiSekvanArgumenton (analizejo: Analizejo): Result<Argumento * Analizejo, Eraro> = failwith "???"

   let legiArgumenton (analizejo: Analizejo): Result<Analizejo, Eraro> =
      legiSekvanArgumenton analizejo
      |> Result.map (fun (argumento, restanta) ->
            { restanta with
                 Argumentoj = restanta.Argumentoj.Conj(argumento) })

   let legiSekvanSolanKlason (analizejo: Analizejo): Result<Predikato * Analizejo, Eraro> = failwith "???"

   let legiSolanKlason (analizejo: Analizejo): Result<Analizejo, Eraro> =
      legiSekvanSolanKlason analizejo
      |> Result.map (fun (klaso, restanta) ->
            { restanta with
                 Frazoj = klaso :: restanta.Frazoj })

   // TODO: Bezonas purigadon
   let rec legiAntaŭeModifitanVortonAk (analizejo: Analizejo) = failwith "forigi"

   let legiArgumentojnPor verbo restanta = [], restanta

   let prepariEniron (eniro: string) ĉuTesto =
      eniro
      |> iĝiEnEnirajVortoj ĉuTesto
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
