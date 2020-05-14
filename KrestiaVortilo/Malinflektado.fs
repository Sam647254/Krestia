namespace KrestiaVortilo

open System
open Vorttipo
open Sintaksanalizilo

module Malinflektado =
   type Finaĵo =
      | InfinitivoFinaĵo of string * Inflekcio
      | DifinitoFinaĵo of string * Inflekcio
      | DUPFinaĵo of string * Definito: Inflekcio * UnuNombro: Inflekcio * PluraNombro: Inflekcio

   type ĈuAkceptiNenombrigeblan =
      | AkceptiNenombrigeblan
      | NeAkceptiNenombrigeblan

   type MalinflektitaVorto =
      { BazaVorto: string
        InflekcioŜtupoj: MalinflektaŜtupo list }

   let okazoFinaĵoj =
      [ InfinitivoFinaĵo("lo", Okazo)
        InfinitivoFinaĵo("laa", AktualaOkazo)
        InfinitivoFinaĵo("lu", FinitaOkazo) ]

   let nombrigeblaKlasoInflekcioj =
      [ DUPFinaĵo("", Difinito, UnuNombro, PluraNombro)
        DUPFinaĵo("nsa", Havaĵo, UnuHavaĵo, PluraHavaĵo)
        InfinitivoFinaĵo("wa", PredikativoEsti)
        InfinitivoFinaĵo("ga", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("va", AtributivoEstiAntaŭ)
        DUPFinaĵo("ra", Sola, UnuSola, PluraSola)
        DUPFinaĵo("rem", Havado, UnuHavado, PluraHavado)
        DUPFinaĵo("reg", Havado, UnuHavado, PluraHavado)
        DUPFinaĵo("rim", Ekzistado, UnuEkzistado, PluraEkzistado)
        DifinitoFinaĵo("lam", Translativo)
        DifinitoFinaĵo("las", Translativo)
        DifinitoFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("va", SpecifaĜerundo) ]

   let nenombrigeblaKlasoInflekcioj =
      [ DifinitoFinaĵo("", Difinito)
        DifinitoFinaĵo("nsa", Havaĵo)
        InfinitivoFinaĵo("wa", PredikativoEsti)
        InfinitivoFinaĵo("ga", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("va", AtributivoEstiAntaŭ)
        DifinitoFinaĵo("ra", Sola)
        DifinitoFinaĵo("rem", Havado)
        DifinitoFinaĵo("reg", Havado)
        DifinitoFinaĵo("rim", Ekzistado)
        DifinitoFinaĵo("lam", Translativo)
        DifinitoFinaĵo("las", Translativo)
        DifinitoFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("va", SpecifaĜerundo) ]

   let malplenaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("io", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("elim", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo) ]
      @ okazoFinaĵoj

   let netransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("e", Progresivo)
        InfinitivoFinaĵo("o", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", Ujo1Volo)
        InfinitivoFinaĵo("ie", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("ia", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Imperativo)
        InfinitivoFinaĵo("a", Invito)
        InfinitivoFinaĵo("etio", Argumento1)
        InfinitivoFinaĵo("elis", Translativo)
        InfinitivoFinaĵo("mea", Ĝerundo)
        InfinitivoFinaĵo("em", PartaUjo1) ]
      @ okazoFinaĵoj

   let transitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("re", Progresivo)
        InfinitivoFinaĵo("ro", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", Ujo1Volo)
        InfinitivoFinaĵo("ore", Ujo2Volo)
        InfinitivoFinaĵo("rie", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("ria", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ri", Imperativo)
        InfinitivoFinaĵo("ia", Invito)
        InfinitivoFinaĵo("etio", Argumento1)
        InfinitivoFinaĵo("oniaa", Argumento2)
        InfinitivoFinaĵo("elit", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("im", PartaUjo1)
        InfinitivoFinaĵo("ig", PartaUjo1)
        InfinitivoFinaĵo("em", PartaUjo2)
        InfinitivoFinaĵo("es", PartaUjo2)
        InfinitivoFinaĵo("rim", Reflekcio)
        InfinitivoFinaĵo("ris", Reflekcio)
        InfinitivoFinaĵo("res", UnueUjo2) ]
      @ okazoFinaĵoj

   let dutransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("re", Progresivo)
        InfinitivoFinaĵo("ro", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", Ujo1Volo)
        InfinitivoFinaĵo("ore", Ujo2Volo)
        InfinitivoFinaĵo("eri", Ujo3Volo)
        InfinitivoFinaĵo("rie", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("ria", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ri", Imperativo)
        InfinitivoFinaĵo("ia", Invito)
        InfinitivoFinaĵo("etio", Argumento1)
        InfinitivoFinaĵo("oniaa", Argumento2)
        InfinitivoFinaĵo("elip", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("eg", PartaUjo1)
        InfinitivoFinaĵo("en", PartaUjo1)
        InfinitivoFinaĵo("em", PartaUjo1)
        InfinitivoFinaĵo("ev", PartaUjo1)
        InfinitivoFinaĵo("os", PartaUjo2)
        InfinitivoFinaĵo("on", PartaUjo2)
        InfinitivoFinaĵo("om", PartaUjo2)
        InfinitivoFinaĵo("osh", PartaUjo2)
        InfinitivoFinaĵo("us", PartaUjo3)
        InfinitivoFinaĵo("ug", PartaUjo3)
        InfinitivoFinaĵo("um", PartaUjo3)
        InfinitivoFinaĵo("ut", PartaUjo3)
        InfinitivoFinaĵo("im", Reflekcio)
        InfinitivoFinaĵo("ish", Reflekcio)
        InfinitivoFinaĵo("rosh", UnueUjo2)
        InfinitivoFinaĵo("rut", UnueUjo3) ]
      @ okazoFinaĵoj

   let nedirektaTransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("e", Progresivo)
        InfinitivoFinaĵo("o", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", Ujo1Volo)
        InfinitivoFinaĵo("eri", Ujo3Volo)
        InfinitivoFinaĵo("ie", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("ia", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Imperativo)
        InfinitivoFinaĵo("a", Invito)
        InfinitivoFinaĵo("etio", Argumento1)
        InfinitivoFinaĵo("elish", Translativo)
        InfinitivoFinaĵo("mea", Ĝerundo)
        InfinitivoFinaĵo("am", PartaUjo1)
        InfinitivoFinaĵo("an", PartaUjo1)
        InfinitivoFinaĵo("om", PartaUjo3)
        InfinitivoFinaĵo("os", PartaUjo3)
        InfinitivoFinaĵo("ros", UnueUjo3)
        InfinitivoFinaĵo("es", Reflekcio) ]
      @ okazoFinaĵoj

   let oblikaNetransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("e", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ore", Ujo2Volo)
        InfinitivoFinaĵo("ra", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("re", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("oniaa", Argumento2)
        InfinitivoFinaĵo("am", PartaUjo2) ]
      @ okazoFinaĵoj

   let oblikaTransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("i", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ore", Ujo2Volo)
        InfinitivoFinaĵo("eri", Ujo3Volo)
        InfinitivoFinaĵo("ri", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("re", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("oniaa", Argumento2)
        InfinitivoFinaĵo("eru", Argumento3)
        InfinitivoFinaĵo("om", PartaUjo2)
        InfinitivoFinaĵo("on", PartaUjo2)
        InfinitivoFinaĵo("im", PartaUjo3)
        InfinitivoFinaĵo("ig", PartaUjo3)
        InfinitivoFinaĵo("rig", UnueUjo3)
        InfinitivoFinaĵo("eg", Reflekcio) ]
      @ okazoFinaĵoj

   let nedirektaNetransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("io", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("eri", Ujo3Volo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("oniaa", Argumento2)
        InfinitivoFinaĵo("om", PartaUjo3) ]
      @ okazoFinaĵoj

   let pridirantoInflekcioj =
      [ InfinitivoFinaĵo("e", Difinito)
        InfinitivoFinaĵo("a", UnuNombro)
        InfinitivoFinaĵo("ie", PluraNombro)
        InfinitivoFinaĵo("u", Havaĵo)
        InfinitivoFinaĵo("ia", PredikativoEsti)
        InfinitivoFinaĵo("ea", AtributivoEstiMalantaŭ)
        InfinitivoFinaĵo("ra", AtributivoEstiAntaŭ)
        InfinitivoFinaĵo("io", Sola)
        InfinitivoFinaĵo("im", Translativo)
        InfinitivoFinaĵo("is", Translativo)
        InfinitivoFinaĵo("i", Ĝerundo)
        InfinitivoFinaĵo("em", Igo)
        InfinitivoFinaĵo("eg", Igo)
        InfinitivoFinaĵo("es", Igo)
        InfinitivoFinaĵo("et", Igo)
        InfinitivoFinaĵo("od", Etigo) ]

   let inflekciojPerVorttipoj =
      [ NombrigeblaKlaso, nombrigeblaKlasoInflekcioj
        NenombrigeblaKlaso, nenombrigeblaKlasoInflekcioj
        AntaŭNombrigeblaEco, nombrigeblaKlasoInflekcioj
        AntaŭNenombrigeblaEco, nenombrigeblaKlasoInflekcioj
        MalantaŭNombrigeblaEco, nombrigeblaKlasoInflekcioj
        MalantaŭNenombrieblaEco, nenombrigeblaKlasoInflekcioj
        MalplenaVerbo, malplenaVerboInflekcioj
        NetransitivaVerbo, netransitivaVerboInflekcioj
        TransitivaVerbo, transitivaVerboInflekcioj
        NedirektaTransitivaVerbo, nedirektaTransitivaVerboInflekcioj
        DutransitivaVerbo, dutransitivaVerboInflekcioj
        OblikaNetransitivaVerbo, oblikaNetransitivaVerboInflekcioj
        OblikaTransitivaVerbo, oblikaTransitivaVerboInflekcioj
        NedirektaNetransitivaVerbo, nedirektaNetransitivaVerboInflekcioj
        Pridiranto, pridirantoInflekcioj ]
      |> Map.ofList

   let ĉiujInflekcioj =
      inflekciojPerVorttipoj
      |> Map.toList
      |> List.map (fun (vorttipo, finaĵoj) -> finaĵoj |> List.map (fun finaĵo -> (vorttipo, finaĵo)))
      |> List.concat

   let nombrigeblaDifinitoFinaĵoj =
      nombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> (finaĵo, NombrigeblaKlaso))
   let difinitoFinaĵoj =
      nombrigeblaDifinitoFinaĵoj
      @ (nenombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> (finaĵo, NenombrigeblaKlaso)))

   let difinitivoAlInfinitivoTabelo =
      [ ('a', "aa")
        ('e', "o")
        ('i', "u") ]
      |> Map.ofList

   let vokaloj = [ 'i'; 'e'; 'a'; 'u'; 'o'; 'ɒ' ] |> Set.ofList
   let ĉuVokalo litero = Set.contains litero vokaloj

   type Litero =
      | Konsonanto of char
      | Vokalo of char

   let difinitoAlInfinitivo (vorto: string) =
      vorto.Substring(0, vorto.Length - 1) + difinitivoAlInfinitivoTabelo.[vorto.Chars(vorto.Length - 1)]

   let infinitivoAlDifinito (vorto: string) =
      difinitivoAlInfinitivoTabelo
      |> Map.pick (fun difinitoFinaĵo infinitivoFinaĵo ->
            if vorto.EndsWith(infinitivoFinaĵo)
            then (vorto.Substring(0, vorto.Length - infinitivoFinaĵo.Length) + difinitoFinaĵo.ToString()) |> Some
            else None)

   let ĉuDifinito (vorto: string) akceptiNenombrigeblan =
      match akceptiNenombrigeblan with
      | AkceptiNenombrigeblan -> difinitoFinaĵoj
      | NeAkceptiNenombrigeblan -> nombrigeblaDifinitoFinaĵoj
      |> List.tryPick (fun (finaĵo, vorttipo) ->
            if vorto.EndsWith(finaĵo) && vorto.Length > finaĵo.Length
            then Some vorttipo
            else None)

   let malinflektiSeDifinito (vorto: string) inflekcio akceptiNenombrigeblan =
      ĉuDifinito vorto akceptiNenombrigeblan
      |> Option.map (fun vorttipo -> Nebazo(vorttipo, inflekcio, difinitoAlInfinitivo vorto))

   let unuNombroFinaĵo = "si"
   let pluraNombroFinaĵo = "ve"

   let rec malinflekti (ĉeno: string): Result<MalinflektaŜtupo, string> =
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo, ĉeno) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo, ĉeno) |> Ok
      | _ ->
         ĉiujInflekcioj
         |> List.tryPick (fun (vorttipo, finaĵo) ->
               match finaĵo with
               | InfinitivoFinaĵo (finaĵo, inflekcio) ->
                  infinitivoFinaĵoj
                  |> Map.tryPick (fun infinitivoFinaĵo infinitivoTipo ->
                        if ĉeno.EndsWith(infinitivoFinaĵo + finaĵo) && infinitivoTipo = vorttipo
                        then Some()
                        else None)
                  |> Option.bind (fun _ ->
                        Some(ĉeno.Substring(0, ĉeno.Length - finaĵo.Length))
                        |> Option.filter (fun subĉeno ->
                              match dividi subĉeno true with
                              | Ok (_) -> true
                              | Error (_) -> false)
                        |> Option.map (fun malinflektita -> Nebazo(vorttipo, inflekcio, malinflektita)))
               | DifinitoFinaĵo (finaĵo, inflekcio) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let difinito = ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)
                     malinflektiSeDifinito difinito inflekcio AkceptiNenombrigeblan
                  else
                     None
               | DUPFinaĵo (finaĵo, difinito, unuNombro, pluraNombro) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let restantaj = ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)
                     if restantaj.EndsWith(unuNombroFinaĵo) then
                        let difinito = ĉeno.Substring(0, restantaj.Length - unuNombroFinaĵo.Length)
                        malinflektiSeDifinito difinito unuNombro NeAkceptiNenombrigeblan
                     elif restantaj.EndsWith(pluraNombroFinaĵo) then
                        let difinito = ĉeno.Substring(0, restantaj.Length - pluraNombroFinaĵo.Length)
                        malinflektiSeDifinito difinito pluraNombro NeAkceptiNenombrigeblan
                     else
                        malinflektiSeDifinito restantaj difinito AkceptiNenombrigeblan
                  else
                     None)
         |> Option.orElseWith (fun () ->
               ĉuInfinitivo ĉeno |> Option.map (fun vorttipo -> Bazo(vorttipo, Infinitivo, ĉeno)))
         |> Option.map Ok
         |> Option.defaultValue (Error(sprintf "%s estas nevalida" ĉeno))

   and tuteMalinflekti (ĉeno: string) =
      malinflekti ĉeno
      |> Result.bind (fun malinflektita ->
            match malinflektita with
            | Bazo (_, _, bazaVorto) ->
               { BazaVorto = bazaVorto
                 InflekcioŜtupoj = List.singleton malinflektita }
               |> Ok
            | Nebazo (_, _, restanta) ->
               tuteMalinflekti restanta
               |> Result.map (fun sekva -> { sekva with InflekcioŜtupoj = malinflektita :: sekva.InflekcioŜtupoj }))

   and tuteMalinflektiĈiujn (ĉenoj: string list) =
      ĉenoj
      |> List.fold (fun akListo sekva ->
            match akListo with
            | Ok (listo) ->
               match tuteMalinflekti sekva with
               | Ok (malinflektitaVorto) -> Ok(malinflektitaVorto :: listo)
               | Error (eraro) -> Error eraro
            | Error (_) -> akListo) (Ok [])
      |> Result.map List.rev

   and ĉuVerbo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun malinflektitaVorto ->
            match malinflektitaVorto.InflekcioŜtupoj with
            | unua :: _ ->
               match unua with
               | Bazo (vorttipo, _, _) ->
                  verboTipoj |> Set.contains vorttipo
               | Nebazo (_, _, _) -> failwith "ne valida unua ŝtupo"
            | [] -> failwith "ne valida ŝtupoj")

   and ĉuVerboInfinitivo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun malinflektitaVorto ->
            match malinflektitaVorto.InflekcioŜtupoj with
            | [ solaŜtupo ] ->
               match solaŜtupo with
               | Bazo (vorttipo, inflekcio, _) ->
                  (inflekcio = Infinitivo) && verboTipoj |> Set.contains vorttipo
               | _ -> false
            | _ -> false)

   and ĉuInfinitivoB (ĉeno: string) =
      ĉuInfinitivo ĉeno |> Option.isSome

   and ĉuVerboInfinitivoB (ĉeno: string) =
      match ĉuVerboInfinitivo ĉeno with
      | Ok (rezulto) -> rezulto
      | Error (_) -> false

   and bazoDe (vorto: string) =
      if ĉuVerboInfinitivoB vorto then
         vorto.Substring
            (0,
             vorto.Length - (if vorto.EndsWith("sh") then 2 else 1))
      else
         vorto

   and bazoPorDividi (infinitivo: string) =
      [ "gru"; "gro"; "dru"; "dro"; "li"; "lu"; "d"; "l"; "r" ]
      |> List.tryPick (fun finaĵo ->
            if infinitivo.EndsWith(finaĵo)
            then Some(infinitivo.Substring(0, infinitivo.Length - finaĵo.Length))
            else None)
      |> Option.defaultValue infinitivo
      |> bazoDe
      
   and normaligiEnVortaranFormon (infinitivo: string) =
      if Char.IsUpper(infinitivo.[0])
      then infinitivo
      else
         [ "gru", "gro"
           "dru", "dro"
           "r", "l" ]
         |> List.tryPick (fun (alterativa, vortara) ->
            if infinitivo.EndsWith(alterativa)
            then infinitivo.Substring(0, infinitivo.Length - alterativa.Length) + vortara |> Some
            else None)
         |> Option.defaultValue infinitivo
         |> bazoDe

   and ĉuMalplenigita (malplenigita: Vorttipo) (originala: string) =
      ĉuInfinitivo originala
      |> Option.filter (fun originalaTipo -> Set.contains originalaTipo verboTipoj)
      |> Option.bind (fun originalaTipo -> Map.tryFind originalaTipo malplenigeblaVerboTipoj)
      |> Option.map (fun malplenigeblaTipoj -> Set.contains malplenigita malplenigeblaTipoj)
      |> Option.defaultValue false

   and malplenigitajFormojDe (ĉeno: string) =
      ĉuInfinitivo ĉeno
      |> Option.filter (fun tipo -> Set.contains tipo verboTipoj)
      |> Option.bind (fun verboTipo -> Map.tryFind verboTipo malplenigeblaVerboTipoj)
      |> Option.map (fun tipoj ->
            let bazo = bazoDe ĉeno
            tipoj
            |> Set.map (fun tipo -> Map.find tipo verboFinaĵoj)
            |> Set.map (fun finaĵo -> bazo + finaĵo))
      |> Option.map Ok
      |> Option.defaultValue ((sprintf "%s ne estas verbo" ĉeno) |> Error)

   and kategorigiLiterojn =
      List.map (fun litero ->
         if ĉuVokalo litero then Vokalo litero else Konsonanto litero)

   and dividi (ĉeno: string) (inkluziFinaĵon: bool) =
      let normaligita =
         ĉeno.Replace("aa", "ɒ").Replace("sh", "ʃ")
         |> if inkluziFinaĵon then (fun s -> s) else bazoPorDividi

      let rec dividiAk ĉuKomenca (literoj: Litero list): Result<string list, string> =
         match literoj with
         // CCVC
         | Konsonanto (k1) :: Konsonanto (k2) :: Vokalo (v) :: Konsonanto (kf) :: Konsonanto (kk2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Konsonanto(kk2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v; kf ]) :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto (k1); Konsonanto (k2); Vokalo (v); Konsonanto (kf) ] ->
            if ĉuKomenca
            then [ System.String.Concat([ k1; k2; v; kf ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CCV
         | Konsonanto (k1) :: Konsonanto (k2) :: Vokalo (v) :: Konsonanto (kf) :: Vokalo (v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Konsonanto(kf) :: Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | Konsonanto (k1) :: Konsonanto (k2) :: Vokalo (v) :: Vokalo (v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto (k1); Konsonanto (k2); Vokalo (v) ] ->
            if ĉuKomenca
            then [ System.String.Concat([ k1; k2; v ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CVC
         | Konsonanto (k1) :: Vokalo (v) :: Konsonanto (kf) :: Konsonanto (kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v; kf ]) :: restantajSilaboj)
         | [ Konsonanto (k1); Vokalo (v); Konsonanto (kf) ] -> [ System.String.Concat([ k1; v; kf ]) ] |> Ok
         // CV
         | Konsonanto (k1) :: Vokalo (v) :: Konsonanto (kk2) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v ]) :: restantajSilaboj)
         | Konsonanto (k1) :: Vokalo (v) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v ]) :: restantajSilaboj)
         | [ Konsonanto (k1); Vokalo (v) ] -> [ System.String.Concat([ k1; v ]) ] |> Ok
         // VC
         | Vokalo (v) :: Konsonanto (kf) :: Konsonanto (kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ v; kf ]) :: restantajSilaboj)
         | [ Vokalo (v); Konsonanto (kf) ] -> [ System.String.Concat([ v; kf ]) ] |> Ok
         // V
         | Vokalo (v) :: Konsonanto (kf) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Konsonanto(kf) :: Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
         | Vokalo (v) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
         | [ Vokalo (v) ] -> [ v.ToString() ] |> Ok
         // Eraro
         | [] -> Error "La vorto estas vida"
         | _ -> Error(sprintf "Ne povas dividi %A" literoj)

      dividiAk true
         (normaligita.ToCharArray()
          |> List.ofArray
          |> kategorigiLiterojn)

   and dividiKunFinaĵo (vorto: string) =
      dividi vorto false
      |> Result.bind (fun silaboj ->
            ĉuInfinitivo vorto
            |> Option.map (fun vorttipo -> silaboj @ [ vorttipo.ToString() ])
            |> Option.map Ok
            |> Option.defaultValue (Error(sprintf "%s ne estas infinitivo" vorto)))

   let predikatajBazajTipoj =
      [ NombrigeblaKlaso
        NenombrigeblaKlaso
        MalantaŭNombrigeblaEco
        AntaŭNombrigeblaEco
        MalantaŭNenombrieblaEco
        AntaŭNenombrigeblaEco ] |> Set.ofList

   let predikatajNebazajInflekcioj =
      [ Progresivo
        Perfekto
        Estonteco
        Ujo1Volo
        Ujo2Volo
        Ujo3Volo
        PredikativoEsti
        Imperativo
        Invito
        Havado
        UnuHavado
        PluraHavado
        Ekzistado
        UnuEkzistado
        PluraEkzistado
        Sola
        UnuSola
        PluraSola ] |> Set.ofList

   let argumentajBazajTipoj =
      [ FremdaVorto; Lokokupilo ] |> Set.ofList

   let argumentajNebazajInflekcioj =
      [ Difinito; UnuNombro; PluraNombro ] |> Set.ofList
      
   let malantaŭModifantajInflekcioj =
      [ AtributivoEstiMalantaŭ ] |> Set.ofList

   let ĉuPredikataVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (vorttipo, _, _) -> Set.contains vorttipo predikatajBazajTipoj
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio predikatajNebazajInflekcioj

   let ĉuArgumentaVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (vorttipo, _, _) -> Set.contains vorttipo argumentajBazajTipoj
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio argumentajNebazajInflekcioj
      
   let ĉuMalantaŭModifantaVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (_, _, _) -> false
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio malantaŭModifantajInflekcioj

   let ĉiujInflekciojDe vorto =
      ĉuInfinitivo vorto
      |> Option.bind (fun vorttipo -> Map.tryFind vorttipo inflekciojPerVorttipoj)
      |> Option.map (fun finaĵoj ->
            finaĵoj
            |> List.map (fun finaĵo ->
                  match finaĵo with
                  | InfinitivoFinaĵo (sufikso, inflekcio) -> (inflekcio, vorto + sufikso)
                  | DifinitoFinaĵo (sufikso, inflekcio) -> (inflekcio, (infinitivoAlDifinito vorto) + sufikso)
                  | DUPFinaĵo (sufikso, inflekcio, _, _) ->
                     let difinito = infinitivoAlDifinito vorto
                     (inflekcio,
                      (sprintf "%s, %s, %s" (difinito + sufikso) (difinito + unuNombroFinaĵo + sufikso)
                          (difinito + pluraNombroFinaĵo + sufikso))))
            |> Map.ofList)
      
   let valencoDeInfinitivo vorto =
      ĉuInfinitivo vorto
      |> Option.map (fun vorttipo ->
            match vorttipo with
            | NetransitivaVerbo
            | OblikaNetransitivaVerbo
            | NedirektaNetransitivaVerbo -> 1
            | TransitivaVerbo
            | NedirektaTransitivaVerbo
            | OblikaTransitivaVerbo -> 2
            | DutransitivaVerbo -> 3
            | _ -> 0)

   let valencoDe vorto =
      vorto.InflekcioŜtupoj
      |> List.tryFind (fun ŝtupo ->
         match ŝtupo with
         | Nebazo(_, inflekcio, _) -> inflekcio = Imperativo
         | _ -> false)
      |> Option.bind (fun _ ->
         valencoDeInfinitivo vorto.BazaVorto
         |> Option.map (fun valenco -> valenco - 1))
      |> Option.orElseWith (fun () -> valencoDeInfinitivo vorto.BazaVorto)

   let vortaraTipoDe vorto =
      match malinflekti vorto with
      | Ok (malinflektaŜtupo) ->
         match malinflektaŜtupo with
         | Bazo (vorttipo, _, _) ->
            match vorttipo with
            | NombrigeblaKlaso
            | NenombrigeblaKlaso -> "Class"
            | MalplenaVerbo
            | NetransitivaVerbo
            | TransitivaVerbo
            | NedirektaTransitivaVerbo
            | DutransitivaVerbo
            | OblikaTransitivaVerbo
            | OblikaNetransitivaVerbo
            | NedirektaNetransitivaVerbo -> "Verb"
            | Pridiranto -> "Descriptor"
            | AntaŭNenombrigeblaEco
            | MalantaŭNenombrieblaEco
            | AntaŭNombrigeblaEco
            | MalantaŭNombrigeblaEco -> "Associative class"
            | AntaŭRekordo | MalantaŭRekordo -> "Record"
            | MalantaŭModifanto
            | AntaŭModifanto -> "Modifier"
            | Lokokupilo -> "Placeholder"
            | FremdaVorto -> "Foreign word"
            | Makro -> "Macro"
         | Nebazo (_) -> failwith "Nevalida lasta malinflekta ŝtupo"
      | Error (eraro) -> failwith eraro
