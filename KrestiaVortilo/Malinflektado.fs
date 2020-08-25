namespace KrestiaVortilo

open System
open Vorttipo
open Sintaksanalizilo

module Malinflektado =
   type EniraVorto = { Vico: int; Pozo: int; Vorto: string }

   let testaVorto vorto = { Vico = 0; Pozo = 0; Vorto = vorto }

   type Eraro = EniraVorto * string

   type Finaĵo =
      | BazaFinaĵo of string * Inflekcio
      | DifinitoFinaĵo of string * Inflekcio
      | PredikativoEstiFinaĵo of string * Inflekcio
      | DUPFinaĵo of string * Definito: Inflekcio * UnuNombro: Inflekcio * PluraNombro: Inflekcio

   type ĈuAkceptiNenombrigeblan =
      | AkceptiNenombrigeblan
      | NeAkceptiNenombrigeblan

   type MalinflektitaVorto =
      { BazaVorto: string
        InflekcioŜtupoj: MalinflektaŜtupo list
        OriginalaVorto: EniraVorto }

   let okazoFinaĵoj =
      [ BazaFinaĵo("lo", Okazo)
        BazaFinaĵo("laa", AktualaOkazo)
        BazaFinaĵo("lu", FinitaOkazo) ]

   let nombrigeblaKlasoInflekcioj =
      [ DUPFinaĵo("nsa", Havaĵo, UnuHavaĵo, PluraHavaĵo)
        DUPFinaĵo("la", Fokuso, UnuFokuso, PluraFokuso)
        DifinitoFinaĵo("ga", AtributivoEstiMalantaŭ)
        DifinitoFinaĵo("va", AtributivoEstiAntaŭ)
        DUPFinaĵo("ra", Sola, UnuSola, PluraSola)
        DUPFinaĵo("res", Havado, UnuHavado, PluraHavado)
        DUPFinaĵo("rem", Havado, UnuHavado, PluraHavado)
        DUPFinaĵo("rim", Ekzistado, UnuEkzistado, PluraEkzistado)
        DifinitoFinaĵo("lam", Translativo)
        PredikativoEstiFinaĵo("las", Translativo)
        PredikativoEstiFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("vra", SpecifaĜerundo)
        PredikativoEstiFinaĵo("re", Kvalito)
        DUPFinaĵo("", Difinito, UnuNombro, PluraNombro)
        PredikativoEstiFinaĵo("", PredikativoEsti) ]

   let nenombrigeblaKlasoInflekcioj =
      [ DifinitoFinaĵo("nsa", Havaĵo)
        BazaFinaĵo("ga", AtributivoEstiMalantaŭ)
        BazaFinaĵo("va", AtributivoEstiAntaŭ)
        DifinitoFinaĵo("ra", Sola)
        DifinitoFinaĵo("rem", Havado)
        DifinitoFinaĵo("reg", Havado)
        DifinitoFinaĵo("rim", Ekzistado)
        DifinitoFinaĵo("lam", Translativo)
        PredikativoEstiFinaĵo("las", Translativo)
        PredikativoEstiFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("vra", SpecifaĜerundo)
        PredikativoEstiFinaĵo("re", Kvalito)
        DifinitoFinaĵo("", Difinito)
        PredikativoEstiFinaĵo("", PredikativoEsti) ]

   let malplenaVerboInflekcioj =
      [ BazaFinaĵo("ia", Progresivo)
        BazaFinaĵo("io", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("elim", Translativo)
        BazaFinaĵo("ea", Ĝerundo) ]
      @ okazoFinaĵoj

   let netransitivaVerboInflekcioj =
      [ BazaFinaĵo("e", Progresivo)
        BazaFinaĵo("o", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ora", Desiderativo)
        BazaFinaĵo("ie", AtributivoEstiMalantaŭ)
        BazaFinaĵo("ia", AtributivoEstiAntaŭ)
        BazaFinaĵo("ea", Imperativo)
        BazaFinaĵo("a", Invito)
        BazaFinaĵo("etio", Argumento1)
        BazaFinaĵo("elis", Translativo)
        BazaFinaĵo("mea", Ĝerundo)
        BazaFinaĵo("em", PartaUjo1) ]
      @ okazoFinaĵoj

   let transitivaVerboInflekcioj =
      [ BazaFinaĵo("re", Progresivo)
        BazaFinaĵo("ro", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ora", Desiderativo)
        BazaFinaĵo("ore", Ujo2Volo)
        BazaFinaĵo("rie", AtributivoEstiMalantaŭ)
        BazaFinaĵo("ria", AtributivoEstiAntaŭ)
        BazaFinaĵo("ri", Imperativo)
        BazaFinaĵo("ia", Invito)
        BazaFinaĵo("etio", Argumento1)
        BazaFinaĵo("oniaa", Argumento2)
        BazaFinaĵo("elit", Translativo)
        BazaFinaĵo("ea", Ĝerundo)
        BazaFinaĵo("im", PartaUjo1)
        BazaFinaĵo("ig", PartaUjo1)
        BazaFinaĵo("em", PartaUjo2)
        BazaFinaĵo("es", PartaUjo2)
        BazaFinaĵo("rim", Reflekcio)
        BazaFinaĵo("ris", Reflekcio)
        BazaFinaĵo("res", UnueUjo2) ]
      @ okazoFinaĵoj

   let dutransitivaVerboInflekcioj =
      [ BazaFinaĵo("re", Progresivo)
        BazaFinaĵo("ro", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ora", Desiderativo)
        BazaFinaĵo("ore", Ujo2Volo)
        BazaFinaĵo("eri", Ujo3Volo)
        BazaFinaĵo("rie", AtributivoEstiMalantaŭ)
        BazaFinaĵo("ria", AtributivoEstiAntaŭ)
        BazaFinaĵo("ri", Imperativo)
        BazaFinaĵo("ia", Invito)
        BazaFinaĵo("etio", Argumento1)
        BazaFinaĵo("oniaa", Argumento2)
        BazaFinaĵo("elip", Translativo)
        BazaFinaĵo("ea", Ĝerundo)
        BazaFinaĵo("eg", PartaUjo1)
        BazaFinaĵo("en", PartaUjo1)
        BazaFinaĵo("em", PartaUjo1)
        BazaFinaĵo("ev", PartaUjo1)
        BazaFinaĵo("os", PartaUjo2)
        BazaFinaĵo("on", PartaUjo2)
        BazaFinaĵo("om", PartaUjo2)
        BazaFinaĵo("osh", PartaUjo2)
        BazaFinaĵo("us", PartaUjo3)
        BazaFinaĵo("ug", PartaUjo3)
        BazaFinaĵo("um", PartaUjo3)
        BazaFinaĵo("ut", PartaUjo3)
        BazaFinaĵo("im", Reflekcio)
        BazaFinaĵo("ish", Reflekcio)
        BazaFinaĵo("rosh", UnueUjo2)
        BazaFinaĵo("rut", UnueUjo3) ]
      @ okazoFinaĵoj

   let nedirektaTransitivaVerboInflekcioj =
      [ BazaFinaĵo("e", Progresivo)
        BazaFinaĵo("o", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ora", Desiderativo)
        BazaFinaĵo("eri", Ujo3Volo)
        BazaFinaĵo("ie", AtributivoEstiMalantaŭ)
        BazaFinaĵo("ia", AtributivoEstiAntaŭ)
        BazaFinaĵo("ea", Imperativo)
        BazaFinaĵo("a", Invito)
        BazaFinaĵo("etio", Argumento1)
        BazaFinaĵo("elish", Translativo)
        BazaFinaĵo("mea", Ĝerundo)
        BazaFinaĵo("am", PartaUjo1)
        BazaFinaĵo("an", PartaUjo1)
        BazaFinaĵo("om", PartaUjo3)
        BazaFinaĵo("os", PartaUjo3)
        BazaFinaĵo("ros", UnueUjo3)
        BazaFinaĵo("es", Reflekcio) ]
      @ okazoFinaĵoj

   let oblikaNetransitivaVerboInflekcioj =
      [ BazaFinaĵo("ia", Progresivo)
        BazaFinaĵo("e", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ore", Ujo2Volo)
        BazaFinaĵo("ra", AtributivoEstiMalantaŭ)
        BazaFinaĵo("re", AtributivoEstiAntaŭ)
        BazaFinaĵo("ea", Ĝerundo)
        BazaFinaĵo("oniaa", Argumento2)
        BazaFinaĵo("am", PartaUjo2) ]
      @ okazoFinaĵoj

   let oblikaTransitivaVerboInflekcioj =
      [ BazaFinaĵo("ia", Progresivo)
        BazaFinaĵo("i", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("ore", Ujo2Volo)
        BazaFinaĵo("eri", Ujo3Volo)
        BazaFinaĵo("ri", AtributivoEstiMalantaŭ)
        BazaFinaĵo("re", AtributivoEstiAntaŭ)
        BazaFinaĵo("ea", Ĝerundo)
        BazaFinaĵo("oniaa", Argumento2)
        BazaFinaĵo("eru", Argumento3)
        BazaFinaĵo("om", PartaUjo2)
        BazaFinaĵo("on", PartaUjo2)
        BazaFinaĵo("im", PartaUjo3)
        BazaFinaĵo("ig", PartaUjo3)
        BazaFinaĵo("rig", UnueUjo3)
        BazaFinaĵo("eg", Reflekcio) ]
      @ okazoFinaĵoj

   let nedirektaNetransitivaVerboInflekcioj =
      [ BazaFinaĵo("ia", Progresivo)
        BazaFinaĵo("io", Perfekto)
        BazaFinaĵo("ela", Estonteco)
        BazaFinaĵo("eri", Ujo3Volo)
        BazaFinaĵo("ea", Ĝerundo)
        BazaFinaĵo("oniaa", Argumento2)
        BazaFinaĵo("om", PartaUjo3) ]
      @ okazoFinaĵoj

   let pridirantoInflekcioj =
      [ BazaFinaĵo("e", Difinito)
        BazaFinaĵo("a", UnuNombro)
        BazaFinaĵo("ie", PluraNombro)
        BazaFinaĵo("u", Havaĵo)
        BazaFinaĵo("ia", PredikativoEsti)
        BazaFinaĵo("ea", AtributivoEstiMalantaŭ)
        BazaFinaĵo("ra", AtributivoEstiAntaŭ)
        BazaFinaĵo("io", Sola)
        BazaFinaĵo("im", Translativo)
        BazaFinaĵo("is", Translativo)
        BazaFinaĵo("i", Ĝerundo)
        BazaFinaĵo("em", Igo)
        BazaFinaĵo("eg", Igo)
        BazaFinaĵo("es", Igo)
        BazaFinaĵo("et", Igo)
        BazaFinaĵo("od", Etigo) ]

   let private inflekciojPerVorttipoj =
      [ NombrigeblaKlaso, nombrigeblaKlasoInflekcioj
        NenombrigeblaKlaso, nenombrigeblaKlasoInflekcioj
        AntaŭNombrigeblaEco, nombrigeblaKlasoInflekcioj
        AntaŭNenombrigeblaEco, nenombrigeblaKlasoInflekcioj
        MalantaŭNombrigeblaEco, nombrigeblaKlasoInflekcioj
        MalantaŭNenombrigeblaEco, nenombrigeblaKlasoInflekcioj
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
      |> List.map (fun (vorttipo, finaĵoj) ->
            finaĵoj
            |> List.map (fun finaĵo -> (vorttipo, finaĵo)))
      |> List.concat

   let nombrigeblaDifinitoFinaĵoj =
      nombrigeblaDifinitoFinaĵoj
      |> List.map (fun finaĵo -> (finaĵo, NombrigeblaKlaso))
      |> List.append [ "dre", AntaŭNombrigeblaEco
                       "dri", MalantaŭNombrigeblaEco ]

   let difinitoFinaĵoj =
      nombrigeblaDifinitoFinaĵoj
      @ (nenombrigeblaDifinitoFinaĵoj
         |> List.map (fun finaĵo -> (finaĵo, NenombrigeblaKlaso)))
      |> List.append [ "gre", AntaŭNenombrigeblaEco
                       "gri", MalantaŭNenombrigeblaEco ]

   let bazajPredikativoEstiFinaĵoj =
      (nombrigeblaPredikativoEstiFinaĵoj
       |> List.map (fun finaĵo -> (finaĵo, NombrigeblaKlaso)))
      @ (nenombrigeblaInfinitivoFinaĵoj
         |> List.map (fun finaĵo -> (finaĵo, NenombrigeblaKlaso)))
      |> List.append [ "dro", AntaŭNombrigeblaEco
                       "dru", MalantaŭNombrigeblaEco
                       "gro", AntaŭNenombrigeblaEco
                       "gru", MalantaŭNenombrigeblaEco ]
   
   let predikativoEstiFinaĵoj =
      bazajPredikativoEstiFinaĵoj
      @ (nebazajNenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> (finaĵo, NenombrigeblaKlaso)))
   
   let nebazajDifinitoFinaĵoj =
      bazajPredikativoEstiFinaĵoj
      |> List.map (fun (finaĵo, vorttipo) -> (finaĵo + "re", "re", vorttipo, Kvalito))

   let difinitivoAlInfinitivoTabelo =
      [ ('a', "aa"); ('e', "o"); ('i', "u") ]
      |> Map.ofList

   let vokaloj =
      [ 'i'; 'e'; 'a'; 'u'; 'o'; 'ɒ' ] |> Set.ofList

   let ĉuVokalo litero = Set.contains litero vokaloj

   type Litero =
      | Konsonanto of char
      | Vokalo of char

   let difinitoAlInfinitivo (vorto: string) =
      vorto.Substring(0, vorto.Length - 1)
      + difinitivoAlInfinitivoTabelo.[vorto.Chars(vorto.Length - 1)]

   let infinitivoAlDifinito (vorto: string) =
      difinitivoAlInfinitivoTabelo
      |> Map.pick (fun difinitoFinaĵo infinitivoFinaĵo ->
            if vorto.EndsWith(infinitivoFinaĵo) then
               (vorto.Substring(0, vorto.Length - infinitivoFinaĵo.Length)
                + difinitoFinaĵo.ToString())
               |> Some
            else
               None)

   let ĉuDifinito (vorto: string) akceptiNenombrigeblan =
      match akceptiNenombrigeblan with
      | AkceptiNenombrigeblan -> difinitoFinaĵoj
      | NeAkceptiNenombrigeblan -> nombrigeblaDifinitoFinaĵoj
      |> List.tryPick (fun (finaĵo, vorttipo) ->
            if vorto.EndsWith(finaĵo)
               && vorto.Length > finaĵo.Length then
               Some vorttipo
            else
               None)

   let ĉuPredikativoEsti (vorto: string) =
      predikativoEstiFinaĵoj
      |> List.tryPick (fun (finaĵo, vorttipo) -> if vorto.EndsWith(finaĵo) then Some vorttipo else None)

   let malinflektiSeDifinito (vorto: string) inflekcio akceptiNenombrigeblan ĉuBazo =
      let bazoŜtupo =
         ĉuDifinito vorto akceptiNenombrigeblan
         |> Option.map (fun vorttipo -> (if ĉuBazo then Bazo else Nebazo) (vorttipo, inflekcio, vorto))
      if Option.isNone bazoŜtupo then
         nebazajDifinitoFinaĵoj
         |> List.tryPick (fun (finaĵo, _, vorttipo, _) ->
            if vorto.EndsWith(finaĵo) && vorto.Length > finaĵo.Length then
               Some(Nebazo(vorttipo, inflekcio, vorto))
            else
               None)
      else bazoŜtupo

   let malinflektiSePredikativoInfinito vorto inflekcio =
      ĉuPredikativoEsti vorto
      |> Option.map (fun vorttipo -> Nebazo(vorttipo, inflekcio, infinitivoAlDifinito vorto))

   let unuNombroFinaĵo = "si"
   let pluraNombroFinaĵo = "ve"

   let rec malinflekti (vorto: EniraVorto): Result<MalinflektaŜtupo, Eraro> =
      let ĉeno = vorto.Vorto
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo, ĉeno) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo, ĉeno) |> Ok
      | _ ->
         ĉiujInflekcioj
         |> List.tryPick (fun (vorttipo, finaĵo) ->
               match finaĵo with
               | BazaFinaĵo (finaĵo, inflekcio) ->
                  bazajFinaĵoj
                  |> Map.tryPick (fun infinitivoFinaĵo infinitivoTipo ->
                        if ĉeno.EndsWith(infinitivoFinaĵo + finaĵo)
                           && infinitivoTipo = vorttipo then
                           Some()
                        else
                           None)
                  |> Option.bind (fun _ ->
                        Some(ĉeno.Substring(0, ĉeno.Length - finaĵo.Length))
                        |> Option.filter (fun subĉeno ->
                              match dividi subĉeno true with
                              | Ok (_) -> true
                              | Error (_) -> false)
                        |> Option.map (fun malinflektita -> Nebazo(vorttipo, inflekcio, malinflektita)))
               | PredikativoEstiFinaĵo (finaĵo, inflekcio) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let radiko =
                        ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)

                     malinflektiSePredikativoInfinito radiko inflekcio
                  else
                     None
               | DifinitoFinaĵo (finaĵo, inflekcio) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let difinito =
                        ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)

                     malinflektiSeDifinito difinito inflekcio AkceptiNenombrigeblan (finaĵo.Length = 0)
                  else
                     None
               | DUPFinaĵo (finaĵo, difinito, unuNombro, pluraNombro) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let restantaj =
                        ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)

                     let ĉuBazo = finaĵo.Length = 0
                     if restantaj.EndsWith(unuNombroFinaĵo) then
                        let difinito =
                           ĉeno.Substring(0, restantaj.Length - unuNombroFinaĵo.Length)

                        malinflektiSeDifinito difinito unuNombro NeAkceptiNenombrigeblan ĉuBazo
                     elif restantaj.EndsWith(pluraNombroFinaĵo) then
                        let difinito =
                           ĉeno.Substring(0, restantaj.Length - pluraNombroFinaĵo.Length)

                        malinflektiSeDifinito difinito pluraNombro NeAkceptiNenombrigeblan ĉuBazo
                     else
                        malinflektiSeDifinito restantaj difinito AkceptiNenombrigeblan ĉuBazo
                  else
                     None)
         |> Option.orElseWith (fun () ->
               ĉuBazo ĉeno
               |> Option.map (fun vorttipo -> Bazo(vorttipo, Infinitivo, ĉeno)))
         |> Option.map Ok
         |> Option.defaultValue
               ((vorto, (sprintf "%s estas nevalida" ĉeno))
                |> Error)

   and tuteMalinflekti (vorto: EniraVorto) =
      malinflekti vorto
      |> Result.bind (fun malinflektita ->
            match malinflektita with
            | Bazo (_, _, bazaVorto) ->
               { BazaVorto = bazaVorto
                 InflekcioŜtupoj = List.singleton malinflektita
                 OriginalaVorto = vorto }
               |> Ok
            | Nebazo (_, _, restanta) ->
               tuteMalinflekti { vorto with Vorto = restanta }
               |> Result.map (fun sekva ->
                     { sekva with
                          InflekcioŜtupoj = malinflektita :: sekva.InflekcioŜtupoj
                          OriginalaVorto = vorto }))

   and tuteMalinflektiĈiujn (ĉenoj: EniraVorto list) =
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
      tuteMalinflekti { Vico = 0; Pozo = 0; Vorto = ĉeno }
      |> Result.map (fun malinflektitaVorto ->
            match malinflektitaVorto.InflekcioŜtupoj with
            | unua :: _ ->
               match unua with
               | Bazo (vorttipo, _, _) -> verboTipoj |> Set.contains vorttipo
               | Nebazo (_, _, _) -> failwith "ne valida unua ŝtupo"
            | [] -> failwith "ne valida ŝtupoj")

   and ĉuVerboInfinitivo (ĉeno: string) =
      tuteMalinflekti { Vico = 0; Pozo = 0; Vorto = ĉeno }
      |> Result.map (fun malinflektitaVorto ->
            match malinflektitaVorto.InflekcioŜtupoj with
            | [ solaŜtupo ] ->
               match solaŜtupo with
               | Bazo (vorttipo, inflekcio, _) ->
                  (inflekcio = Infinitivo)
                  && verboTipoj
                  |> Set.contains vorttipo
               | _ -> false
            | _ -> false)

   and ĉuInfinitivoB (ĉeno: string) = ĉuBazo ĉeno |> Option.isSome

   and ĉuVerboInfinitivoB (ĉeno: string) =
      match ĉuVerboInfinitivo ĉeno with
      | Ok (rezulto) -> rezulto
      | Error (_) -> false

   and bazoDe (vorto: string) =
      if ĉuVerboInfinitivoB vorto then
         vorto.Substring
            (0,
             vorto.Length
             - (if vorto.EndsWith("sh") then 2 else 1))
      else
         vorto

   and bazoPorDividi (infinitivo: string) =
      [ "gru"
        "gro"
        "dru"
        "dro"
        "li"
        "lu"
        "d"
        "l"
        "r" ]
      |> List.tryPick (fun finaĵo ->
            if infinitivo.EndsWith(finaĵo)
            then Some(infinitivo.Substring(0, infinitivo.Length - finaĵo.Length))
            else None)
      |> Option.defaultValue infinitivo
      |> bazoDe

   and normaligiEnVortaranFormon (infinitivo: string) =
      if Char.IsUpper(infinitivo.[0]) then
         infinitivo
      else
         [ "gru", "gro"; "dru", "dro"; "r", "l" ]
         |> List.tryPick (fun (alterativa, vortara) ->
               if infinitivo.EndsWith(alterativa) then
                  infinitivo.Substring(0, infinitivo.Length - alterativa.Length)
                  + vortara
                  |> Some
               else
                  None)
         |> Option.defaultValue infinitivo
         |> bazoDe

   and ĉuMalplenigita (malplenigita: Vorttipo) (originala: string) =
      ĉuBazo originala
      |> Option.filter (fun originalaTipo -> Set.contains originalaTipo verboTipoj)
      |> Option.bind (fun originalaTipo -> Map.tryFind originalaTipo malplenigeblaVerboTipoj)
      |> Option.map (fun malplenigeblaTipoj -> Set.contains malplenigita malplenigeblaTipoj)
      |> Option.defaultValue false

   and malplenigitajFormojDe (ĉeno: string) =
      ĉuBazo ĉeno
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
      List.map (fun litero -> if ĉuVokalo litero then Vokalo litero else Konsonanto litero)

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
               |> Result.map (fun restantajSilaboj ->
                     String.Concat([ k1; k2; v; kf ])
                     :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto (k1); Konsonanto (k2); Vokalo (v); Konsonanto (kf) ] ->
            if ĉuKomenca
            then [ String.Concat([ k1; k2; v; kf ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CCV
         | Konsonanto (k1) :: Konsonanto (k2) :: Vokalo (v) :: Konsonanto (kf) :: Vokalo (v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Konsonanto(kf) :: Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | Konsonanto (k1) :: Konsonanto (k2) :: Vokalo (v) :: Vokalo (v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else
               Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto (k1); Konsonanto (k2); Vokalo (v) ] ->
            if ĉuKomenca
            then [ String.Concat([ k1; k2; v ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CVC
         | Konsonanto (k1) :: Vokalo (v) :: Konsonanto (kf) :: Konsonanto (kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v; kf ]) :: restantajSilaboj)
         | [ Konsonanto (k1); Vokalo (v); Konsonanto (kf) ] -> [ String.Concat([ k1; v; kf ]) ] |> Ok
         // CV
         | Konsonanto (k1) :: Vokalo (v) :: Konsonanto (kk2) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v ]) :: restantajSilaboj)
         | Konsonanto (k1) :: Vokalo (v) :: Vokalo (v2) :: restantaj ->
            dividiAk false (Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v ]) :: restantajSilaboj)
         | [ Konsonanto (k1); Vokalo (v) ] -> [ String.Concat([ k1; v ]) ] |> Ok
         // VC
         | Vokalo (v) :: Konsonanto (kf) :: Konsonanto (kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ v; kf ]) :: restantajSilaboj)
         | [ Vokalo (v); Konsonanto (kf) ] -> [ String.Concat([ v; kf ]) ] |> Ok
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

      dividiAk
         true
         (normaligita.ToCharArray()
          |> List.ofArray
          |> kategorigiLiterojn)

   and dividiKunFinaĵo (vorto: string) =
      dividi vorto false
      |> Result.bind (fun silaboj ->
            ĉuBazo vorto
            |> Option.map (fun vorttipo -> silaboj @ [ vorttipo.ToString() ])
            |> Option.map Ok
            |> Option.defaultValue (Error(sprintf "%s ne estas infinitivo" vorto)))

   let predikatajBazajTipoj =
      [ NombrigeblaKlaso
        NenombrigeblaKlaso
        MalantaŭNombrigeblaEco
        AntaŭNombrigeblaEco
        MalantaŭNenombrigeblaEco
        AntaŭNenombrigeblaEco ]
      |> Set.ofList

   let predikatajNebazajInflekcioj =
      [ Progresivo
        Perfekto
        Estonteco
        Desiderativo
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
        PluraSola ]
      |> Set.ofList

   let argumentajBazajTipoj =
      [ FremdaVorto; Lokokupilo ] |> Set.ofList

   let argumentajInflekcioj =
      [ Difinito
        UnuNombro
        PluraNombro
        SolaFormo
        Havaĵo
        UnuHavaĵo
        PluraHavaĵo
        Kvalito ]
      |> Set.ofList

   let malantaŭModifantajInflekcioj = [ AtributivoEstiMalantaŭ ] |> Set.ofList

   let antaŭModifantajInflekcioj = [ AtributivoEstiAntaŭ ] |> Set.ofList

   let ĉuPredikataVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (vorttipo, _, _) -> Set.contains vorttipo predikatajBazajTipoj
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio predikatajNebazajInflekcioj

   let ĉuArgumentaVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (_, inflekcio, _) -> Set.contains inflekcio argumentajInflekcioj
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio argumentajInflekcioj

   let ĉuDifinita (vorto: MalinflektitaVorto) =
      ĉuArgumentaVorto vorto
      && match List.last vorto.InflekcioŜtupoj with
         | Bazo (vorttipo, _, _) ->
            vorttipo = NombrigeblaKlaso
            || vorttipo = NenombrigeblaKlaso
            || vorttipo = AntaŭNenombrigeblaEco
            || vorttipo = AntaŭNombrigeblaEco
            || vorttipo = MalantaŭNombrigeblaEco
            || vorttipo = MalantaŭNenombrigeblaEco
         | _ -> false

   let ĉuMalantaŭModifantaVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Bazo (_, _, _) -> false
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio malantaŭModifantajInflekcioj

   let ĉuAntaŭModifantaVorto (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Nebazo (_, inflekcio, _) -> Set.contains inflekcio antaŭModifantajInflekcioj
      | _ -> false

   let ĉuSolaArgumento (vorto: MalinflektitaVorto) =
      match vorto.InflekcioŜtupoj.Head with
      | Nebazo (_, inflekcio, _) ->
         inflekcio = Sola
         || inflekcio = UnuSola
         || inflekcio = PluraSola
      | _ -> false

   let ĉuAntaŭEco (vorto: MalinflektitaVorto) =
      let unuaInflekcio = List.last vorto.InflekcioŜtupoj
      match unuaInflekcio with
      | Bazo (vorttipo, inflekcio, _) ->
         (vorttipo = AntaŭNombrigeblaEco
          && (inflekcio = Difinito
              || inflekcio = UnuNombro
              || inflekcio = PluraNombro))
         || (vorttipo = AntaŭNenombrigeblaEco
             && inflekcio = Difinito)
      | _ -> false

   let ĉuMalantaŭEco (vorto: MalinflektitaVorto) =
      let unuaInflekcio = List.last vorto.InflekcioŜtupoj
      match unuaInflekcio with
      | Bazo (vorttipo, inflekcio, _) ->
         (vorttipo = MalantaŭNombrigeblaEco
          && (inflekcio = Difinito
              || inflekcio = UnuNombro
              || inflekcio = PluraNombro))
         || (vorttipo = MalantaŭNenombrigeblaEco
             && inflekcio = Difinito)
      | _ -> false

   let ĉuEcoHavaĵo vorto =
      vorto.InflekcioŜtupoj
      |> List.exists (fun ŝtupo ->
            match ŝtupo with
            | Nebazo (vorttipo, inflekcio, _) ->
               (vorttipo = AntaŭNombrigeblaEco
                || vorttipo = AntaŭNenombrigeblaEco
                || vorttipo = MalantaŭNombrigeblaEco
                || vorttipo = MalantaŭNenombrigeblaEco)
               && (inflekcio = Havaĵo
                   || inflekcio = UnuHavaĵo
                   || inflekcio = PluraHavaĵo)
            | Bazo (_, _, _) -> false)

   let ĉiujInflekciojDe vorto =
      ĉuBazo vorto
      |> Option.bind (fun vorttipo -> Map.tryFind vorttipo inflekciojPerVorttipoj)
      |> Option.map (fun finaĵoj ->
            finaĵoj
            |> List.map (fun finaĵo ->
                  match finaĵo with
                  | BazaFinaĵo (sufikso, inflekcio) -> (inflekcio, vorto + sufikso)
                  | DifinitoFinaĵo (sufikso, inflekcio) -> (inflekcio, vorto + sufikso)
                  | DUPFinaĵo (sufikso, inflekcio, _, _) ->
                     (inflekcio,
                      (sprintf
                         "%s, %s, %s"
                          (vorto + sufikso)
                          (vorto + unuNombroFinaĵo + sufikso)
                          (vorto + pluraNombroFinaĵo + sufikso)))
                  | PredikativoEstiFinaĵo (sufikso, inflekcio) -> (inflekcio, (difinitoAlInfinitivo vorto) + sufikso))
            |> Map.ofList)

   let valencoDeInfinitivo vorto =
      ĉuBazo vorto
      |> Option.map (fun vorttipo ->
            match vorttipo with
            | NombrigeblaKlaso
            | NenombrigeblaKlaso -> 0
            | NetransitivaVerbo
            | OblikaNetransitivaVerbo
            | NedirektaNetransitivaVerbo -> 1
            | TransitivaVerbo
            | NedirektaTransitivaVerbo
            | OblikaTransitivaVerbo -> 2
            | DutransitivaVerbo -> 3
            | _ -> 0)
      |> Option.defaultValue 0

   let vortaraTipoDe (vorto: string) =
      match vorto |> testaVorto |> malinflekti with
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
            | MalantaŭNenombrigeblaEco
            | AntaŭNombrigeblaEco
            | MalantaŭNombrigeblaEco -> "Associative class"
            | AntaŭRekordo
            | MalantaŭRekordo -> "Record"
            | MalantaŭModifanto
            | AntaŭModifanto -> "Modifier"
            | Lokokupilo -> "Placeholder"
            | FremdaVorto -> "Foreign word"
            | Makro -> "Macro"
         | Nebazo (_) -> failwith "Nevalida lasta malinflekta ŝtupo"
      | Error ((_, eraro)) -> failwith eraro
