namespace KrestiaVortilo

open Vorttipo
open Sintaksanalizilo

module Sintaksanalizilo2 =
   type Finaĵo =
      | InfinitivoFinaĵo of string * Inflekcio
      | DifinitoFinaĵo of string * Inflekcio
      | DUPFinaĵo of string * Definito: Inflekcio * UnuNombro: Inflekcio * PluraNombro: Inflekcio

   type ĈuAkceptiNenombrigeblan =
      | AkceptiNenombrigeblan
      | NeAkceptiNenombrigeblan

   let kreiListon vorttipo listo =
      listo |> List.map (fun finaĵo -> (vorttipo, finaĵo))

   let nombrigeblaKlasoInflekcioj =
      [ DUPFinaĵo("", Difinito, UnuNombro, PluraNombro)
        DUPFinaĵo("nsa", Havaĵo, UnuHavaĵo, PluraHavaĵo)
        InfinitivoFinaĵo("wa", PredikativoEsti)
        InfinitivoFinaĵo("ga", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("va", AtributativoEstiAntaŭ)
        DUPFinaĵo("ra", Sola, UnuSola, PluraSola)
        DUPFinaĵo("ris", Havado, UnuHavado, PluraHavado)
        DUPFinaĵo("rim", Ekzistado, UnuEkzistado, PluraEkzistado)
        DifinitoFinaĵo("las", Translativo)
        DifinitoFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("va", SpecifaĜerundo) ]
      |> kreiListon NombrigeblaKlaso

   let nenombrigeblaKlasoInflekcioj =
      [ DifinitoFinaĵo("", Difinito)
        DifinitoFinaĵo("nsa", Havaĵo)
        InfinitivoFinaĵo("wa", PredikativoEsti)
        InfinitivoFinaĵo("ga", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("va", AtributativoEstiAntaŭ)
        DifinitoFinaĵo("ra", Sola)
        DifinitoFinaĵo("ris", Havado)
        DifinitoFinaĵo("rim", Ekzistado)
        DifinitoFinaĵo("las", Translativo)
        DifinitoFinaĵo("vra", Ĝerundo)
        DifinitoFinaĵo("va", SpecifaĜerundo) ]
      |> kreiListon NenombrigeblaKlaso

   let malplenaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("io", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("elim", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo) ]
      |> kreiListon MalplenaVerbo

   let netransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("e", Progresivo)
        InfinitivoFinaĵo("o", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", NominativoVolo)
        InfinitivoFinaĵo("ie", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("ia", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Imperativo)
        InfinitivoFinaĵo("a", Invito)
        InfinitivoFinaĵo("etio", Aganto)
        InfinitivoFinaĵo("elis", Translativo)
        InfinitivoFinaĵo("mea", Ĝerundo)
        InfinitivoFinaĵo("em", PartaNominativo) ]
      |> kreiListon NetransitivaVerbo

   let transitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("re", Progresivo)
        InfinitivoFinaĵo("ro", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", NominativoVolo)
        InfinitivoFinaĵo("ore", AkuzativoVolo)
        InfinitivoFinaĵo("rie", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("lia", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ri", Imperativo)
        InfinitivoFinaĵo("ia", Invito)
        InfinitivoFinaĵo("etio", Aganto)
        InfinitivoFinaĵo("oniaa", Patiento)
        InfinitivoFinaĵo("elit", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("ig", PartaNominativo)
        InfinitivoFinaĵo("es", PartaAkuzativo)
        InfinitivoFinaĵo("is", Reflekcio) ]
      |> kreiListon TransitivaVerbo

   let dutransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("re", Progresivo)
        InfinitivoFinaĵo("ro", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", NominativoVolo)
        InfinitivoFinaĵo("ore", AkuzativoVolo)
        InfinitivoFinaĵo("eri", DativoVolo)
        InfinitivoFinaĵo("rie", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("lia", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ri", Imperativo)
        InfinitivoFinaĵo("ia", Invito)
        InfinitivoFinaĵo("etio", Aganto)
        InfinitivoFinaĵo("oniaa", Patiento)
        InfinitivoFinaĵo("elip", Translativo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("ev", PartaNominativo)
        InfinitivoFinaĵo("esh", PartaAkuzativo)
        InfinitivoFinaĵo("et", PartaDativo)
        InfinitivoFinaĵo("ish", Reflekcio) ]
      |> kreiListon DutransitivaVerbo

   let nedirektaTransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("e", Progresivo)
        InfinitivoFinaĵo("o", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ora", NominativoVolo)
        InfinitivoFinaĵo("eri", DativoVolo)
        InfinitivoFinaĵo("ie", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("ia", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Imperativo)
        InfinitivoFinaĵo("a", Invito)
        InfinitivoFinaĵo("etio", Aganto)
        InfinitivoFinaĵo("elish", Translativo)
        InfinitivoFinaĵo("mea", Ĝerundo) ]
      |> kreiListon NedirektaTransitivaVerbo

   let oblikaNetransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("e", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ore", AkuzativoVolo)
        InfinitivoFinaĵo("ra", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("re", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("orio", Aganto)
        InfinitivoFinaĵo("am", PartaAkuzativo) ]
      |> kreiListon OblikaNetransitivaVerbo

   let oblikaTransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("i", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("ore", AkuzativoVolo)
        InfinitivoFinaĵo("eri", DativoVolo)
        InfinitivoFinaĵo("ri", AtributativoEstiMalantaŭ)
        InfinitivoFinaĵo("re", AtributativoEstiAntaŭ)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("orio", Aganto)
        InfinitivoFinaĵo("etio", Patiento)
        InfinitivoFinaĵo("on", PartaAkuzativo)
        InfinitivoFinaĵo("ig", PartaDativo) ]
      |> kreiListon OblikaTransitivaVerbo

   let nedirektaNetransitivaVerboInflekcioj =
      [ InfinitivoFinaĵo("ia", Progresivo)
        InfinitivoFinaĵo("io", Perfekto)
        InfinitivoFinaĵo("ela", Estonteco)
        InfinitivoFinaĵo("eri", DativoVolo)
        InfinitivoFinaĵo("ea", Ĝerundo)
        InfinitivoFinaĵo("etio", Patiento)
        InfinitivoFinaĵo("om", PartaDativo) ]
      |> kreiListon NedirektaNetransitivaVerbo

   let ĉiujInflekcioj =
      nombrigeblaKlasoInflekcioj
      @ nenombrigeblaKlasoInflekcioj
        @ malplenaVerboInflekcioj
          @ netransitivaVerboInflekcioj
            @ transitivaVerboInflekcioj
              @ oblikaNetransitivaVerboInflekcioj
                @ oblikaTransitivaVerboInflekcioj @ nedirektaNetransitivaVerboInflekcioj
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

   let ĉuDifinito (vorto: string) akceptiNenombrigeblan =
      match akceptiNenombrigeblan with
      | AkceptiNenombrigeblan -> difinitoFinaĵoj
      | NeAkceptiNenombrigeblan -> nombrigeblaDifinitoFinaĵoj
      |> List.tryPick (fun (finaĵo, vorttipo) ->
            if vorto.EndsWith(finaĵo) && vorto.Length > finaĵo.Length then Some vorttipo
            else None)

   let malinflektiSiDifinito (vorto: string) inflekcio akceptiNenombrigeblan =
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
               | InfinitivoFinaĵo(finaĵo, inflekcio) ->
                  infinitivoFinaĵoj
                  |> Map.tryPick (fun infinitivoFinaĵo infinitivoTipo ->
                        if ĉeno.EndsWith(infinitivoFinaĵo + finaĵo) && infinitivoTipo = vorttipo then Some()
                        else None)
                  |> Option.bind (fun _ ->
                        Some(ĉeno.Substring(0, ĉeno.Length - finaĵo.Length))
                        |> Option.filter (fun subĉeno ->
                              match dividi subĉeno true with
                              | Ok(_) -> true
                              | Error(_) -> false)
                        |> Option.map (fun malinflektita -> Nebazo(vorttipo, inflekcio, malinflektita)))
               | DifinitoFinaĵo(finaĵo, inflekcio) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let difinito = ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)
                     malinflektiSiDifinito difinito inflekcio AkceptiNenombrigeblan
                  else
                     None
               | DUPFinaĵo(finaĵo, difinito, unuNombro, pluraNombro) ->
                  if ĉeno.EndsWith(finaĵo) then
                     let restantaj = ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)
                     if restantaj.EndsWith(unuNombroFinaĵo) then
                        let difinito = ĉeno.Substring(0, restantaj.Length - unuNombroFinaĵo.Length)
                        malinflektiSiDifinito difinito unuNombro NeAkceptiNenombrigeblan
                     elif restantaj.EndsWith(pluraNombroFinaĵo) then
                        let difinito = ĉeno.Substring(0, restantaj.Length - pluraNombroFinaĵo.Length)
                        malinflektiSiDifinito difinito pluraNombro NeAkceptiNenombrigeblan
                     else
                        malinflektiSiDifinito restantaj difinito AkceptiNenombrigeblan
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
            | Bazo(_, _, _) ->
               malinflektita
               |> List.singleton
               |> Ok
            | Nebazo(_, _, restanta) ->
               tuteMalinflekti restanta |> Result.map (fun sekvaj -> malinflektita :: sekvaj))

   and ĉuVerbo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun ŝtupoj ->
            match ŝtupoj with
            | unua :: _ ->
               match unua with
               | Bazo(vorttipo, _, _) ->
                  verboTipoj |> Set.contains vorttipo
               | Nebazo(_, _, _) -> failwith "ne valida unua ŝtupo"
            | [] -> failwith "ne valida ŝtupoj")

   and ĉuVerboInfinitivo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun ŝtupoj ->
            match ŝtupoj with
            | [ solaŜtupo ] ->
               match solaŜtupo with
               | Bazo(vorttipo, inflekcio, _) ->
                  (inflekcio = Infinitivo) && verboTipoj |> Set.contains vorttipo
               | _ -> false
            | _ -> false)

   and ĉuInfinitivoB (ĉeno: string) =
      ĉuInfinitivo ĉeno |> Option.isSome

   and ĉuVerboInfinitivoB (ĉeno: string) =
      match ĉuVerboInfinitivo ĉeno with
      | Ok(rezulto) -> rezulto
      | Error(_) -> false

   and bazoDe (vorto: string) =
      if ĉuVerboInfinitivoB vorto then
         vorto.Substring
            (0,
             (if vorto.EndsWith("sh") then vorto.Length - 2
              else vorto.Length - 1))
      elif vorto.EndsWith("l") || vorto.EndsWith("r") then
         vorto.Substring(0, vorto.Length - 1)
      else
         vorto

   and bazoPorDividi (infinitivo: string) =
      [ "gru"; "gro"; "dru"; "dro"; "li"; "lu"; "d" ]
      |> List.tryPick (fun finaĵo ->
            if infinitivo.EndsWith(finaĵo) then Some(infinitivo.Substring(0, infinitivo.Length - finaĵo.Length))
            else None)
      |> Option.defaultValue infinitivo
      |> bazoDe

   and ĉuMalplenigita (malplenigita: Vorttipo) (originala: string) =
      ĉuInfinitivo originala
      |> Option.filter (fun originalaTipo -> Set.contains originalaTipo verboTipoj)
      |> Option.bind (fun originalaTipo -> Map.tryFind originalaTipo malplenigeblaVerboTipoj)
      |> Option.filter (fun malplenigeblaTipoj -> Set.contains malplenigita malplenigeblaTipoj)
      |> Option.isSome

   and kategorigiLiterojn =
      List.map (fun litero ->
         if ĉuVokalo litero then Vokalo litero
         else Konsonanto litero)

   and dividi (ĉeno: string) (inkluziFinaĵon: bool) =
      let normaligita =
         ĉeno.Replace("aa", "ɒ").Replace("sh", "ʃ")
         |> if inkluziFinaĵon then (fun s -> s)
            else bazoPorDividi

      let rec dividiAk ĉuKomenca (literoj: Litero list): Result<string list, string> =
         match literoj with
         // CCVC
         | Konsonanto(k1) :: Konsonanto(k2) :: Vokalo(v) :: Konsonanto(kf) :: Konsonanto(kk2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Konsonanto(kk2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v; kf ]) :: restantajSilaboj)
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto(k1); Konsonanto(k2); Vokalo(v); Konsonanto(kf) ] ->
            if ĉuKomenca then [ System.String.Concat([ k1; k2; v; kf ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CCV
         | Konsonanto(k1) :: Konsonanto(k2) :: Vokalo(v) :: Konsonanto(kf) :: Vokalo(v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Konsonanto(kf) :: Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         | Konsonanto(k1) :: Konsonanto(k2) :: Vokalo(v) :: Vokalo(v2) :: restantaj ->
            if ĉuKomenca then
               dividiAk false (Vokalo(v2) :: restantaj)
               |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; k2; v ]) :: restantajSilaboj)
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         | [ Konsonanto(k1); Konsonanto(k2); Vokalo(v) ] ->
            if ĉuKomenca then [ System.String.Concat([ k1; k2; v ]) ] |> Ok
            else Error "Vorto ne rajtas komenci per du konsonantoj"
         // CVC
         | Konsonanto(k1) :: Vokalo(v) :: Konsonanto(kf) :: Konsonanto(kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v; kf ]) :: restantajSilaboj)
         | [ Konsonanto(k1); Vokalo(v); Konsonanto(kf) ] -> [ System.String.Concat([ k1; v; kf ]) ] |> Ok
         // CV
         | Konsonanto(k1) :: Vokalo(v) :: Konsonanto(kk2) :: Vokalo(v2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v ]) :: restantajSilaboj)
         | Konsonanto(k1) :: Vokalo(v) :: Vokalo(v2) :: restantaj ->
            dividiAk false (Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ k1; v ]) :: restantajSilaboj)
         | [ Konsonanto(k1); Vokalo(v) ] -> [ System.String.Concat([ k1; v ]) ] |> Ok
         // VC
         | Vokalo(v) :: Konsonanto(kf) :: Konsonanto(kk2) :: restantaj ->
            dividiAk false (Konsonanto(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> System.String.Concat([ v; kf ]) :: restantajSilaboj)
         | [ Vokalo(v); Konsonanto(kf) ] -> [ System.String.Concat([ v; kf ]) ] |> Ok
         // V
         | Vokalo(v) :: Konsonanto(kf) :: Vokalo(v2) :: restantaj ->
            dividiAk false (Konsonanto(kf) :: Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
         | Vokalo(v) :: Vokalo(v2) :: restantaj ->
            dividiAk false (Vokalo(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
         | [ Vokalo(v) ] -> [ v.ToString() ] |> Ok
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
