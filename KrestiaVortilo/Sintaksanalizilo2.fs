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

   let ĉiujInflekcioj = nombrigeblaKlasoInflekcioj
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

   let malinflekti (ĉeno: string): Result<MalinflektaŜtupo, string> =
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo, ĉeno) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo, ĉeno) |> Ok
      | _ ->
         ĉiujInflekcioj
         |> List.tryPick (fun (vorttipo, finaĵo) ->
               match finaĵo with
               | InfinitivoFinaĵo(finaĵo, inflekcio) ->
                  infinitivoFinaĵoj
                  |> Map.tryPick (fun infinitivoFinaĵo _ ->
                        if ĉeno.EndsWith(infinitivoFinaĵo + finaĵo) then Some()
                        else None)
                  |> Option.map (fun _ -> Nebazo(vorttipo, inflekcio, ĉeno.Substring(0, ĉeno.Length - finaĵo.Length)))
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

   let rec tuteMalinflekti (ĉeno: string) =
      malinflekti ĉeno
      |> Result.bind (fun malinflektita ->
            match malinflektita with
            | Bazo(_, _, _) ->
               malinflektita
               |> List.singleton
               |> Ok
            | Nebazo(_, _, restanta) ->
               tuteMalinflekti restanta |> Result.map (fun sekvaj -> malinflektita :: sekvaj))

   let ĉuVerbo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun ŝtupoj ->
            match ŝtupoj with
            | unua :: _ ->
               match unua with
               | Bazo(vorttipo, _, _) ->
                  verboTipoj |> Set.contains vorttipo
               | Nebazo(_, _, _) -> failwith "ne valida unua ŝtupo"
            | [] -> failwith "ne valida ŝtupoj")

   let ĉuVerboInfinitivo (ĉeno: string) =
      tuteMalinflekti ĉeno
      |> Result.map (fun ŝtupoj ->
            match ŝtupoj with
            | [ solaŜtupo ] ->
               match solaŜtupo with
               | Bazo(vorttipo, inflekcio, _) ->
                  (inflekcio = Infinitivo) && verboTipoj |> Set.contains vorttipo
               | _ -> false
            | _ -> false)

   let ĉuInfinitivoB (ĉeno: string) =
      ĉuInfinitivo ĉeno |> Option.isSome

   let ĉuVortaraFormo (ĉeno: string) =
      ĉuInfinitivoB ĉeno || ĉuLokokupilo ĉeno || ĉuFremdaVorto ĉeno

   let ĉuVerboInfinitivoB (ĉeno: string) =
      match ĉuVerboInfinitivo ĉeno with
      | Ok(rezulto) -> rezulto
      | Error(_) -> false

   let bazoDe (vorto: string) =
      if ĉuVerboInfinitivoB vorto then
         vorto.Substring
            (0,
             (if vorto.EndsWith("sh") then vorto.Length - 2
              else vorto.Length - 1))
      else
         vorto

   let ĉuMalplenigita (malplenigita: Vorttipo) (originala: string) =
      ĉuInfinitivo originala
      |> Option.filter (fun originalaTipo -> Set.contains originalaTipo verboTipoj)
      |> Option.bind (fun originalaTipo -> Map.tryFind originalaTipo malplenigeblaVerboTipoj)
      |> Option.filter (fun malplenigeblaTipoj -> Set.contains malplenigita malplenigeblaTipoj)
      |> Option.isSome