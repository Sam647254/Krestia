namespace KrestiaVortilo

module Vorttipo =
   type Inflekcio =
   | Infinitivo
   | NekonitaNombro
   | UnuNombro
   | Havaĵo
   | PluraNombro
   | Progresivo
   | Perfekto
   | Estonteco
   | NominativoVolo
   | AkuzativoVolo
   | DativoVolo
   | PredikativoEsti
   | AtributativoEsti
   | PredikativoHavi
   | AtributativoHavi
   | Imperativo
   | Patiento
   | Aganto
   | Translativo
   | Ĝerundo
   | PartaNominativo
   | PartaAkuzativo
   | PartaDativo
   | Pasivigo
   | Igo
   | SolaFormo

   type Vorttipo =
   | NombrigeblaKlaso
   | NenombrigeblaKlaso
   | Rekordo
   | NombrigeblaEco
   | NenombrigeblaEco
   | TransitivaVerbo
   | NetransitivaVerbo
   | MalplenaVerbo
   | PartaTransitivaVerbo
   | PartaNetransitivaVerbo
   | Pridiranto
   | Lokokupilo

   type Vortformo = Vorttipo * Inflekcio
   type VorttipoKontrolilo = (string -> bool)
   type Vorttraktilo = {
      Formo : Vortformo
      Kontroli : (string -> bool)
      Inflekti : (Vortformo -> string -> string option)
      Malinflekti : (string -> (string * Vortformo))
   }

   let neinflektebla = fun _formo _vorto -> None

   let _kontrolilaro: (Vortformo * VorttipoKontrolilo) list = [
      ((NombrigeblaKlaso, Infinitivo),
         fun s ->
            [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa"]
            |> List.exists (fun finaĵo -> s.EndsWith(finaĵo)))
      ((NombrigeblaKlaso, NekonitaNombro),
         fun s ->
            [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka"]
            |> List.exists (fun finaĵo -> s.EndsWith(finaĵo)))

      ((NenombrigeblaKlaso, Infinitivo),
         fun s ->
            [ "mu"; "mo"; "maa"; "nu"; "no"; "naa"]
            |> List.exists (fun finaĵo -> s.EndsWith(finaĵo)))

      ((NenombrigeblaKlaso, Ĝerundo),
         fun s ->
            [ "miva"; "meva"; "mava"; "niva"; "neva"; "nava"]
            |> List.exists (fun finaĵo -> s.EndsWith(finaĵo)))
   ]