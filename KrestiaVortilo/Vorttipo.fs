namespace KrestiaVortilo

module Vorttipo =
   type Inflekcio =
   | Infinitivo
   | NekonitaNombro
   | UnuNombro
   | PluraNombro
   | Progresivo
   | Perfekto
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

   type Vorttipo =
   | NombrigeblaKlaso
   | NenombrigeblaKlaso
   | TransitivaVerbo
   | NetransitivaVerbo
   | MalplenaVerbo
   | PartaTransitivaVerbo
   | PartaNetransitivaVerbo
   | Priskribanto
   | Lokokupilo

   type Vortformo = Vorttipo * Inflekcio
   type VorttipoKontrolilo = (string -> bool)
   type Vorttraktilo = {
      Formo : Vortformo
      Kontroli : (string -> bool)
      Inflekti : (Vortformo -> string -> string)
      Malinflekti : (string -> (string * Vortformo))
   }

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