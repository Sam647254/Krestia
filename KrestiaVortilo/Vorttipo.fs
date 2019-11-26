namespace KrestiaVortilo

module Vorttipoj =
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
   | Patiento
   | Aganto
   | Translativo
   | Ĝerundo

   type Vorttipo =
   | NombrigeblaKlaso
   | NenombrigeblaKlaso
   | TransitivaVerbo
   | NetransitivaVerbo
   | Priskribanto
   | Lokokupilo

   type Vortformo = Vorttipo * Inflekcio
   type VorttipoKontrolilo = (string -> bool)

   let kontrolilaro: (Vortformo * VorttipoKontrolilo) list = [
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

   let kontroli (vorto: string): Vortformo option =
      kontrolilaro
      |> List.tryFind (fun (_, kontrolilo) -> (kontrolilo vorto))
      |> Option.map (fun (formo, _) -> formo)
      