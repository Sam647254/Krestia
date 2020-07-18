namespace KrestiaVortilo

open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Vorttipo

module Valenco =
   type MultaVorto =
      { AntaŭValenco : int
        MalantaŭValenco : int }
      
   let multaVorto antaŭ malantaŭ = { AntaŭValenco = antaŭ; MalantaŭValenco = malantaŭ }
   
   let multaVortoValenco vorttipo inflekcio =
      match vorttipo with
      | NombrigeblaKlaso | NenombrigeblaKlaso ->
         match inflekcio with
         | AtributivoEstiAntaŭ -> multaVorto 0 1
         | AtributivoEstiMalantaŭ -> multaVorto 1 0
         | _ -> multaVorto 0 0
      | MalantaŭNombrigeblaEco | MalantaŭNenombrigeblaEco ->
         match inflekcio with
         | Havaĵo | UnuHavaĵo | PluraHavaĵo -> multaVorto 0 0
         | _ -> multaVorto 1 0
      | Pridiranto ->
         match inflekcio with
         | AtributivoEstiAntaŭ -> multaVorto 0 1
         | AtributivoEstiMalantaŭ -> multaVorto 1 0
         | _ -> multaVorto 0 0
      | _ -> multaVorto 0 0

   let valencoDe (vorto: MalinflektitaVorto): Result<int, string> =
      failwith "???"