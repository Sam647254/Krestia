namespace KrestiaVortilo

open KrestiaVortilo.Malinflektado

module Valenco =
   type Valenco =
      { AntaŭValenco : int
        MalantaŭValenco : int }

   let valencoDe (vorto: MalinflektitaVorto): Result<Valenco, string> =
      failwith "???"