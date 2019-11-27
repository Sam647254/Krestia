namespace KrestiaVortilo

open Vorttipo

module KlasoTrakiloj =
   let NekAlInf = [
      ('i', "u")
      ('e', "o")
      ('a', "aa") ] |> Map.ofList

   let trakiloj: Vorttraktilo list = [
      { Formo = (NombrigeblaKlaso, Infinitivo)
        Kontroli = fun vorto ->
          [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa"; "etio"; "oniaa" ]
          |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (NombrigeblaKlaso, Infinitivo)) }

      { Formo = (NombrigeblaKlaso, NekonitaNombro)
        Kontroli = fun s ->
           [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka"; "etie"; "onia" ]
           |> List.exists (fun finaĵo -> s.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1) + NekAlInf.[vorto.Chars(vorto.Length - 1)],
              match vorto with 
              | v when v.EndsWith("setie") -> (NetransitivaVerbo, Aganto)
              | v when
                 [ "petie"; "tetie"; "ketie" ]
                 |> List.exists (fun finaĵo -> v.EndsWith(finaĵo)) -> (TransitivaVerbo, Aganto)
              | v when
                 [ "ponia"; "tonia"; "konia" ]
                 |> List.exists (fun finaĵo -> v.EndsWith(finaĵo)) -> (TransitivaVerbo, Patiento)
              | _ -> (NombrigeblaKlaso, Infinitivo)) }
      ]