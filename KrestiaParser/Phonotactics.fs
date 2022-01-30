module KrestiaParser.Phonotactics

open System

type Letter =
   | Consonant of char
   | Vowel of char

let normalize (str: string) =
   str.Replace("aa", "ɒ").Replace("sh", "ʃ").ToLower()

let toLetter (ch: char) =
   match ch with
   | 'a' | 'e' | 'i' | 'o'| 'u' | 'ɒ' -> Vowel ch
   | _ -> Consonant ch

let dividi (str: string) =
   let normaligita = normalize str
   let rec splitAcc ĉuKomenca (literoj: Letter list): Result<string list, string> =
      match literoj with
      // CCVC
      | Consonant k1 :: Consonant k2 :: Vowel v :: Consonant kf :: Consonant kk2 :: restantaj ->
         if ĉuKomenca then
            splitAcc false (Consonant(kk2) :: restantaj)
            |> Result.map (fun restantajSilaboj ->
                  String.Concat([ k1; k2; v; kf ])
                  :: restantajSilaboj)
         else
            Error "Vorto ne rajtas komenci per du Consonantj"
      | [ Consonant k1; Consonant k2; Vowel v; Consonant kf ] ->
         if ĉuKomenca
         then [ String.Concat([ k1; k2; v; kf ]) ] |> Ok
         else Error "Vorto ne rajtas komenci per du Consonantj"
      // CCV
      | Consonant k1 :: Consonant k2 :: Vowel v :: Consonant kf :: Vowel v2 :: restantaj ->
         if ĉuKomenca then
            splitAcc false (Consonant(kf) :: Vowel(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ k1; k2; v ]) :: restantajSilaboj)
         else
            Error "Vorto ne rajtas komenci per du Consonantj"
      | Consonant k1 :: Consonant k2 :: Vowel v :: Vowel v2 :: restantaj ->
         if ĉuKomenca then
            splitAcc false (Vowel(v2) :: restantaj)
            |> Result.map (fun restantajSilaboj -> String.Concat([ k1; k2; v ]) :: restantajSilaboj)
         else
            Error "Vorto ne rajtas komenci per du Consonantj"
      | [ Consonant k1; Consonant k2; Vowel v ] ->
         if ĉuKomenca
         then [ String.Concat([ k1; k2; v ]) ] |> Ok
         else Error "Vorto ne rajtas komenci per du Consonantj"
      // CVC
      | Consonant k1 :: Vowel v :: Consonant kf :: Consonant kk2 :: restantaj ->
         splitAcc false (Consonant(kk2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v; kf ]) :: restantajSilaboj)
      | [ Consonant k1; Vowel v; Consonant kf ] -> [ String.Concat([ k1; v; kf ]) ] |> Ok
      // CV
      | Consonant k1 :: Vowel v :: Consonant kk2 :: Vowel v2 :: restantaj ->
         splitAcc false (Consonant(kk2) :: Vowel(v2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v ]) :: restantajSilaboj)
      | Consonant k1 :: Vowel v :: Vowel v2 :: restantaj ->
         splitAcc false (Vowel(v2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> String.Concat([ k1; v ]) :: restantajSilaboj)
      | [ Consonant k1; Vowel v ] -> [ String.Concat([ k1; v ]) ] |> Ok
      // VC
      | Vowel v :: Consonant kf :: Consonant kk2 :: restantaj ->
         splitAcc false (Consonant(kk2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> String.Concat([ v; kf ]) :: restantajSilaboj)
      | [ Vowel v; Consonant kf ] -> [ String.Concat([ v; kf ]) ] |> Ok
      // V
      | Vowel v :: Consonant kf :: Vowel v2 :: restantaj ->
         splitAcc false (Consonant(kf) :: Vowel(v2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
      | Vowel v :: Vowel v2 :: restantaj ->
         splitAcc false (Vowel(v2) :: restantaj)
         |> Result.map (fun restantajSilaboj -> v.ToString() :: restantajSilaboj)
      | [ Vowel v ] -> [ v.ToString() ] |> Ok
      // Eraro
      | [] -> Ok []
      | _ -> Error $"Cannot split %A{literoj}"
   
   splitAcc
      true
      (normaligita.ToCharArray()
       |> List.ofArray
       |> List.map toLetter)

let isValidWord word = failwith "???"