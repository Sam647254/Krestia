namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =
   type ModifeblaVorto() =
      member private _.modifantoj = HashSet<Modifanto>()
   
   type Argumento(vorto: MalinflektitaVorto) =
      inherit ModifeblaVorto()
   
   type ImperitivaLegilo(enira: MalinflektitaVorto list) =
      let argumentoj = LinkedList<Argumento>
      let frazoj = List<Predikato>()
      
      member this.Legi(): Result<Analizejo, Eraro> =
         this.LegiSekvan(enira)
         |> Result.map (fun () -> failwith "???")
      
      member this.LegiSekvan(restantajVortoj: MalinflektitaVorto list): Result<unit, Eraro> =
         match restantajVortoj with
         | sekva :: _ ->
            if ĉuArgumentaVorto sekva then
               this.LegiArgumenton(restantajVortoj)
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         | [] -> Ok()
      
      member private _.LegiArgumenton(restantajVortoj) =
         failwith "???"