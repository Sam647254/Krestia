namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =
   type Vorto() =
      class end
   
   type ModifeblaVorto() =
      inherit Vorto()
      member private _.modifantoj = HashSet<Modifanto>()

   [<AbstractClass>]
   type Argumento() =
      inherit ModifeblaVorto()
      
   type PlenaArgumento(vorto: MalinflektitaVorto) =
      inherit Argumento()
      
   type Eco(eco: Argumento, de: Argumento) =
      inherit Argumento()

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let frazoj = List<Predikato>()

      member this.Legi(): Result<Analizejo, Eraro> =
         this.LegiSekvan()
         |> Result.map (fun () -> failwith "???")

      member this.LegiSekvan(): Result<unit, Eraro> =
         if enira.Count > 0 then
            let sekva = enira.Peek()
            if ĉuArgumentaVorto sekva then
               this.LegiArgumenton()
               this.LegiSekvan()
            elif ĉuAntaŭEco sekva then
               let eco = this.LegiArgumenton()
               let de = this.LegiArgumenton()
               argumentoj.AddLast(Eco(eco, de))
               this.LegiSekvan()
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton() =
         match restantajVortoj with
         | argumento :: restantaj ->
            if not (ĉuArgumentaVorto argumento)
            then Error
                    (Eraro
                       (argumento.OriginalaVorto, sprintf "%s is not a valid argument" argumento.OriginalaVorto.Vorto))
            elif ĉuAntaŭEco argumento then
               let ecoDe = this.LegiArgumenton(restantaj)
         | [] -> failwith "Argument expected"
