namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =
   type Vorto() =
      class
      end

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
               |> Result.bind (fun argumento ->
                     argumentoj.AddLast(argumento) |> ignore
                     this.LegiSekvan())
            elif ĉuAntaŭEco sekva then
               this.LegiArgumenton()
               |> Result.bind (fun eco ->
                     this.LegiArgumenton()
                     |> Result.bind (fun de ->
                           argumentoj.AddLast(Eco(eco, de)) |> ignore
                           this.LegiSekvan()))
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let argumento = enira.Dequeue()
         if not (ĉuArgumentaVorto argumento)
         then Error
                 (Eraro
                    (argumento.OriginalaVorto, sprintf "%s is not a valid argument" argumento.OriginalaVorto.Vorto))
         else failwith "???"