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
      
      member this.AldoniModifanton(modifanto: Modifanto) =
         this.modifantoj.Add(modifanto) |> ignore

   and Argumento(kapo: MalinflektitaVorto) =
      inherit ModifeblaVorto()
   
   and Rezulto =
      { Frazoj : Predikato list
        Argumentoj: Argumento list }
   
   and Modifanto =
      | EcoDe of Argumento

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let frazoj = List<Predikato>()

      member this.Legi(): Result<Rezulto, Eraro> =
         this.LegiSekvan()
         |> Result.map (fun () ->
            { Frazoj = frazoj |> List.ofSeq
              Argumentoj = argumentoj |> List.ofSeq })

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
                           eco.AldoniModifanton(EcoDe(de))
                           argumentoj.AddLast(eco) |> ignore
                           this.LegiSekvan()))
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()
            
      member private this.PostuliArgumenton(): Result<Argumento, Eraro> =
         if argumentoj.Count > 0 then
            let argumento = argumentoj.First.Value
            argumentoj.RemoveFirst()
            Ok(argumento)
         else
            this.LegiArgumenton()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let argumento = enira.Dequeue()
         if not (ĉuArgumentaVorto argumento)
         then Error
                 (Eraro
                    (argumento.OriginalaVorto, sprintf "%s is not a valid argument" argumento.OriginalaVorto.Vorto))
         else
            Ok(Argumento(argumento))