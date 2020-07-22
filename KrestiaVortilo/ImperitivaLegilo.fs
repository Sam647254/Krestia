namespace KrestiaVortilo

open System.Collections.Generic
open KrestiaVortilo.Malinflektado
open KrestiaVortilo.Sintaksanalizilo2

module Imperativa =

   [<CustomEquality; NoComparison>]
   type ModifeblaVorto =
      { Kapo: MalinflektitaVorto
        Modifantoj: HashSet<Modifanto> }
      
      override this.Equals(alia) =
         match alia with
         | :? ModifeblaVorto as aliaVorto ->
            aliaVorto.Kapo = this.Kapo && aliaVorto.Modifantoj.SetEquals(this.Modifantoj)
         | _ -> false
         
      override this.GetHashCode() =
         hash this.Kapo + 17 * this.Modifantoj.GetHashCode()

   type Argumento =
      | PlenaArgumento of ModifeblaVorto
      | Eco of eco: Argumento * de: Argumento

   and Rezulto =
      { Frazoj: Predikato list
        Argumentoj: Argumento list }
      
   let plenaArgumento argumento =
      PlenaArgumento
         { Kapo = argumento
           Modifantoj = HashSet() }
         
   let plenaModifitaArgumento argumento (modifantoj: Modifanto seq) =
      PlenaArgumento
         { Kapo = argumento
           Modifantoj = HashSet(modifantoj) }

   type ImperitivaLegilo(enira: Queue<MalinflektitaVorto>) =
      let argumentoj = LinkedList<Argumento>()
      let frazoj = List<Predikato>()
      let atendantajPridirantoj = Queue<Modifanto>()
      let atendantajAntaŭajEcoj = Queue<Argumento>()
      let mutable lastaLegitaArgumento: Argumento option = None

      member this.Legi(): Result<Rezulto, Eraro> =
         this.LegiSekvan()
         |> Result.map (fun () ->
               { Frazoj = frazoj |> List.ofSeq
                 Argumentoj = argumentoj |> List.ofSeq })

      member private this.LegiSekvan(): Result<unit, Eraro> =
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
                     match eco with
                     | PlenaArgumento (argumento) ->
                        argumento.Modifantoj.UnionWith(atendantajPridirantoj :> IEnumerable<Modifanto>)
                     | _ -> ()
                     atendantajPridirantoj.Clear()
                     atendantajAntaŭajEcoj.Enqueue(eco)
                     lastaLegitaArgumento <- Some eco
                     this.LegiSekvan())
            elif ĉuAntaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               atendantajPridirantoj.Enqueue(pridiranto)
               this.LegiSekvan()
            elif ĉuMalantaŭModifantaVorto sekva then
               let pridiranto = this.LegiPridiranton()
               match lastaLegitaArgumento with
               | Some(a) ->
                  match a with
                  | PlenaArgumento(argumento) -> argumento.Modifantoj.Add(pridiranto) |> ignore
                  | _ -> failwith "Nur povas aldoni modifanton al PlenaArgumento"
               | None -> failwith "No argument to modify"
               this.LegiSekvan()
            else
               Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
               |> Error
         else
            Ok()

      member private this.LegiArgumenton(): Result<Argumento, Eraro> =
         let sekva = enira.Dequeue()
         if ĉuDifinitaKlaso sekva then
            this.LegiPlenanArgumenton(sekva) |> Ok
         elif ĉuAntaŭModifantaVorto sekva then
            atendantajPridirantoj.Enqueue(Pridiranto(sekva))
            this.LegiArgumenton()
         elif ĉuAntaŭEco sekva then
            let eco = plenaModifitaArgumento sekva atendantajPridirantoj
            lastaLegitaArgumento <- Some eco
            this.LegiArgumenton()
            |> Result.map (fun de -> Eco(eco, de))
         elif ĉuMalantaŭModifantaVorto sekva then
            failwith "???"
            this.LegiArgumenton()
         elif ĉuMalantaŭEco sekva then
            match lastaLegitaArgumento with
            | Some(a) ->
               match a with
               | PlenaArgumento(argumento) -> argumento.Modifantoj.Add(Pridiranto(sekva)) |> ignore
               | Eco(eco, de) ->
                  let novaArgumento = this.LegiPlenanArgumenton(sekva)
                  
            | None -> failwith "No argument to associate with"
            this.LegiArgumenton()
         else
            failwith "Unexpected input"
      
      member private this.LegiPlenanArgumenton(sekva) =
         let novaArgumento =
            let argumento = PlenaArgumento { Kapo = sekva; Modifantoj = HashSet(atendantajPridirantoj) }
            lastaLegitaArgumento <- Some argumento
            atendantajAntaŭajEcoj
            |> Seq.reduce (fun ak sek -> Eco(ak, sek))
            |> (fun ĉiujEcoj -> Eco(argumento, ĉiujEcoj))
         atendantajPridirantoj.Clear()
         atendantajAntaŭajEcoj.Clear()
         novaArgumento

      member private this.LegiPridiranton() = enira.Dequeue() |> Pridiranto
      
      member private this.LegiĈiujnMalantaŭajnModifantojn() =
         let modifantoj = HashSet()
         while enira.Count > 0 && ĉuMalantaŭModifantaVorto (enira.Peek()) do
            let sekva = enira.Dequeue()
            modifantoj.Add(Pridiranto(sekva)) |> ignore
         modifantoj