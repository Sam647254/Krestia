namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Traktilaro
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Legiloj
open KrestiaVortilo.Strukturo

module Testiloj =
   let kontroliFormon (vorto: string) (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))

   let kontroliFrazon (prava: Frazo) (frazo: string) =
      [ "evilel" ]
      |> set
      |> legiFrazon (frazo.Split(' ') |> List.ofArray) None
      |> Result.map
         (fun (rezulto, restantaj, _) ->
            Assert.AreEqual(prava, rezulto)
            Assert.AreEqual(0, restantaj.Length))
      |> Result.mapError (fun rezulto -> Assert.Fail(rezulto))
      |> ignore