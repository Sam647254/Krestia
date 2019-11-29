namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Trakilaro
open KrestiaVortilo.Vorttipo

module Testiloj =
   let kontroliFormon (vorto: string) (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))

   let kontroliInflekcioj (vorto: string) (pravaInflekcioj: Vortformo list) (pravaMalinflektitaVorto: string) =
      malinflekti vorto
      |> Option.map
            (fun (formoj, malinflektitaVorto) ->
               Assert.AreEqual(pravaInflekcioj, formoj)
               Assert.AreEqual(pravaMalinflektitaVorto, malinflektitaVorto))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "ne povis malinflekti %s" vorto))