namespace KrestiaVortilo.Testo

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipoj

[<TestClass>]
type VorttipoTestoj () =

   [<TestMethod>]
   member this.VorttipojTesto () =
      [ ("kresku", NombrigeblaKlaso, Infinitivo)
        ("kreski", NombrigeblaKlaso, NekonitaNombro)
        ("gremu", NenombrigeblaKlaso, Infinitivo)
        ("lugrismaa", NenombrigeblaKlaso, Infinitivo)
        ("lugrismava", NenombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> this.KontroliFormon(vorto, tipo, inflekcio))
      |> ignore

   member _.KontroliFormon (vorto: string, pravaTipo: Vorttipo, pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))