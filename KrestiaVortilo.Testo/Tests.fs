namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Trakilaro

[<TestClass>]
type VorttipoTestoj () =

   [<TestMethod>]
   member this.KontroliTesto () =
      [ ("kresku", NombrigeblaKlaso, Infinitivo)
        ("kreski", NombrigeblaKlaso, NekonitaNombro)
        ("gremu", NenombrigeblaKlaso, Infinitivo)
        ("lugrismaa", NenombrigeblaKlaso, Infinitivo)
        ("lugrismava", NenombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> this.KontroliFormon(vorto, tipo, inflekcio))
      |> ignore

   [<TestMethod>]
   member _.MalinflektiTesto () =
      [ ("kresku", [ (NombrigeblaKlaso, Infinitivo) ])
        ("kreski", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, NekonitaNombro) ]) ]
      |> List.map (fun (inflektitaVorto, pravaListo) ->
            malinflekti inflektitaVorto
            |> Option.map
                  (fun (formoj, _) ->
                     Assert.AreEqual(pravaListo, formoj))
            |> Option.defaultWith (fun () -> Assert.Fail()))
      |> ignore

   member _.KontroliFormon (vorto: string, pravaTipo: Vorttipo, pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))