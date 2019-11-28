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
      |> List.map (fun (vorto, tipo, inflekcio) -> this.KontroliFormon vorto tipo inflekcio)
      |> ignore

   [<TestMethod>]
   member this.MalinflektiTesto1 () =
      [ ("kresku", [ (NombrigeblaKlaso, Infinitivo) ], "kresku")
        ("kreski", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, NekonitaNombro) ], "kresku")
        ("kreskisi", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, UnuNombro) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            this.KontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore

   [<TestMethod>]
   member this.MalinflektiTesto2 () =
      [ ("kreskuwa", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, PredikativoEsti) ], "kresku")
        ("kreskuga", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, AtributativoEsti) ], "kresku")
        ("kreskutra", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, PredikativoHavi) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            this.KontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore

   [<TestMethod>]
   member this.MalinflektiTesto3 () =
      [ ("kreskutre", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, AtributativoHavi) ], "kresku")
        ("kreskilas", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, Translativo) ], "kresku")
        ("kreskiva", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, Ĝerundo) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            this.KontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore

   member _.KontroliFormon (vorto: string) (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))

   member _.KontroliInflekcioj (vorto: string) (pravaInflekcioj: Vortformo list) (pravaMalinflektitaVorto: string) =
      malinflekti vorto
      |> Option.map
            (fun (formoj, malinflektitaVorto) ->
               Assert.AreEqual(pravaInflekcioj, formoj)
               Assert.AreEqual(pravaMalinflektitaVorto, malinflektitaVorto))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "ne povis malinflekti %s" vorto))