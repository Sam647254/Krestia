namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo
open Testiloj

[<TestClass>]
type LearnTheseWordsFirstTestoj () =

   [<TestMethod>]
   member _.``Tony sees Lisa.`` () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            FremdaVorto("Tony"),
            FremdaVorto("Lisa"))
      kontroliStrukturon pravo "meratre Tony Lisa" (fun _ -> Some(2))

   [<TestMethod>]
   member _.``Tony sees something.`` () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            FremdaVorto("Tony"),
            Lokokupilo("wel"))
      kontroliStrukturon pravo "meratre Tony wel" (fun _ -> Some(2))

   [<TestMethod>]
   member _.``Tony sees the other thing.`` () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            FremdaVorto("Tony"),
            PridiritaVorto(Lokokupilo("wel"), [ Modifanto0("ilel") ]))
      kontroliStrukturon pravo "meratre Tony wel ilel" (fun _ -> Some(2))

   [<TestMethod>]
   member _.``Tony sees this thing.`` () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            FremdaVorto("Tony"),
            PridiritaVorto(Lokokupilo("wel"), [ Modifanto0("tred") ]))
      kontroliStrukturon pravo "meratre Tony wel tredea" (fun _ -> Some(2))