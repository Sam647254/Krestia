namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo
open Testiloj

// [<TestClass>]
type LearnTheseWordsFirstTestoj () =

   [<TestMethod>]
   member _.``Tony sees Lisa.`` () =
      let pravo =
         Predikato2(
            { Vorto = "merat"; Inflekcio = [ Progresivo ]},
            FremdaVorto("Tony"),
            FremdaVorto("Lisa"))
         |> Predikato
      kontroliFrazon pravo "meratre Tony Lisa"

   [<TestMethod>]
   member _.``Tony sees something.`` () =
      let pravo =
         Predikato2(
            { Vorto = "merat"; Inflekcio = [ Progresivo ]},
            FremdaVorto("Tony"),
            Lokokupilo("wel"))
         |> Predikato
      kontroliFrazon pravo "meratre Tony wel"

   [<TestMethod>]
   member _.``Tony sees the other thing.`` () =
      let pravo =
         Predikato2(
            { Vorto = "merat"; Inflekcio = [ Progresivo ]},
            FremdaVorto("Tony"),
            PridiritaVorto(Lokokupilo("wel"), [ Modifanto0("evilel") ]))
         |> Predikato
      kontroliFrazon pravo "meratre Tony wel evilel"

   [<TestMethod>]
   member _.``Tony sees this thing.`` () =
      let pravo =
         Predikato2(
            { Vorto = "merat"; Inflekcio = [ Progresivo ]},
            FremdaVorto("Tony"),
            PridiritaVorto(Lokokupilo("wel"), [ Pridiranto("tred") ]))
         |> Predikato
      kontroliFrazon pravo "meratre Tony wel tredea"