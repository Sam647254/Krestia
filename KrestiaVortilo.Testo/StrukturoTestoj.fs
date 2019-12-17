namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo
open Testiloj

[<TestClass>]
type StrukturoTestoj () =

   [<TestMethod>]
   member _.``meratre hem hes`` () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            Lokokupilo("hem"),
            Lokokupilo("hes"))
      kontroliStrukturon pravo "meratre hem hes" (fun _ -> Some(2))

   [<TestMethod>]
   member _.``menapro Eşxan trupasi wil`` () =
      let pravo =
         Predikato3(
            { Verbo = "menap"; Inflekcio = Perfekto },
            FremdaVorto("Eşxan"),
            Objekto({ Objekto = "trupaa"; Inflekcio = UnuNombro}),
            Lokokupilo("wil"))
      kontroliStrukturon pravo "menapro Eşxan trupasi wil" (fun _ -> Some(3))