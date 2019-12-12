namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo

[<TestClass>]
type StrukturoTestoj () =

   [<TestMethod>]
   member _.Test1 () =
      Assert.AreEqual(
         (Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            Lokokupilo("hem"),
            Lokokupilo("hes")))
         |> Ok,
         strukturigi "meratre hem hes")