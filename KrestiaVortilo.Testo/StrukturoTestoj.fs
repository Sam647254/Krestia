namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo

[<TestClass>]
type StrukturoTestoj () =

   [<TestMethod>]
   member _.Test1 () =
      let pravo =
         Predikato2(
            { Verbo = "merat"; Inflekcio = Progresivo },
            Lokokupilo("hem"),
            Lokokupilo("hes"))
      strukturigi "meratre hem hes" (fun _ -> Some(2))
      |> Result.map (fun rezulto -> Assert.AreEqual(pravo, rezulto))
      |> Result.mapError (fun eraro -> Assert.Fail(eraro))
      |> ignore