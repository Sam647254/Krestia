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
         |> Predikato
      kontroliFrazon pravo "meratre hem hes"

   [<TestMethod>]
   member _.``menapro Eşxan trupasi wil`` () =
      let pravo =
         Predikato3(
            { Verbo = "menap"; Inflekcio = Perfekto },
            FremdaVorto("Eşxan"),
            Objekto({ Objekto = "trupaa"; Inflekcio = UnuNombro}),
            Lokokupilo("wil"))
         |> Predikato
      kontroliFrazon pravo "menapro Eşxan trupasi wil"

   [<TestMethod>]
   member _.``peral meratre heti megre kunapasi kredea meratre heti kitigrensa`` () =
      let pravo =
         Peral(
            Predikato2(
               { Verbo = "merat"; Inflekcio = Progresivo },
               Lokokupilo("heti"),
               Eco(
                  { Objekto = "megro"; Inflekcio = NekonitaNombro },
                  PridiritaVorto(
                     Objekto({ Objekto = "kunapaa"; Inflekcio = UnuNombro }),
                     [ Pridiranto("kred") ]))),
            Predikato2(
               { Verbo = "merat"; Inflekcio = Progresivo },
               Lokokupilo("heti"),
               Objekto({ Objekto = "kitigro"; Inflekcio = Havaĵo })))
      kontroliFrazon
         pravo
         "peral meratre heti megre kunapasi kredea meratre heti kitigrensa"

   [<TestMethod>]
   member _.``remudia remudia endre rima`` () =
      let pravo =
         Predikato1(
            { Verbo = "remud"; Inflekcio = PredikativoEsti },
            Eco(
               { Objekto = "endro"; Inflekcio = NekonitaNombro },
               Objekto({ Objekto = "rimaa"; Inflekcio = NekonitaNombro })))
         |> Predikato
      kontroliFrazon
         pravo
         "remudia remudia endre rima"

   [<TestMethod>]
   member _.``retodia retodia tunipe vol durana`` () =
      let pravo =
         Predikato1(
            { Verbo = "retod"; Inflekcio = PredikativoEsti },
            PridiritaVorto(
               Objekto({ Objekto = "tunipo"; Inflekcio = NekonitaNombro }),
               [ Modifanto1("vol", Objekto({ Objekto = "duranaa"; Inflekcio = NekonitaNombro })) ]))
         |> Predikato
      kontroliFrazon
         pravo
         "retodia retodia tunipe vol durana"