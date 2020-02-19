namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Strukturo
open Testiloj

[<TestClass>]
type StrukturoTestoj () =

   [<TestMethod>]
   member _.``meratre hem hes`` () =
      let pravo =
         Predikato2(
            { Vorto = "merat"; Inflekcio = [ Progresivo ] },
            Lokokupilo("hem"),
            Lokokupilo("hes"))
         |> Predikato
      kontroliFrazon pravo "meratre hem hes"

   [<TestMethod>]
   member _.``menapro Eşxan trupasi wil`` () =
      let pravo =
         Predikato3(
            { Vorto = "menap"; Inflekcio = [ Perfekto ] },
            FremdaVorto("Eşxan"),
            BazaVorto({ Vorto = "trupaa"; Inflekcio = [ UnuNombro ] }),
            Lokokupilo("wil"))
         |> Predikato
      kontroliFrazon pravo "menapro Eşxan trupasi wil"

   [<TestMethod>]
   member _.``peral meratre heti megre kunapasi kredea meratre heti kitigrensa`` () =
      let pravo =
         Peral(
            Predikato2(
               { Vorto = "merat"; Inflekcio = [ Progresivo ]},
               Lokokupilo("heti"),
               Eco(
                  { Vorto = "megro"; Inflekcio = [ NekonitaNombro ]},
                  PridiritaVorto(
                     BazaVorto({ Vorto = "kunapaa"; Inflekcio = [ UnuNombro ]}),
                     [ Pridiranto("kred") ]))),
            Predikato2(
               { Vorto = "merat"; Inflekcio = [ Progresivo ] },
               Lokokupilo("heti"),
               BazaVorto({ Vorto = "kitigro"; Inflekcio = [ Havaĵo ]})))
      kontroliFrazon
         pravo
         "peral meratre heti megre kunapasi kredea meratre heti kitigrensa"

   [<TestMethod>]
   member _.``petro Naapi bivitulasemema heti`` () =
      let pravo =
         Predikato2(
            { Vorto = "pet"; Inflekcio = [ Perfekto ]},
            FremdaVorto("Naapi"),
            BazaVorto({ Vorto = "bivitu"
                        Inflekcio = [ Translativo
                                      PartaNominativo(Lokokupilo("heti"))
                                      InflekcioŜtupo.Ĝerundo ] }))
         |> Predikato
      kontroliFrazon
         pravo
         "petro Naapi bivitilasemena heti"

   [<TestMethod>]
   member _.``remudia remudia endre rima`` () =
      let pravo =
         Predikato1(
            { Vorto = "remud"; Inflekcio = [ PredikativoEsti ]},
            Eco(
               { Vorto = "endro"; Inflekcio = [ NekonitaNombro ]},
               BazaVorto({ Vorto = "rimaa"; Inflekcio = [ NekonitaNombro ]})))
         |> Predikato
      kontroliFrazon
         pravo
         "remudia remudia endre rima"

   [<TestMethod>]
   member _.``retodia retodia tunipe vol durana`` () =
      let pravo =
         Predikato1(
            { Vorto = "retod"; Inflekcio = [ PredikativoEsti ]},
            PridiritaVorto(
               BazaVorto({ Vorto = "tunipo"; Inflekcio = [ NekonitaNombro ]}),
               [ Modifanto1("vol", BazaVorto({ Vorto = "duranaa"; Inflekcio = [ NekonitaNombro ]})) ]))
         |> Predikato
      kontroliFrazon
         pravo
         "retodia retodia tunipe vol durana"