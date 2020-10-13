namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Sintaksanalizilo2
open Testiloj

[<TestClass>]
type MetilmoTestoj() =
   [<TestMethod>]
   member _.Unu() =
      let [ hime; deletro; linetiga; rone; nomil; merogia; nivoral; sivil; mine; hel; teretro; ene; mirateva; delegio ] =
         [ "hime"
           "deletro"
           "linetiga"
           "rone"
           "nomil"
           "merogia"
           "nivoral"
           "sivil"
           "mine"
           "hel"
           "teretro"
           "ene"
           "mirateva"
           "delegio" ]
         |> List.map praveMalinflekti

      kontroliFrazojn "hime deletro linetiga rone
nomil merogia nivoral sivil
mine hel teretro ene
mirateva delegio rone"
         [ { Kapo =
                verbo
                   deletro
                   [ Pridiranto(argumento linetiga [])
                     ModifantoKunFrazo
                        (nomil,
                         { Kapo = verbo merogia [ modifanto nivoral; modifanto sivil ]
                           Argumentoj =
                              [ argumento
                                 mine
                                   [ Mine
                                      ({ Kapo = verbo teretro []
                                         Argumentoj =
                                            [ argumento hel []
                                              argumento
                                                 ene
                                                 [ Ene
                                                    ({ Kapo = verbo delegio [ Pridiranto(argumento mirateva []) ]
                                                       Argumentoj = [ argumento rone [] ] }) ] ] }) ] ] }) ]
             Argumentoj = [ argumento hime []; argumento rone [] ] } ]

   [<TestMethod>]
   member _.Du() =
      let [ hime; rilitro; keni; prenta; meritetiega; mine; gremegrela; borol; relos; kerel; segrerem; melismea ] =
         [ "hime"
           "rilitro"
           "keni"
           "prenta"
           "meritetiega"
           "mine"
           "gremegrela"
           "borol"
           "relos"
           "kerel"
           "segrerem"
           "melismea" ]
         |> List.map praveMalinflekti

      kontroliFrazojn "hime rilitro keni
prenta meritetiega
mine gremegrela borol relos
kerel segrerem melismea"
         [ { Kapo = verbo rilitro []
             Argumentoj =
                [ argumento hime []
                  argumento
                     keni
                     [ Keni
                        (argumento prenta [ Pridiranto(argumento meritetiega []) ],
                         argumento
                            mine
                            [ Mine
                               ({ Kapo =
                                     verbo
                                        relos
                                        [ ModifantoKunFrazo
                                           (kerel,
                                            { Kapo = verbo segrerem [ EcoDe(argumento melismea []) ]
                                              Argumentoj = [] }) ]
                                  Argumentoj = [ argumento gremegrela [ modifanto borol ] ] }) ]) ] ] } ]

   [<TestMethod>]
   member _.Tri() =
      let [ hime; glatela; mevekinsa; kerel; liseregela; gremensa; nel; kriteva; ponakava; lumiteva; eleteva; grema ] =
         [ "hime"
           "glatela"
           "mevekinsa"
           "kerel"
           "liseregela"
           "gremensa"
           "nel"
           "kriteva"
           "ponakava"
           "lumiteva"
           "elateva"
           "grema" ]
         |> List.map praveMalinflekti

      kontroliFrazojn "hime glatela mevekinsa
kerel liseregela gremensa nil
nel kriteva ponakava
lumiteva elateva grema"
         [ { Kapo =
                verbo
                   glatela
                   [ ModifantoKunFrazo
                      (kerel,
                       { Kapo =
                            verbo
                               liseregela
                               [ ModifantoKunArgumentoj
                                  (nel,
                                   [ argumento
                                      grema
                                        [ pridiranto kriteva
                                          pridiranto ponakava
                                          pridiranto lumiteva
                                          pridiranto eleteva ] ]) ]
                         Argumentoj = [ argumento gremensa [] ] }) ]
             Argumentoj =
                [ argumento hime []
                  argumento mevekinsa [] ] } ]

   [<TestMethod>]
   member _.Kvar() =
      let [ hime; lipro; rone; metilmo; kerel; gelemela; ponel; pini; het; hal; lukrenega; likrenega; rinomega; seskoma;
            vol; sonol ] =
         [ "hime"
           "lipro"
           "rone"
           "Metilmo"
           "kerel"
           "gelemela"
           "ponel"
           "pini"
           "het"
           "hal"
           "lukrenega"
           "likrenega"
           "rinomega"
           "seskoma"
           "vol"
           "sonol" ]
         |> List.map praveMalinflekti

      kontroliFrazojn "hime lipro rone Metilmo
kerel gelemela ponel pini vol het
hal lukrenega hal likrenega
hal rinomega sonol seskoma"
         [ { Kapo =
                verbo
                   lipro
                   [ ModifantoKunFrazo
                      (kerel,
                       { Kapo =
                            verbo
                               gelemela
                               [ ModifantoKunArgumentoj
                                  (ponel,
                                   [ argumento
                                      pini
                                        [ Pini
                                           (argumento hal [ pridiranto lukrenega ],
                                            argumento hal [ pridiranto likrenega ],
                                            argumento
                                               hal
                                               [ Pridiranto
                                                  (argumento
                                                     rinomega
                                                      [ ModifantoKunArgumentoj(sonol, [ argumento seskoma [] ]) ]) ]) ] ]) ]
                         Argumentoj = [] }) ]
             Argumentoj =
                [ argumento hime []
                  argumento rone []
                  argumento metilmo [] ] } ]
