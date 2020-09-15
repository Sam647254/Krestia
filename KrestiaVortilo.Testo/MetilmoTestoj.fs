namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Sintaksanalizilo2
open Testiloj

[<TestClass>]
type MetilmoTestoj() =
   [<TestMethod>]
   member _.Unu() =
      let [ hime; deletro; linetiga; rone; _; merogia; _; _; mine; hel; teretro; ene; mirateva; delegio ] =
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
                     Nomil
                        ({ Kapo = verbo merogia [ Nivoral; Sivil ]
                           Argumentoj =
                              [ Mine
                                 (mine,
                                  { Kapo = verbo teretro []
                                    Argumentoj =
                                       [ argumento hel []
                                         Ene
                                            (ene,
                                             { Kapo = verbo delegio [ Pridiranto(argumento mirateva []) ]
                                               Argumentoj = [ argumento rone [] ] }) ] }) ] }) ]
             Argumentoj = [ argumento hime []; argumento rone [] ] } ]

   [<TestMethod>]
   member _.Du() =
      let [ hime; rilitro; keni; prenta; meritetiega; mine; gremegrela; relos; segrerem; melismea ] =
         [ "hime"
           "rilitro"
           "keni"
           "prenta"
           "meritetiega"
           "mine"
           "gremegrela"
           "relos"
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
                  Keni
                     (keni,
                      argumento prenta [ Pridiranto(argumento meritetiega []) ],
                      Mine
                         (mine,
                          { Kapo =
                               verbo
                                  relos
                                  [ Kerel
                                     ({ Kapo = verbo segrerem [ EcoDe(argumento melismea []) ]
                                        Argumentoj = [] }) ]
                            Argumentoj = [ argumento gremegrela [ Borol ] ] })) ] } ]

   [<TestMethod>]
   member _.Tri() =
      let [ hime; glatela; mevekinsa; liseregela; gremensa; kriteva; ponakava; lumiteva; eleteva; grema ] =
         [ "hime"
           "glatela"
           "mevekinsa"
           "liseregela"
           "gremensa"
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
                   [ Kerel
                      ({ Kapo =
                            verbo
                               liseregela
                               [ Nel
                                  (argumento
                                     grema
                                      [ pridiranto kriteva
                                        pridiranto ponakava
                                        pridiranto lumiteva
                                        pridiranto eleteva ]) ]
                         Argumentoj = [ argumento gremensa [] ] }) ]
             Argumentoj =
                [ argumento hime []
                  argumento mevekinsa [] ] } ]
