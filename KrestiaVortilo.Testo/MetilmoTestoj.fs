namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Sintaksanalizilo2
open Testiloj

[<TestClass>]
type MetilmoTestoj() =
   [<TestMethod>]
   member _.Unu() =
      let [ hime; deletro; linetiga; rone; _; merogia; _; _; mite; hel; teretro; ete; mirateva; delevio ] =
         [ "hime"
           "deletro"
           "linetiga"
           "rone"
           "nomil"
           "merogia"
           "nivoral"
           "sivil"
           "mite"
           "hel"
           "teretro"
           "ete"
           "mirateva"
           "delevio" ]
         |> List.map praveMalinflekti

      kontroliFrazojn "hime deletro linetiga rone
nomil merogia nivoral sivil
mite hel teretro ete
mirateva delevio rone"
         [ { Kapo =
                verbo
                   deletro
                   [ Pridiranto(argumento linetiga [])
                     Nomil
                        ({ Kapo = verbo merogia []
                           Argumentoj =
                              [ Mite
                                 (mite,
                                  { Kapo = verbo teretro []
                                    Argumentoj =
                                       [ argumento hel []
                                         Ete
                                            (ete,
                                             { Kapo = verbo delevio [ Pridiranto(argumento mirateva []) ]
                                               Argumentoj = [ argumento rone [] ] }) ] }) ] }) ]
             Argumentoj = [ argumento hime []; argumento rone [] ] } ]
