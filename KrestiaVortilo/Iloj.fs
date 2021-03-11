namespace KrestiaVortilo

module Iloj =
   type RezultoBuilder() =
      member _.Bind(rezulto, funkcio) = Result.bind funkcio rezulto
      
      member _.Return valuo = Ok valuo
   
   let rezulto = RezultoBuilder()