﻿namespace KrestiaVortilo

module Iloj =
   type RezultoBuilder() =
      member _.Bind(rezulto, funkcio) = Result.bind funkcio rezulto
      
      member _.Return valuo = Ok valuo
      
      member _.ReturnFrom valuo = valuo
      
      member _.Zero () = Ok ()
   
   let rezulto = RezultoBuilder()
   
   type OpcioBuilder() =
      member _.Bind(opcio, funkcio) = Option.bind funkcio opcio
      
      member _.Combine(opcio, alternativo) =
         match opcio with
         | Some _ -> opcio
         | None -> alternativo
      
      member _.Return valuo = Some valuo
      
      member _.ReturnFrom valuo = valuo
      
      member _.Delay f = f()
      
      member _.Zero () = None
   
   let opcio = OpcioBuilder()