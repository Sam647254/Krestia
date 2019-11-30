Imports KrestiaVortilo.Vorttipo
Imports Microsoft.FSharp.Core

Public Module Kontrolilaro
   Function ĈuKlasoInfinitivo(vorto As String) As Boolean
      Dim tipo = KrestiaVortilo.Trakilaro.kontroli(vorto)
      Try
         Dim ekzistantaTipo = tipo.Value
         Dim vorttipo = ekzistantaTipo.Item1
         Dim inflekcio = ekzistantaTipo.Item2
         Return (vorttipo.Equals(Vorttipo.NombrigeblaKlaso) Or vorttipo.Equals(Vorttipo.NenombrigeblaKlaso)) And
            inflekcio.Equals(Inflekcio.Infinitivo)
      Catch ex As NullReferenceException
         Return False
      End Try
   End Function
End Module
