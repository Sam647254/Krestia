Imports KrestiaVortilo
Imports KrestiaVortilo.Vorttipo
Imports Vorttipo = KrestiaVortilo.Vorttipo.Vorttipo

Public Module Kontrolilaro
   Function ĈuKlasoInfinitivo(vorto As String) As Boolean
      Return ĈuEstas(vorto, Vorttipo.NombrigeblaKlaso, Vorttipo.NenombrigeblaKlaso, Inflekcio.Infinitivo)
   End Function

   Function ĈuVerboInfinitivo(vorto As String) As Boolean
      Return ĈuEstas(vorto, Vorttipo.TransitivaVerbo, Vorttipo.NetransitivaVerbo, Inflekcio.Infinitivo)
   End Function

   Function ĈuPridiranto(vorto As String) As Boolean
      Return ĈuEstas(vorto, Vorttipo.Pridiranto, Vorttipo.Pridiranto, Inflekcio.Infinitivo)
   End Function

   Private Function ĈuEstas(vorto As String, tipo1 As Vorttipo, tipo2 As Vorttipo, inflekcio As Inflekcio) As Boolean
      Dim tuple = Traktilaro.kontroli(vorto)
      Try
         Dim ekzistantaTipo = tuple.Value
         Dim rezultaTipo = ekzistantaTipo.Item1
         Dim rezultaInflekcio = ekzistantaTipo.Item2
         Return (rezultaTipo.Equals(tipo1) Or rezultaTipo.Equals(tipo2)) And rezultaInflekcio.Equals(inflekcio)
      Catch ex As NullReferenceException
         Return False
      End Try
   End Function
End Module
