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

   Function TipoDe(vorto As String) As KreitaVorttipo
      If ĈuKlasoInfinitivo(vorto) Then
         Return KreitaVorttipo.Klaso
      ElseIf ĈuPridiranto(vorto) Then
         Return KreitaVorttipo.Pridiranto
      ElseIf ĈuVerboInfinitivo(vorto) Then
         If ĈuEstas(vorto, Vorttipo.TransitivaVerbo, Vorttipo.TransitivaVerbo, Inflekcio.Infinitivo) Then
            Return KreitaVorttipo.TransitivaVerbo
         ElseIf ĈuEstas(vorto, Vorttipo.NetransitivaVerbo, Vorttipo.NetransitivaVerbo, Inflekcio.Infinitivo) Then
            Return KreitaVorttipo.NetransitivaVerbo
         ElseIf ĈuEstas(vorto, Vorttipo.MalplenaVerbo, Vorttipo.MalplenaVerbo, Inflekcio.Infinitivo) Then
            Return KreitaVorttipo.MalplenaVerbo
         ElseIf ĈuEstas(vorto, Vorttipo.PartaTransitivaVerbo, Vorttipo.PartaTransitivaVerbo, Inflekcio.Infinitivo) Then
            Return KreitaVorttipo.PartaTransitivaVerbo
         ElseIf ĈuEstas(vorto, Vorttipo.PartaNetransitivaVerbo, Vorttipo.PartaNetransitivaVerbo, Inflekcio.Infinitivo) Then
            Return KreitaVorttipo.PartaNetransitivaVerbo
         Else
            Return KreitaVorttipo.Nekonita
         End If
      Else
         Return KreitaVorttipo.Nekonita
      End If
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

   Public Enum KreitaVorttipo
      Klaso
      TransitivaVerbo
      NetransitivaVerbo
      MalplenaVerbo
      PartaTransitivaVerbo
      PartaNetransitivaVerbo
      Pridiranto
      Nekonita
   End Enum
End Module