Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace KrestiaInterfaco.Testo
   <TestClass>
   Public Class VortaroTestoj
      <TestMethod>
      Async Function TestSub() As Task
         Dim vortaro = New TestaVortaro()
         Await vortaro.AldoniKlason("gremu", False)
         Await Assert.ThrowsExceptionAsync(Of InvalidOperationException)(Async Function()
                                                                            Await vortaro.AldoniKlason("gremi", False)
                                                                         End Function)
      End Function
   End Class
End Namespace