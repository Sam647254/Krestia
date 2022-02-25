using Microsoft.FSharp.Core;

namespace KrestiaVortaro; 

public static class Helpers {
   public static bool IsSome<T>(this FSharpOption<T> option) {
      return FSharpOption<T>.get_IsSome(option);
   }

   public static bool IsNone<T>(this FSharpOption<T> option) {
      return FSharpOption<T>.get_IsNone(option);
   }
}