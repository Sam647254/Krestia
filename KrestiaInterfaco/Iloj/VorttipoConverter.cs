using KrestiaVortaro;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.UI.Xaml.Data;

namespace KrestiaInterfaco.Iloj {
   public class VorttipoConverter : IValueConverter {
      public object Convert(object value, Type targetType, object parameter, string language) {
         return value switch {
            string eniro when Kontrolilaro.ĈuKlasoInfinitivo(eniro) => "Klaso",
            string eniro when Kontrolilaro.ĈuPridiranto(eniro) => "Pridiranto",
            string eniro when Kontrolilaro.ĈuVerboInfinitivo(eniro) => "Verbo",
            string eniro when eniro.Length > 0 => "Nekonita",
            _ => "Nova vorto",
         };
      }

      public object ConvertBack(object value, Type targetType, object parameter, string language) {
         throw new NotImplementedException();
      }
   }
}
