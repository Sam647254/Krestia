using KrestiaInterfaco.Iloj;
using KrestiaVortaro;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;

// Pour plus d'informations sur le modèle d'élément Page vierge, consultez la page https://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

namespace KrestiaInterfaco {
   /// <summary>
   /// Une page vide peut être utilisée seule ou constituer une page de destination au sein d'un frame.
   /// </summary>
   public sealed partial class MainPage : Page, INotifyPropertyChanged {
      private readonly Vortaro vortaro = new AWSVortaro();
      private string novaVorto = "";

      public event PropertyChangedEventHandler PropertyChanged;

      private string NovaVorto {
         get {
            return novaVorto;
         }
         set {
            Debug.WriteLine($"Nova vorto: {value}");
            novaVorto = value;
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs("NovaVorto"));
         }
      }

      public MainPage() {
         InitializeComponent();
      }

      protected override async void OnNavigatedTo(NavigationEventArgs e) {
         base.OnNavigatedTo(e);
      }

      private void Aldoni_Click(object sender, RoutedEventArgs e) {

      }
   }
}
