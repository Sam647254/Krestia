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
using Windows.Storage;
using Windows.Storage.Pickers;
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
      private readonly AWSVortaro vortaro = new AWSVortaro();
      private string novaVorto = "";
      private bool KreiKlason {
         get => Klaso_RadioButton.IsChecked ?? false;
      }
      private bool KreiVerbon {
         get => Verbo_RadioButton.IsChecked ?? false;
      }

      public event PropertyChangedEventHandler PropertyChanged;

      private string NovaVorto {
         get {
            return novaVorto;
         }
         set {
            Debug.WriteLine($"Nova vorto: {value}");
            novaVorto = value;
            EcoŜanĝita("NovaVorto");
         }
      }

      public MainPage() {
         InitializeComponent();
      }

      protected override async void OnNavigatedTo(NavigationEventArgs e) {
         base.OnNavigatedTo(e);
      }

      private async void Aldoni_Click(object sender, RoutedEventArgs e) {
         switch (novaVorto) {
            case string vorto when Kontrolilaro.ĈuKlasoInfinitivo(novaVorto):
               await vortaro.AldoniKlason(vorto, ĈuAnimeco_CheckBox.IsChecked ?? false);
               break;
            case string vorto when Kontrolilaro.ĈuVerboInfinitivo(novaVorto):
               await vortaro.AldoniVerbon(vorto, (int) Valenco_Slider.Value);
               break;
            case string vorto when Kontrolilaro.ĈuPridiranto(novaVorto):
               await vortaro.AldoniPridiranto(vorto);
               break;
         }

         var dialog = new ContentDialog {
            Title = "Vorton aldonis",
            CloseButtonText = "Bone",
         };

         await dialog.ShowAsync();
      }

      private void EcoŜanĝita(string nomo) {
         PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(nomo));
      }

      private void RadioButton_Checked(object sender, RoutedEventArgs e) {
         EcoŜanĝita("KreiKlason");
         EcoŜanĝita("KreiVerbon");
      }

      private async void AldoniPlurajn_AppBarButton_Click(object sender, RoutedEventArgs e) {
         var picker = new FileOpenPicker {
            SuggestedStartLocation = PickerLocationId.DocumentsLibrary
         };
         picker.FileTypeFilter.Add(".txt");
         var file = await picker.PickSingleFileAsync();
         if (file == null) {
            return;
         }
         var content = await FileIO.ReadLinesAsync(file);
         await vortaro.AldoniPlurajn(content);
         await new ContentDialog {
            Title = "Ĉiujn vortojn aldonis",
            CloseButtonText = "Bone"
         }.ShowAsync();
      }
   }
}
