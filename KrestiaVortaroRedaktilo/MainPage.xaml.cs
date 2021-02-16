using System;
using System.Collections.Generic;
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
using KrestiaVortaroBazo;

// The Blank Page item template is documented at https://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

namespace KrestiaVortaroRedaktilo {
   /// <summary>
   /// An empty page that can be used on its own or navigated to within a Frame.
   /// </summary>
   public sealed partial class MainPage : Page {
      private static NovaVortaraIndekso? _dictionary;
      
      public MainPage() {
         InitializeComponent();
      }

      private async void OpenDictionary_OnClick(object sender, RoutedEventArgs e) {
         var picker = new FileOpenPicker();
         picker.FileTypeFilter.Add(".json");
         var file = await picker.PickSingleFileAsync();
         if (file == null) return;
         var content = await FileIO.ReadTextAsync(file)!;
         _dictionary = new NovaVortaraIndekso(content);
         Stats.Text = $"Total words: {_dictionary.Indekso.Count}";
      }
   }
}