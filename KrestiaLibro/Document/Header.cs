using System.IO;

namespace KrestiaLibro.Document {
   public class H1 : DocumentPart {
      public string Text { get; }
      public H1(string text) {
         Text = text;
      }
      
      internal override void WriteMarkdown(TextWriter output) {
         output.Write("# ");
         output.WriteLine(Text);
         output.WriteLine();
      }
   }
}