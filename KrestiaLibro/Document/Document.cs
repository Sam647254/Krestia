using System.IO;

namespace KrestiaLibro.Document {
   public class Document {
      
   }

   public abstract class DocumentPart {
      internal abstract void WriteMarkdown(TextWriter output);
   }
}