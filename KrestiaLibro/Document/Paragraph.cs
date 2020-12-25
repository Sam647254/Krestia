using System.Collections.Generic;
using System.IO;

namespace KrestiaLibro.Document {
   public class Paragraph : DocumentPart {
      public List<Segment> Segments { get; set; }
      internal override void WriteMarkdown(TextWriter output) {
         foreach (var segment in Segments) {
            segment.WriteMarkdown(output);
         }
         output.WriteLine();
         output.WriteLine();
      }
   }

   public class Segment : DocumentPart {
      private string _text;

      public string Text {
         get => _text;
         set => _text = value.TrimStart();
      }

      internal override void WriteMarkdown(TextWriter output) {
         output.Write(Text);
      }
   }

   class EmSegment : Segment {
      public Segment InnerSegment { get; set; }
      internal override void WriteMarkdown(TextWriter output) {
         output.Write("*");
         InnerSegment.WriteMarkdown(output);
         output.Write("*");
      }
   }

   class StrongSegment : Segment {
      public Segment InnerSegment { get; set; }
      internal override void WriteMarkdown(TextWriter output) {
         output.Write("**");
         InnerSegment.WriteMarkdown(output);
         output.Write("** ");
      }
   }

   class UnorderedList : DocumentPart {
      public IEnumerable<ListItem> ListItems { get; set;  }
      internal override void WriteMarkdown(TextWriter output) {
         foreach (var item in ListItems) {
            output.Write("- ");
            item.WriteMarkdown(output);
            output.WriteLine();
         }
         output.WriteLine();
      }
   }

   internal class ListItem : DocumentPart {
      public List<Segment> Segments { get; set; }
      internal override void WriteMarkdown(TextWriter output) {
         foreach (var segment in Segments) {
            segment.WriteMarkdown(output);
         }
      }
   }
}