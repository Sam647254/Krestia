using System.Collections.Generic;

namespace KrestiaLibro.Document {
   public class Paragraph : DocumentPart {
      public List<Segment> Segments { get; set; }
   }

   public class Segment {
      public string Text { get; set; }
   }

   class EmSegment : Segment {
      public Segment InnerSegment { get; set; }
   }

   class StrongSegment : Segment {
      public Segment InnerSegment { get; set; }
   }

   class UnorderedList : DocumentPart {
      public List<ListItem> ListItems { get; set; }
   }

   class ListItem : DocumentPart {
      public List<Segment> Segments { get; set; }
   }
}