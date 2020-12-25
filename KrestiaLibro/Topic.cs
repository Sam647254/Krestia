using System;
using System.Collections.Generic;
using System.IO;
using KrestiaLibro.Document;

namespace KrestiaLibro {
   public class Topic {
      public string Title { get; set; }
      public IEnumerable<DocumentPart> DocumentParts { get; set; }

      public void WriteMarkdown(TextWriter output) {
         foreach (var part in DocumentParts) {
            part.WriteMarkdown(output);
         }
      }
      
      public static Topic Create(string title, Action<TopicBuilder> block) {
         var builder = new TopicBuilder(title);
         block(builder);
         return builder.Build();
      }
   }

   public class TopicBuilder {
      private List<DocumentPart> _documentParts = new List<DocumentPart>();
      private string Title { get; }
      
      internal TopicBuilder(string title) {
         Title = title;
         _documentParts.Add(new H1(title));
      }

      internal void Paragraph(Action<ParagraphBuilder> block) {
         var builder = new ParagraphBuilder();
         block(builder);
         _documentParts.Add(builder.Build());
      }

      internal void Ul(params ListItem[] listItems) {
         _documentParts.Add(new UnorderedList {
            ListItems = listItems
         });
      }

      internal ListItem Li(params Segment[] segments) {
         return new ListItem {
            Segments = new List<Segment>(segments)
         };
      }

      internal Segment T(string text) {
         return new Segment {
            Text = text
         };
      }

      internal StrongSegment Strong(Segment text) {
         return new StrongSegment {
            InnerSegment = text
         };
      }

      internal Topic Build() {
         return new Topic {
            Title = Title,
            DocumentParts = _documentParts
         };
      }

      internal class ParagraphBuilder {
         private readonly List<Segment> _segments = new List<Segment>();

         internal void Text(string text) {
            _segments.Add(new Segment {
               Text = text
            });
         }

         internal Paragraph Build() {
            return new Paragraph {
               Segments = _segments
            };
         }
      }
   }
}