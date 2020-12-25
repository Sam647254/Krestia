using System;
using System.IO;

namespace KrestiaLibro {
   class Program {
      static void Main(string[] args) {
         Topics.Topics.Pragmatics.WriteMarkdown(new StreamWriter(Console.OpenStandardOutput()));
      }
   }
}