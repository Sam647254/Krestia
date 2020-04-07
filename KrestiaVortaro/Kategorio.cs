namespace KrestiaVortaro {
   public class Kategorio {
      public int Id { get; }
      
      public string Nomo { get; }

      public Kategorio(int id, string nomo) {
         Id = id;
         Nomo = nomo;
      }
   }
}