using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaAWSAlirilo {
   static class Program {
      static async Task Main(string[] args) {
         var awsAlirilo = new AwsAlirilo();
         switch (args[0]) {
            case "aldoni1":
               var vortoj = await awsAlirilo.AlportiĈiujnVortojnKunSignifoj();
               await File.WriteAllLinesAsync(args[1], vortoj.Select(r => $"{r.Item1}|{r.Item2}|"));
               break;
            case "aldoni2":
               var eniro = await File.ReadAllLinesAsync(args[1]);
               await UnuFojajProgrametoj.AldoniKategorionAlĈiujVorto(awsAlirilo, eniro.Select(v => {
                  var partoj = v.Split('|');
                  return (partoj[0], partoj[2]);
               }));
               break;
            case "ĉiuj":
               await UnuFojajProgrametoj.AlportiĈiujnVortojn(awsAlirilo, args[1]);
               break;
         }
      }
   }
}