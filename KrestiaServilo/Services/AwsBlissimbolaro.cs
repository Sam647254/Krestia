using System.IO;
using System.Threading.Tasks;
using Amazon;
using Amazon.S3;

namespace KrestiaServilo.Services {
   public class AwsBlissimbolaro : IBlissFonto {
      private const string Bucket = "blissimboloj";
      private readonly AmazonS3Client _amazonS3Client = new AmazonS3Client(RegionEndpoint.USWest2);

      public async Task<string> AlportiBlissimbolon(int id) {
         using var respondo = await _amazonS3Client.GetObjectAsync(Bucket, $"bliss_svg_id/{id}.svg");
         await using var stream = respondo.ResponseStream;
         using var reader = new StreamReader(stream);
         return await reader.ReadToEndAsync();
      }
   }
}