using KrestiaServilo.Services;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.SpaServices.ReactDevelopmentServer;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace KrestiaServilo; 

public class Startup {
   public Startup(IConfiguration configuration) {
      Configuration = configuration;
   }

   public IConfiguration Configuration { get; }

   // This method gets called by the runtime. Use this method to add services to the container.
   public void ConfigureServices(IServiceCollection services) {
      services.AddControllersWithViews().AddJsonOptions(options => {
         options.JsonSerializerOptions.Converters.Add(new ModifantoJsonConverter());
         options.JsonSerializerOptions.Converters.Add(new ArgumentoJsonConverter());
         options.JsonSerializerOptions.Converters.Add(new MalinflektaŜtupoJsonConverter());
      });
      services.AddSingleton<IDictionaryService>(_ => new DictionaryService());
      services.AddSpaStaticFiles(configuration => {
         configuration.RootPath = "ClientApp/build";
      });
   }

   // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
   public void Configure(IApplicationBuilder app, IWebHostEnvironment env) {
      if (env.IsDevelopment()) {
         app.UseWebAssemblyDebugging();
      }

      app.UseHttpsRedirection();
      app.UseBlazorFrameworkFiles();
      app.UseStaticFiles();
      app.UseRouting();
      app.UseEndpoints(endpoints => {
         endpoints.MapControllers();
         endpoints.MapFallbackToFile("index.html");
      });
   }
}