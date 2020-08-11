using KrestiaServilo.Services;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace KrestiaServilo {
   public class Startup {
      public Startup(IConfiguration configuration) {
         Configuration = configuration;
      }

      public IConfiguration Configuration { get; }

      // This method gets called by the runtime. Use this method to add services to the container.
      public void ConfigureServices(IServiceCollection services) {
         services.AddControllers().AddJsonOptions(options => {
            options.JsonSerializerOptions.Converters.Add(new ModifantoJsonConverter());
         });
         services.AddSingleton<IVortaroService>(provider => new VortaroService());
         services.AddSingleton<IBlissFonto>(provider => new AwsBlissimbolaro());
      }

      // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
      public void Configure(IApplicationBuilder app, IWebHostEnvironment env) {
         if (env.IsDevelopment()) {
            app.UseDeveloperExceptionPage();
         }

         app.UseHttpsRedirection();

         app.UseRouting();

         app.UseAuthorization();

         app.UseEndpoints(endpoints => { endpoints.MapControllers(); });

         app.UseDefaultFiles();

         app.UseStaticFiles();

         app.UseSpa(builder => { });
      }
   }
}