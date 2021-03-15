import Axios from "axios";

export async function trovi(peto: string) {
   const respondo = await Axios.get(`/api/trovi/${peto}`);
   return (await respondo.data) as VortoRezulto;
}

export async function alporti(vorto: string) {
   const respondo = await Axios.get(`/api/vorto/${vorto}`);
   return (await respondo.data) as PlenaVortoRespondo;
}

export async function alportiĈiujn() {
   const respondo = await Axios.get(`/api/vortlisto/alfabeta`);
   return (await respondo.data) as VortoRespondo[];
}

export async function alportiĈiujnTipajn() {
   const respondo = await Axios.get("/api/vortlisto/tipo");
   return (await respondo.data) as TipaVortlisto;
}

export async function alportiĈiujnKategoriojn() {
   const respondo = await Axios.get("/api/vortlisto/kategorioj");
   return (await respondo.data) as KategoriaVortlisto;
}

export async function legi(eniro: string): Promise<Rezulto | Eraro> {
   try {
      const respondo = await Axios.post("/api/legi", { eniro });
      return (await respondo.data) as Rezulto;
   } catch (e) {
      if (e.response.status === 422) {
         const data = e.response.data;
         return [data.item1, data.item2];
      } else {
         throw e;
      }
   }
}

const glosoj = new Map<string, string>();

export async function alportiGloson(
   vorto: string
): Promise<GlosaRezulto | null> {
   if (glosoj.has(vorto)) {
      return { gloso: glosoj.get(vorto)! };
   }
   try {
      const respondo = await Axios.get(`/api/gloso/${vorto}`);
      if (respondo.status === 200) {
         const rezulto = (await respondo.data) as GlosaRezulto;
         glosoj.set(vorto, rezulto.gloso);
         return rezulto;
      } else {
         return null;
      }
   } catch (e) {
      if (e.response.status === 404) return null;
      throw e;
   }
}

export interface VortoRezulto {
   malinflektitaVorto: string | undefined;
   plenigitaVorto: string | undefined;
   rezultoj: VortoRespondo[];
   gloso: string | undefined;
   malinflektajŜtupoj: string[] | undefined;
   glosajVortoj: string[] | undefined;
   glosajŜtupoj: string[][] | undefined;
   bazajVortoj: string[] | undefined;
   nombroRezulto: number | undefined;
}

export interface VortoRespondo {
   vorto: string;
   signifo: string;
}

export interface PlenaVortoRespondo extends VortoRespondo {
   kategorioj: string[];
   noto: string;
   radikoj: string[];
   vorttipo: string;
   silaboj: string[];
   inflektitajFormoj: {
      [key: string]: string;
   };
   blissimbolo?: number[];
   ujoj: [string, string, string];
   frazaSignifo?: string;
}

export interface TipaVortlisto {
   [key: string]: VortoRespondo[];
}

export interface KategoriaVortlisto {
   [key: string]: {
      vortoj: VortoRespondo[];
   };
}

export interface MalinflektitaVorto {
   originalaVorto: EniraVorto;
   bazaVorto: string;
   inflekcioŜtupoj: MalinflektaŜtupo[];
}

export type MalinflektaŜtupo =
   | {
        tipo: "Nebazo";
        vorttipo: string;
        inflekcio: string;
        restantaVorto: string;
     }
   | {
        tipo: "Bazo";
        vorttipo: string;
        inflekcio: string;
        bazaVorto: string;
     };

export interface EniraVorto {
   vico: number;
   pozo: number;
   vorto: string;
}

export type Argumento =
   | {
        tipo: "ArgumentaVorto";
        vorto: ModifeblaVorto;
     }
   | {
        tipo: "ene";
        predikato: Predikato;
        ene: MalinflektitaVorto;
     }
   | {
        tipo: "mine";
        predikato: Predikato;
        mine: MalinflektitaVorto;
     }
   | {
        tipo: "nombro";
        nombro: number;
     };

export interface ModifeblaVorto {
   kapo: MalinflektitaVorto;
   modifantoj: Modifanto[];
}

export type Modifanto =
   | {
        tipo: "Pridiranto";
        argumento: Argumento;
     }
   | {
        tipo: "EcoDe";
        argumento: Argumento;
     }
   | {
        tipo: "ModifantoKunFrazo";
        modifanto: string;
        frazo: Predikato;
     }
   | {
        tipo: "ModifantoKunArgumentoj";
        modifanto: string;
        argumento: Argumento[];
     }
   | {
        tipo: "Mine";
        predikato: Predikato;
     }
   | {
        tipo: "Ene";
        predikato: Predikato;
     }
   | {
        tipo: "Keni";
        argumento1: Argumento;
        argumento2: Argumento;
     }
   | {
        tipo: "Pini";
        argumento1: Argumento;
        argumento2: Argumento;
        argumento3: Argumento;
     };

export interface Rezulto {
   frazoj: Predikato[];
   argumentoj: Argumento[];
}

export interface Predikato {
   kapo: Verbo;
   argumentoj: Argumento[];
}

export interface Verbo {
   vorto: ModifeblaVorto;
}

export type Eraro = [EniraVorto, string];

export interface GlosaRezulto {
   gloso: string;
}
