import React, { useEffect, useState } from "react";
import { Link, useParams } from "react-router-dom";
import { trovi, VortoRezulto } from "../API";

import "./Trovi.scss";

export const inflekcioj = new Map(
   Object.entries({
      Infinitivo: "INF",
      Difinito: "DEF",
      UnuNombro: "SING",
      Havaĵo: "POSN",
      UnuHavaĵo: "POSN_SING",
      PluraHavaĵo: "POSN_PLUR",
      PluraNombro: "PLUR",
      Progresivo: "PROG",
      Perfekto: "STOP",
      Estonteco: "FUTR",
      Desiderativo: "DES",
      PredikativoEsti: "PRED_IS",
      AtributivoEstiAntaŭ: "ATTR_IS>",
      AtributivoEstiMalantaŭ: "ATTR_IS<",
      Havado: "POSS",
      UnuHavado: "POSS_SING",
      PluraHavado: "POSS_PLUR",
      AtributativoHavi: "ATTR_POSS",
      Imperativo: "IMPR",
      Argumento1: "ARG1",
      Argumento2: "ARG2",
      Argumento3: "ARG3",
      Ekzistado: "EXST",
      UnuEkzistado: "EXST_SING",
      PluraEkzistado: "EXST_PLUR",
      Invito: "HORT",
      Translativo: "TRAN",
      Ĝerundo: "GRND",
      SpecifaĜerundo: "SGND",
      PartaUjo1: "PRT1",
      PartaUjo2: "PRT2",
      PartaUjo3: "PRT3",
      Pasivigo: "PASV",
      Igo: "CAUS",
      SolaFormo: "-",
      Egigo: "INTS",
      Etigo: "DIMN",
      Sola: "STDL",
      UnuSola: "STDL_SING",
      PluraSola: "STDL_PLUR",
      Reflekcio: "RFLX",
      Okazo: "OCCR",
      AktualaOkazo: "COCR",
      FinitaOkazo: "FOCR",
      UnueUjo2: "FST2",
      UnueUjo3: "FST3",
      Intenco: "INTN",
      Kvalito: "QUAL",
      Hipoteza: "HYP",
   })
);

interface Params {
   peto: string;
}

function inflektajŜtupoj(rezulto: VortoRezulto) {
   return `It is inflected as ${rezulto.gloso}-${rezulto.malinflektajŜtupoj
      ?.reverse()
      .map((ŝ) => inflekcioj.get(ŝ) || ŝ)
      .join("-")}.`;
}

function specialaRezulto(rezulto: VortoRezulto, peto: string) {
   if (rezulto.plenigitaVorto != null) {
      const ligoAlPlenigita = (
         <Link to={`/word/${rezulto.plenigitaVorto}`}>
            {rezulto.plenigitaVorto}
         </Link>
      );
      if (rezulto.malinflektitaVorto != null) {
         return (
            <span>
               {peto} is an inflected form of a reduced form of{" "}
               {ligoAlPlenigita}. <br /> {inflektajŜtupoj(rezulto)}
            </span>
         );
      }
      return (
         <span>
            {peto} is a reduced form of {ligoAlPlenigita}.
         </span>
      );
   }
   if (rezulto.malinflektitaVorto != null) {
      return (
         <span>
            {peto} is an inflected form of{" "}
            <Link to={`/word/${rezulto.malinflektitaVorto}`}>
               {rezulto.malinflektitaVorto}
            </Link>
            . <br /> {inflektajŜtupoj(rezulto)}
         </span>
      );
   }
   if (rezulto.nombroRezulto != null) {
      return (
         <span>
            {peto} is the number {rezulto.nombroRezulto}.
         </span>
      );
   }
   return null;
}

type Ŝtato = "trovado" | "trovita" | "eraro";

export function Trovi() {
   const { peto } = useParams<Params>();
   const [rezulto, setResult] = useState<VortoRezulto | undefined>(undefined);
   const [ŝtato, setSearchState] = useState<Ŝtato>("trovado");

   useEffect(() => {
      setSearchState("trovado");
      trovi(peto)
         .then((rezulto) => {
            setSearchState("trovita");
            setResult(rezulto);
         })
         .catch(() => {
            setSearchState("eraro");
         });
   }, [peto]);

   const glosoPeto = peto.trim().split(" ");

   if (ŝtato === "eraro")
      return <div>An error occurred during the search. Please try again.</div>;

   if (
      ŝtato === "trovado" ||
      rezulto == null ||
      (glosoPeto.length > 1 &&
         rezulto.glosajVortoj != null &&
         glosoPeto.length > (rezulto.glosajVortoj.length || 0))
   )
      return <div>Searching...</div>;

   const speciala = specialaRezulto(rezulto, peto);
   if (
      rezulto.rezultoj.length === 0 &&
      rezulto.plenigitaVorto == null &&
      rezulto.malinflektitaVorto == null &&
      rezulto.glosajVortoj == null &&
      rezulto.nombroRezulto == null
   ) {
      return <div>No results for "{peto}".</div>;
   }

   let glosaRezulto = null;
   if (rezulto.glosajVortoj != null) {
      const vortoj = peto.split(" ");
      glosaRezulto = (
         <div>
            <p>Gloss result for "{peto}":</p>
            <table className="gloso-tabelo">
               <thead>
                  <tr>
                     <th>Word</th>
                     <th>Base</th>
                     <th>Gloss</th>
                  </tr>
               </thead>
               <tbody>
                  {vortoj.map((v: any, i: any) => (
                     <tr key={v}>
                        <td>{v}</td>
                        <td>
                           {rezulto?.bazajVortoj![i].length === 0 ? null : (
                              <Link to={`/word/${rezulto?.bazajVortoj![i]}`}>
                                 {rezulto?.bazajVortoj![i]}
                              </Link>
                           )}
                        </td>
                        <td>
                           {rezulto?.glosajVortoj![i]}
                           {rezulto?.glosajŜtupoj?.length! > 0
                              ? rezulto
                                   ?.glosajŜtupoj![i]?.reverse()
                                   .map((ŝ) => `-${inflekcioj.get(ŝ) || ŝ}`)
                                   .join("")
                              : null}
                        </td>
                     </tr>
                  ))}
               </tbody>
            </table>
         </div>
      );
   }

   return (
      <div>
         {glosaRezulto}
         <div className="peto">Search results for "{peto}":</div>
         {speciala == null ? null : <div>{speciala}</div>}
         {rezulto.rezultoj.map((r) => (
            <div className="rezulto" key={r.vorto}>
               <span className="rezulto-vorto">
                  <Link to={`/word/${r.vorto}`}>{r.vorto}</Link>
               </span>
               <span className="rezulto-signifo">{r.signifo}</span>
            </div>
         ))}
      </div>
   );
}
