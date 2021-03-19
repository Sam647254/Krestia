import React, {useEffect, useState} from "react";
import {Link, useParams} from "react-router-dom";
import {alporti, PlenaVortoRespondo} from "../API";

import "./Vorto.scss";
import {Timeran} from "./Timeran";

const inflekcioj = new Map(
   Object.entries({
      Infinitivo: "Infinitive",
      Difinito: "Definite",
      UnuNombro: "Singular",
      PluraNombro: "Plural",
      Fokuso: "Focus",
      Havaĵo: "Possession",
      Progresivo: "Progressive",
      Perfekto: "Perfect",
      Intenco: "Intention",
      Desiderativo: "Desiderative",
      PredikativoEsti: "Predicative identity",
      AtributivoEstiAntaŭ: "Attributive identity (prefix)",
      AtributivoEstiMalantaŭ: "Attributive identity (postfix)",
      Havado: "Possessive",
      UnuHavado: "Possessive (singular)",
      PluraHavado: "Possessive (plural)",
      Imperativo: "Imperative",
      Argumento1: "Slot-1 argument",
      Argumento2: "Slot-2 argument",
      Argumento3: "Slot-3 argument",
      Ekzistado: "Existential",
      Hortativo: "Hortative",
      Translativo: "Translative",
      Ĝerundo: "Gerund",
      SpecifaĜerundo: "Specific gerund",
      PartaUjo1: "Slot-1 partial",
      PartaUjo2: "Slot-2 partial",
      PartaUjo3: "Slot-3 partial",
      UnueUjo2: "Slot-2 first",
      UnueUjo3: "Slot-3 first",
      Igo: "Causative",
      Sola: "Standalone",
      Etigo: "Diminutive",
      Reflekcio: "Reflexive",
      Okazo: "Occurrence",
      AktualaOkazo: "Ongoing occurrence",
      FinitaOkazo: "Complete occurrence",
      Hipoteza: "Hypothetical",
      Kvalito: "Quality",
      Optativo: "Optative",
   })
);

interface Params {
   vorto: string;
}

type Ŝtato = "alportado" | "alportita" | "netrovita" | "eraro";

export function Vorto() {
   const {vorto} = useParams<Params>();
   const [rezulto, setResult] = useState<PlenaVortoRespondo | undefined>();
   const [ŝtato, setFetchState] = useState<Ŝtato>("alportado");

   useEffect(() => {
      setFetchState("alportado");
      alporti(vorto)
         .then((respondo) => {
            setResult(respondo);
            setFetchState("alportita");
         })
         .catch((eraro) => {
            console.error(eraro.response);
            if (eraro.response.status === 404) {
               setFetchState("netrovita");
            } else {
               setFetchState("eraro");
            }
         });
   }, [vorto]);

   if (ŝtato === "alportado") return <div>Loading...</div>;
   if (ŝtato === "netrovita") return <div>Word not found: {vorto}</div>;
   if (ŝtato === "eraro")
      return <div>A server-side error occurred when fetching {vorto}.</div>;
   if (rezulto == null) throw new Error("Unreachable state");

   return (
      <div>
         <h2 className="vorto">{rezulto.vorto}</h2>
         <Timeran alteco={25} silaboj={rezulto.silaboj}/>
         {rezulto.blissimbolo != null ? (
            <Timeran
               alteco={25}
               silaboj={rezulto.silaboj}
               bliss={rezulto.blissimbolo}
            />
         ) : null}
         <div className="streko"/>
         <p className="vorttipo">{rezulto.vorttipo}</p>
         <p>{rezulto.signifo}</p>
         {rezulto.frazaSignifo != null ? (
            <p>
               In context:{" "}
               <span
                  dangerouslySetInnerHTML={{__html: rezulto.frazaSignifo}}
               />
            </p>
         ) : null}
         {rezulto.sintakso != null ? (
            <p>
               Syntax: <span dangerouslySetInnerHTML={{__html: rezulto.sintakso}}/>
            </p>
         ) : null}
         {rezulto.modifeblajVorttipoj != null ? (
            <div>
               <p>Valid types for {"<v>"}: </p>
               <ul>
                  {rezulto.modifeblajVorttipoj.map(t => <li>{t}</li>)}
               </ul>
            </div>
         ) : null}
         <p>
            {rezulto.ujoj?.map((ujo, i) =>
               ujo == null
                  ? null
                  : [
                     <span>
                          Slot {i + 1}: {ujo}
                       </span>,
                     <br/>,
                  ]
            )}
         </p>
         {rezulto.noto.length > 0 ? (
            <p>
               Notes:{" "}
               <span dangerouslySetInnerHTML={{__html: rezulto.noto}}/>
            </p>
         ) : null}
         {rezulto.radikoj.length > 0 ? (
            <p>
               Roots:{" "}
               {rezulto?.radikoj
                  .map((r) => (
                     <Link to={`/word/${r}`} key={r}>
                        {r}
                     </Link>
                  ))
                  .reduce((lasta, sekva) => [lasta, ", ", sekva] as any)}
            </p>
         ) : null}
         {rezulto?.inflektitajFormoj == null
            ? null
            : [
               <h3 key="h3">Inflected forms</h3>,
               <table className="inflekcio-tabelo" key="tabelo">
                  <thead>
                  <tr>
                     <th>Inflection</th>
                     <th>Inflected form</th>
                  </tr>
                  </thead>
                  <tbody>
                  {Object.entries(rezulto.inflektitajFormoj).map(
                     ([inflekcio, inflektitaFormo]) => (
                        <tr key={inflekcio}>
                           <td>
                              {inflekcioj.get(inflekcio) || inflekcio}
                           </td>
                           <td>{inflektitaFormo}</td>
                        </tr>
                     )
                  )}
                  </tbody>
               </table>,
            ]}
      </div>
   );
}
