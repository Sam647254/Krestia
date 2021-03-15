import React, { useEffect, useState } from "react";

import "./Legilo.scss";
import {
   alportiGloson,
   Argumento,
   Eraro,
   legi,
   MalinflektitaVorto,
   Modifanto,
   ModifeblaVorto,
   Predikato,
   Rezulto,
} from "../API";
import { inflekcioj } from "./Trovi";
import { useParams, Redirect } from "react-router-dom";

interface Params {
   eniro?: string;
}

export function RedirektaLegilo() {
   const params = useParams<Params>();
   return params.eniro == null ? (
      <Redirect to="/parse" />
   ) : (
      <Redirect to={`/parse/${btoa(params.eniro)}`} />
   );
}

export function Legilo() {
   const eniro = React.createRef<HTMLTextAreaElement>();
   const [rezulto, setResult] = useState<Rezulto | Eraro | null>(null);
   const [legado, setParsing] = useState(false);
   const params = useParams<Params>();

   useEffect(() => {
      if (params.eniro != null) {
         legi(atob(params.eniro!)).then(setResult);
      }
   }, [params.eniro]);

   return (
      <div className="legilo">
         <textarea className="legilo-eniro" rows={10} ref={eniro}>
            {params.eniro != null ? atob(params.eniro!) : null}
         </textarea>
         <button
            type="button"
            className="serĉiButono legi-butono"
            onClick={async (event) => {
               const eniraTeksto = eniro.current!.value;
               setParsing(true);
               const respondo = await legi(eniraTeksto);
               window.history.replaceState(
                  null,
                  "",
                  `/parse/${btoa(eniraTeksto)}`
               );
               setParsing(false);
               setResult(respondo);
            }}
         >
            Parse
         </button>
         {legado ? (
            <p>Waiting for result</p>
         ) : rezulto != null ? (
            rezulto instanceof Array ? (
               <EraroAfiŝo eraro={rezulto} />
            ) : (
               RezultoAfiŝo(rezulto)
            )
         ) : null}
      </div>
   );
}

function EraroAfiŝo({ eraro }: { eraro: Eraro }) {
   const [vorto, mesaĝo] = eraro;
   return (
      <div className="eraro-afiŝo">
         Error while parsing:{" "}
         {vorto.pozo === 0 && vorto.vico === 0 && vorto.vorto === "" ? null : (
            <p>
               On line {vorto.vico} at position {vorto.pozo} (word {vorto.vorto}
               ):
            </p>
         )}
         {mesaĝo}
      </div>
   );
}

function RezultoAfiŝo(rezulto: Rezulto) {
   return (
      <div>
         {rezulto.frazoj.length > 0
            ? [
                 <h2>
                    Parsed {rezulto.frazoj.length} sentence
                    {rezulto.frazoj.length > 1 ? "s" : ""}
                 </h2>,
                 rezulto.frazoj.map((frazo, i) => (
                    <FrazoAfiŝo frazo={frazo} key={i} subfrazo={false} />
                 )),
              ]
            : null}
         {rezulto.argumentoj.length > 0
            ? [
                 <h2>Leftover arguments</h2>,
                 rezulto.argumentoj.map((a, i) => (
                    <ArgumentoAfiŝo key={i} argumento={a} />
                 )),
              ]
            : null}
      </div>
   );
}

function ArgumentoAfiŝo({
   argumento,
   onHover,
   por,
}: {
   argumento: Argumento;
   onHover?: (hover: boolean) => void;
   por?: MalinflektitaVorto;
}) {
   const [kaŝita, setHidden] = useState(true);
   const [hover, setHover] = useState(false);
   switch (argumento.tipo) {
      case "ene":
      case "mine":
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <VortoAfiŝo
                  vorto={{
                     kapo:
                        argumento.tipo === "ene"
                           ? argumento.ene
                           : argumento.mine,
                     modifantoj: [],
                  }}
                  onHover={setHover}
                  glosaInfo={
                     por != null
                        ? `Argument for ${por.originalaVorto.vorto}`
                        : undefined
                  }
               />{" "}
               {kaŝita ? (
                  <span onClick={() => setHidden(!kaŝita)}>(...)</span>
               ) : (
                  <span className={hover ? "hover-child" : ""}>
                     <FrazoAfiŝo frazo={argumento.predikato} subfrazo />
                  </span>
               )}
            </span>
         );
      case "ArgumentaVorto":
         return (
            <span
               className={hover ? "hover-child" : ""}
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <VortoAfiŝo
                  vorto={argumento.vorto}
                  glosaInfo={
                     por != null
                        ? `Argument for ${por.originalaVorto.vorto}`
                        : undefined
                  }
               />{" "}
            </span>
         );
      case "nombro":
         return <span>{argumento.nombro} </span>;
   }
}

function FrazoAfiŝo({
   frazo,
   subfrazo,
   onHover,
}: {
   frazo: Predikato;
   subfrazo: boolean;
   onHover?: (hover: boolean) => void;
}) {
   const [hover, setHover] = useState(false);
   const [childHover, setChildHover] = useState(false);
   return (
      <span
         className={subfrazo ? "" : "predikato"}
         onMouseOver={() => onHover?.(true)}
         onMouseOut={() => onHover?.(false)}
      >
         <span className={subfrazo ? "" : "verbo"}>
            <VortoAfiŝo
               className={[
                  "predikata-vorto",
                  childHover ? "hover-parent" : "",
               ].join(" ")}
               vorto={frazo.kapo.vorto}
               onHover={setHover}
               glosaInfo="Predicate verb"
            />
         </span>{" "}
         <span className={hover ? "hover-child" : ""}>
            {frazo.argumentoj.map((a, i) => (
               <ArgumentoAfiŝo
                  argumento={a}
                  key={i}
                  onHover={setChildHover}
                  por={frazo.kapo.vorto.kapo}
               />
            ))}
         </span>
      </span>
   );
}

function VortoAfiŝo({
   vorto,
   className = "",
   onHover,
   glosaInfo,
}: {
   vorto: ModifeblaVorto;
   className?: string | undefined;
   onHover?: (hover: boolean) => void;
   glosaInfo?: string;
}) {
   const [kaŝita, setHidden] = useState(true);
   const [hover, setHover] = useState(false);
   const [childHover, setChildHover] = useState(false);
   return (
      <span>
         <span
            className={`${className} ${
               hover ? "hover-self" : childHover ? "hover-parent" : ""
            }`}
            onMouseOver={() => {
               onHover?.(true);
               setHover(true);
            }}
            onMouseOut={() => {
               onHover?.(false);
               setHover(false);
            }}
         >
            <BasaAfiŝo vorto={vorto.kapo} glosaInfo={glosaInfo} />
         </span>{" "}
         <span>
            {vorto.modifantoj.length > 0 ? (
               kaŝita ? (
                  <span className="kaŝita" onClick={() => setHidden(!kaŝita)}>
                     [...]
                  </span>
               ) : (
                  <span className="malkaŝita">
                     [{" "}
                     {vorto.modifantoj.map((m, i) => (
                        <ModifantoAfiŝo
                           key={i}
                           modifanto={m}
                           onHover={setChildHover}
                        />
                     ))}{" "}
                     ]
                  </span>
               )
            ) : null}
         </span>
      </span>
   );
}

function ModifantoAfiŝo({
   modifanto,
   onHover,
}: {
   modifanto: Modifanto;
   onHover?: (hover: boolean) => void;
}): JSX.Element {
   const [kaŝita, setHidden] = useState(true);
   const [hover, setHover] = useState(false);
   switch (modifanto.tipo) {
      case "Pridiranto": {
         return (
            <ArgumentoAfiŝo argumento={modifanto.argumento} onHover={onHover} />
         );
      }
      case "EcoDe": {
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               of: <ArgumentoAfiŝo argumento={modifanto.argumento} />
            </span>
         );
      }
      case "ModifantoKunFrazo": {
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <span
                  className={hover ? "hover-self" : ""}
                  onMouseOver={() => setHover(true)}
                  onMouseOut={() => setHover(false)}
               >
                  <BasaAfiŝo vorto={modifanto.modifanto} />{" "}
               </span>
               {kaŝita ? (
                  <span onClick={() => setHidden(!kaŝita)}>(...)</span>
               ) : (
                  <FrazoAfiŝo frazo={modifanto.frazo} subfrazo />
               )}
            </span>
         );
      }
      case "ModifantoKunArgumentoj": {
         return modifanto.argumento.length > 0 ? (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <span
                  className={hover ? "hover-self" : ""}
                  onMouseOut={() => setHover(false)}
                  onMouseOver={() => setHover(true)}
               >
                  <BasaAfiŝo vorto={modifanto.modifanto} />
               </span>{" "}
               (
               {modifanto.argumento.map((argumento) => (
                  <ArgumentoAfiŝo argumento={argumento} />
               ))}
               )
            </span>
         ) : (
            <span
               className={hover ? "hover-self" : ""}
               onMouseOver={() => {
                  onHover?.(true);
                  setHover(true);
               }}
               onMouseOut={() => {
                  onHover?.(false);
                  setHover(false);
               }}
            >
               <BasaAfiŝo vorto={modifanto.modifanto} />{" "}
            </span>
         );
      }
      case "Keni": {
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <ArgumentoAfiŝo argumento={modifanto.argumento1} />
               <ArgumentoAfiŝo argumento={modifanto.argumento2} />
            </span>
         );
      }
      case "Pini": {
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <ArgumentoAfiŝo argumento={modifanto.argumento1} />
               <ArgumentoAfiŝo argumento={modifanto.argumento2} />
               <ArgumentoAfiŝo argumento={modifanto.argumento3} />
            </span>
         );
      }
      case "Ene":
      case "Mine": {
         return (
            <span
               onMouseOver={() => onHover?.(true)}
               onMouseOut={() => onHover?.(false)}
            >
               <FrazoAfiŝo frazo={modifanto.predikato} subfrazo />
            </span>
         );
      }
   }
}

function BasaAfiŝo({
   vorto,
   glosaInfo,
}: {
   vorto: MalinflektitaVorto | string;
   glosaInfo?: string;
}) {
   const [glosoTrovita, setFound] = useState(false);
   const [gloso, setGloss] = useState<string | undefined>();

   return (
      <span
         className="vorto-afiŝo"
         onMouseOver={() => {
            alportiGloson(
               typeof vorto === "string" ? vorto : vorto.bazaVorto
            ).then((rezulto) => {
               setFound(true);
               if (rezulto != null) {
                  setGloss(rezulto.gloso);
               }
            });
         }}
      >
         {typeof vorto === "string" ? vorto : vorto.originalaVorto.vorto}
         <span className="vorto-gloso">
            {glosoTrovita ? (
               [
                  gloso == null ? <p>(Word not in dictionary)</p> : null,
                  <p>
                     {typeof vorto === "string"
                        ? gloso || vorto
                        : vorto.inflekcioŜtupoj
                             .map((ŝ) =>
                                ŝ.tipo === "Bazo"
                                   ? gloso == null
                                      ? ŝ.bazaVorto
                                      : gloso
                                   : inflekcioj.get(ŝ.inflekcio) || ŝ.inflekcio
                             )
                             .reverse()
                             .join("-")}
                  </p>,
                  glosaInfo != null ? (
                     <p className="glosa-info">{glosaInfo}</p>
                  ) : null,
               ]
            ) : (
               <p>...</p>
            )}
         </span>
      </span>
   );
}
