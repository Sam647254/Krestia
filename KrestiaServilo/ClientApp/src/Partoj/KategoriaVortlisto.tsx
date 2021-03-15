import { APIKomponanto } from "./APIKomponanto";
import { alportiĈiujnKategoriojn } from "../API";
import React from "react";
import ScrollableAnchor from "react-scrollable-anchor";
import { Vorttabelo } from "./Vorttabelo";

export const KategoriaVortlisto = () =>
   APIKomponanto(alportiĈiujnKategoriojn, rezulto => (
      <div>
         <p>
            Categories at a glance:
            <ul>
               {Object.keys(rezulto).map(k => (
                  <li>
                     <a href={`#${k}`}>{k}</a>
                  </li>
               ))}
            </ul>
         </p>
         <div className="streko"/>
         {Object.entries(rezulto).map(p => {
            const [nomo, grupo] = p;
            return (
               <ScrollableAnchor id={nomo}>
                  <div>
                     <h2>{nomo}</h2>
                     {grupo.subkategorioj.length > 0 ? (
                        <p>
                           Subcategories:{" "}
                           {grupo.subkategorioj
                              .map(k => <a href={`#${k}`}>{k}</a>)
                              .reduce(
                                 (lasta, sekva) => [lasta, ", ", sekva] as any
                              )}
                        </p>
                     ) : null}
                     {grupo.superkategorioj.length > 0 ? (
                        <p>
                           A subcategory of:{" "}
                           {grupo.superkategorioj
                              .map(k => <a href={`#${k}`}>{k}</a>)
                              .reduce((l, s) => [l, ", ", s] as any)}
                        </p>
                     ) : null}
                     <Vorttabelo vortoj={grupo.vortoj} />
                  </div>
               </ScrollableAnchor>
            );
         })}
      </div>
   ));
