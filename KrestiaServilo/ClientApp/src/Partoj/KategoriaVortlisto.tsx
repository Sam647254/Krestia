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
                     <Vorttabelo vortoj={grupo.vortoj} />
                  </div>
               </ScrollableAnchor>
            );
         })}
      </div>
   ));
