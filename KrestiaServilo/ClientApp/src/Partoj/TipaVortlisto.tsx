import React from "react";
import { alportiĈiujnTipajn } from "../API";
import { Vorttabelo } from "./Vorttabelo";
import { APIKomponanto } from "./APIKomponanto";

type Ŝtato = "alportado" | "alportita" | "eraro";

export const TipoVortlisto = () =>
   APIKomponanto(alportiĈiujnTipajn, rezulto => (
      <div>
         {Object.entries(rezulto).map(valuo => (
            <div>
               <h2>{valuo[0]}</h2>
               <Vorttabelo vortoj={valuo[1]} />
            </div>
         ))}
      </div>
   ));
