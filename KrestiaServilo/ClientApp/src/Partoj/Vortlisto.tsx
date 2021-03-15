import * as React from "react";
import { alportiĈiujn } from "../API";
import "./Vortlisto.scss";
import { Vorttabelo } from "./Vorttabelo";
import { APIKomponanto } from "./APIKomponanto";

type Ŝtato = "alportado" | "alportita" | "eraro";

export const Vortlisto = () =>
   APIKomponanto(alportiĈiujn, rezulto => (
      <div>
         <p className="centro">
            The dictionary currently holds {rezulto.length} words.
         </p>
         <Vorttabelo vortoj={rezulto} />
      </div>
   ));
