import React from "react";
import { VortoRespondo } from "../API";
import { Link } from "react-router-dom";

import "./Vorttabelo.scss";

interface Props {
   vortoj: VortoRespondo[];
}

export function Vorttabelo({ vortoj }: Props) {
   return (
      <table className="vortlisto">
         <thead>
            <tr>
               <th>Word</th>
               <th>Meaning</th>
            </tr>
         </thead>
         <tbody>
            {vortoj.map(v => (
               <tr>
                  <td>
                     <Link to={`/word/${v.vorto}`}>{v.vorto}</Link>
                  </td>
                  <td>{v.signifo}</td>
               </tr>
            ))}
         </tbody>
      </table>
   );
}
