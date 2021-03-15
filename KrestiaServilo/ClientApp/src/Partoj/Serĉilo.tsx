import {useHistory} from "react-router-dom";
import React from "react";

export function Serĉilo() {
   const history = useHistory();
   const serĉiTeksto = React.createRef<HTMLInputElement>();

   return <form className="serĉilo">
      <input type="text" className="serĉiTeksto" ref={serĉiTeksto}/>
      <input type="submit" className="serĉiButono"
             value="Search"
             onClick={async event => {
                event.preventDefault();
                history.push(`/search/${serĉiTeksto.current!.value}`);
             }}/>
      <input type="submit" className="serĉiButono"
             onClick={async event => {
                event.preventDefault();
                history.push(`/parse/${btoa(serĉiTeksto.current!.value)}`)
             }}
             value="Parse"/>
   </form>;
}