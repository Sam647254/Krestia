import {ReactElement, useEffect, useState} from "react";
import * as React from "react";

type Ŝtato = "alportado" | "alportita" | "eraro";

export function APIKomponanto<TProps>(
   alportilo: () => Promise<TProps>,
   komponanto: (props: TProps) => ReactElement<TProps>
) {
   const [ŝtato, setFetchState] = useState<Ŝtato>("alportado");
   const [rezulto, setResult] = useState<TProps | undefined>();

   useEffect(() => {
      setFetchState("alportado");
      alportilo()
         .then(rezulto => {
            setResult(rezulto);
            setFetchState("alportita");
         })
         .catch(eraro => {
            setFetchState("eraro");
            console.error(eraro);
         });
   }, [alportilo]);

   if (ŝtato === "alportado") return <div>Loading</div>;
   if (ŝtato === "eraro")
      return <div>An error occurred while fetching the word list.</div>;
   if (rezulto == null) throw new Error("Unreachable state");

   return komponanto(rezulto);
}
