import React from "react";
import { Link } from "react-router-dom";

export function Ĉefpaĝo() {
   return (
      <div>
         <p>
            Krestia is an engineered language that I have started working on
            since late 2019. It is designed to be a formal language like Lojban
            that may be turned into a programming language, and has a
            minimalistic phonetic inventory.
         </p>
         <p>
            This website is the dictionary. You can use the search bar to look
            up words, even in their inflected forms. In addition, you can use
            the parser to parse one or more sentences. You can also view the
            full word list in the following formats:
         </p>
         <ul>
            <li>
               <Link to="/wordlist">Full alphabetical list</Link>
            </li>
            <li>
               <Link to="/typedlist">Alphabetical grouped by word type</Link>
            </li>
            <li>
               <Link to="/categorylist">
                  Alphabetical grouped by categories
               </Link>
            </li>
         </ul>
         {/*<p>*/}
         {/*   For the reference grammar, please see the{" "}*/}
         {/*   <a href="http://5am.link/krestia-book">Krestia book.</a>*/}
         {/*</p>*/}
         <div>
            <h2>Changelog</h2>
            <h3>v0.4 (TBD)</h3>
            <ul>
               <li>[Dictionary] Added contextual definitions for verbs</li>
               <li>[Dictionary] Added functionality to evaluate math expressions in the search bar</li>
               <li>[Dictionary] Looking up a multi-word number will additionally display the gloss</li>
               <li>[Parser] Fixed issue of numbers causing the parser to crash</li>
            </ul>
            <h3>v0.3.1 (2021/01/17)</h3>
            <ul>
               <li>[General] Corrected the horizontal rule's colour</li>
               <li>
                  [Parser] Corrected the bug in which modifiers' tooltips do not
                  display the meaning
               </li>
               <li>
                  [Dictionary] Corrected the bug in which a query with multiple
                  English words would bring up an empty gloss result instead of
                  showing the search results
               </li>
            </ul>
            <h3>v0.3 (2020/12/19)</h3>
            <ul>
               <li>Added the parser</li>
            </ul>
            <h3>v0.2.1 (2020/11/30)</h3>
            <ul>
               <li>Updated colour scheme</li>
            </ul>
            <h3>v0.2 (2020/10/09)</h3>
            <ul>
               <li>Added word lists grouped by word type and categories</li>
               <li>Added slot information to verbs</li>
               <li>Restructured the lexicon</li>
            </ul>
            <h3>v0.1 (2020/04/05)</h3>
            <ul>
               <li>Initial public release</li>
            </ul>
            <div className="streko" />
            <p>
               The content of the dictionary is released into the public domain.
               The{" "}
               <a
                  href="https://github.com/Sam647254/Krestia-vortaro"
                  target="_blank"
                  rel="noopener noreferrer"
               >
                  source code
               </a>{" "}
               is available under the{" "}
               <a
                  href="http://unlicense.org"
                  target="_blank"
                  rel="noopener noreferrer"
               >
                  Unlicense
               </a>
               .
            </p>
         </div>
      </div>
   );
}
