(this["webpackJsonpkrestia-vortaro"]=this["webpackJsonpkrestia-vortaro"]||[]).push([[0],{30:function(e,n,o){e.exports=o(63)},35:function(e,n,o){},36:function(e,n,o){},55:function(e,n,o){},61:function(e,n,o){},62:function(e,n,o){},63:function(e,n,o){"use strict";o.r(n);var t=o(0),l=o.n(t),a=o(27),i=o.n(a),r=(o(35),o(36),o(5)),c=o(7);function u(){return l.a.createElement("div",null,l.a.createElement("p",null,"Krestia is an engineered language that I have started working on since late 2019."),l.a.createElement("p",null,"This website is the dictionary. You can use the search bar to look up words, even in their inflected forms."),l.a.createElement("p",null,"For the reference grammar, please see the"," ",l.a.createElement("a",{href:"http://5am.link/krestia-book"},"Krestia book.")))}var s=o(10),T=o(8),v=o.n(T),m=o(12),f=o(14),d=o.n(f);function p(){return(p=Object(m.a)(v.a.mark((function e(n){var o;return v.a.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.next=2,d.a.get("/trovi/".concat(n));case 2:return o=e.sent,e.next=5,o.data;case 5:return e.abrupt("return",e.sent);case 6:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function h(){return(h=Object(m.a)(v.a.mark((function e(n){var o;return v.a.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.next=2,d.a.get("/vorto/".concat(n));case 2:return o=e.sent,e.next=5,o.data;case 5:return e.abrupt("return",e.sent);case 6:case"end":return e.stop()}}),e)})))).apply(this,arguments)}o(55);function E(){var e=Object(c.g)().peto,n=Object(t.useState)(),o=Object(s.a)(n,2),a=o[0],i=o[1],u=Object(t.useState)("trovado"),T=Object(s.a)(u,2),v=T[0],m=T[1];if(Object(t.useEffect)((function(){m("trovado"),function(e){return p.apply(this,arguments)}(e).then((function(e){m("trovita"),i(e)}))}),[e]),"trovado"===v||null==a)return l.a.createElement("div",null,"Searching...");var f=function(e,n){if(null!=e.plenigitaVorto){var o=l.a.createElement(r.b,{to:"/word/".concat(e.plenigitaVorto)},e.plenigitaVorto);return null!=e.malinflektitaVorto?l.a.createElement("span",null,n," is an inflected form of a reduced form of"," ",o,"."):l.a.createElement("span",null,n," is a reduced form of ",o,".")}return null!=e.malinflektitaVorto?l.a.createElement("span",null,n," is an inflected form of"," ",l.a.createElement(r.b,{to:"/word/".concat(e.malinflektitaVorto)},e.malinflektitaVorto),"."):null}(a,e);return 0===a.rezultoj.length&&null==a.plenigitaVorto&&null==a.malinflektitaVorto?l.a.createElement("div",null,'No results for "',e,'".'):l.a.createElement("div",null,l.a.createElement("div",{className:"peto"},'Search results for "',e,'":'),null==f?null:l.a.createElement("div",null,l.a.createElement("span",null,"Note: ")," ",f),a.rezultoj.map((function(e){return l.a.createElement("div",{className:"rezulto",key:e.vorto},l.a.createElement("span",{className:"rezulto-vorto"},l.a.createElement(r.b,{to:"/word/".concat(e.vorto)},e.vorto)),l.a.createElement("span",{className:"rezulto-signifo"},e.signifo))})))}function b(){var e=Object(c.f)(),n=l.a.createRef();return l.a.createElement("form",{className:"ser\u0109ilo"},l.a.createElement("input",{type:"text",className:"ser\u0109iTeksto",ref:n}),l.a.createElement("input",{type:"submit",className:"ser\u0109iButono",value:"Search",onClick:function(){var o=Object(m.a)(v.a.mark((function o(t){return v.a.wrap((function(o){for(;;)switch(o.prev=o.next){case 0:t.preventDefault(),e.push("/search/".concat(n.current.value));case 2:case"end":return o.stop()}}),o)})));return function(e){return o.apply(this,arguments)}}()}))}o(61),o(62);var g=function(e,n,o,t,l){e.lineTo(t,l+n),e.lineTo(t+o,l+n)},k=function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l)},w=function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o,l+n)},j=function(e,n,o,t,l){e.moveTo(t+o,l),e.lineTo(t,l+n/2),e.lineTo(t+o,l+n)},N=new Map([["p",k],["b",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n)}],["m",g],["v",function(e,n,o,t,l){e.moveTo(t+o,l),e.lineTo(t,l),e.lineTo(t+o/2,l+n)}],["t",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t+o/2,l),e.lineTo(t+o/2,l+n)}],["d",function(e,n,o,t,l){o<10?(e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n)):(e.lineTo(t+o,l),e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.moveTo(t+2*o/3,l),e.lineTo(t+2*o/3,l+n))}],["n",function(e,n,o,t,l){e.moveTo(t+o/2,l),e.lineTo(t+o/2,l+n),e.moveTo(t,l+n),e.lineTo(t+o,l+n)}],["s",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t+o/2,l),e.lineTo(t,l+n),e.moveTo(t+o/2,l),e.lineTo(t+o,l+n)}],["l",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.lineTo(t,l+n),e.lineTo(t,l)}],["r",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o/2,l+n),e.lineTo(t,l)}],["j",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t+o/2,l),e.lineTo(t+o/2,l+n),e.moveTo(t,l+n),e.lineTo(t+o,l+n)}],["k",w],["g",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.moveTo(t+(o<10?o/5:2*o/3),l),e.lineTo(t+(o<10?o/5:2*o/3),l+n)}],["w",function(e,n,o,t,l){e.moveTo(t+o,l+n/2),e.ellipse(t+o/2,l+n/2,o/2,n/2,0,0,360)}],["h",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t,l+n),e.lineTo(t+o,l+n)}],["i",k],["e",function(e,n,o,t,l){e.lineTo(t,l+n),e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2)}],["a",g],["u",w],["o",function(e,n,o,t,l){e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2),e.moveTo(t+o,l),e.lineTo(t+o,l+n)}],["\u0252",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t+o,l+n),e.lineTo(t+o,l)}],["pl",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.lineTo(t+2*o/3,l+n),e.lineTo(t+2*o/3,l)}],["pr",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(3*t/4,l+n),e.lineTo(t/2,l)}],["bl",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.lineTo(t+2*o/3,l+n),e.lineTo(t+2*o/3,l),e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n)}],["br",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+5*o/6,l+n),e.lineTo(t+2*o/3,l),e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n)}],["tl",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.lineTo(t+2*o/3,l+n),e.lineTo(t+2*o/3,l)}],["tr",function(e,n,o,t,l){e.lineTo(t+o,l),e.moveTo(t+o/4,l),e.lineTo(t+o/2,l+n),e.lineTo(t+3*o/4,l)}],["dl",function(e,n,o,t,l){e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.lineTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.lineTo(t+2*o/3,l+n),e.lineTo(t+2*o/3,l)}],["dr",function(e,n,o,t,l){e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+2*o/3,l+n),e.lineTo(t+2*o/3,l)}],["kl",function(e,n,o,t,l){e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.lineTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n)}],["kr",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.moveTo(t,l),e.lineTo(t+o/4,l+n),e.lineTo(t+o/2,l)}],["gl",function(e,n,o,t,l){e.moveTo(t+o/3,l),e.lineTo(t+o/3,l+n),e.lineTo(t,l+n),e.lineTo(t,l),e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.moveTo(t+2*o/3,l),e.lineTo(t+2*o/3,l+n)}],["gr",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t+o,l+n),e.moveTo(t,l),e.lineTo(t+o/3,l+n),e.lineTo(t+2*o/3,l),e.moveTo(t+2*o/3,l),e.lineTo(t+2*o/3,l+n)}],["NombrigeblaKlaso",j],["NenombrigeblaKlaso",j],["Anta\u016dNenombrigeblaEco",function(e,n,o,t,l){e.moveTo(t+o,l+n),e.ellipse(t+o,l+n/2,o,n/2,0,Math.PI/2,1.5*Math.PI),e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2)}],["Anta\u016dNombrigeblaEco",function(e,n,o,t,l){e.moveTo(t+o,l),e.lineTo(t,l),e.lineTo(t,l+n),e.lineTo(t+o,l+n),e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2)}],["MalplenaVerbo",function(e,n,o,t,l){e.lineTo(t+o/2,l+n/2),e.lineTo(t+o,l),e.moveTo(t,l+n/2),e.lineTo(t+o/2,l+n),e.lineTo(t+o,l+n/2)}],["NetransitivaVerbo",function(e,n,o,t,l){e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2),e.moveTo(t+o/2,l),e.lineTo(t+o/2,l+n)}],["TransitivaVerbo",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t,l+n),e.lineTo(t+o,l+n)}],["DutransitivaVerbo",function(e,n,o,t,l){e.lineTo(t+o,l),e.lineTo(t,l+n),e.lineTo(t+o,l+n),e.moveTo(t,l+n/2),e.lineTo(t+o,l+n/2)}],["Lokokupilo",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t+o,l+n),e.lineTo(t+o,l),e.moveTo(t+o/2,l),e.lineTo(t+o/2,l+n)}],["Pridiranto",function(e,n,o,t,l){e.moveTo(t,l+n),e.lineTo(t+o/2,l),e.lineTo(t+o,l+n),e.lineTo(t,l+n)}]]),A=new Set(["i","e","a","u","o","\u0252"]);function O(e){var n=e.alteco,o=e.silaboj,a=l.a.createRef(),i=n/2,r=n+7;return Object(t.useEffect)((function(){var e=a.current.getContext("2d");e.lineCap="round",e.lineWidth=4.7,e.lineJoin="round",e.clearRect(0,0,e.canvas.width,e.canvas.height),e.beginPath();var n=5;o.forEach((function(o){var t,l,a,c,u;switch(e.moveTo(n,5),o.length){case 1:null===(t=N.get(o))||void 0===t||t.call(null,e,r,15,n,5),n-=8;break;case 2:var s,T,v,m;if(A.has(o.charAt(0)))null===(s=N.get(o.charAt(0)))||void 0===s||s.call(null,e,r,8,n,5),e.moveTo(n+8+7,5),null===(T=N.get(o.charAt(1)))||void 0===T||T.call(null,e,r,8,n+8+7,5);else null===(v=N.get(o.charAt(0)))||void 0===v||v.call(null,e,i,15,n,5),e.moveTo(n,5+i+7),null===(m=N.get(o.charAt(1)))||void 0===m||m.call(null,e,i,15,n,5+i+7),n-=8;break;case 3:var f,d,p,h,E;if(A.has(o.charAt(2)))null===(f=N.get(o.substr(0,2)))||void 0===f||f.call(null,e,i,23,n,5),e.moveTo(n,5+i+7),null===(d=N.get(o.charAt(2)))||void 0===d||d.call(null,e,i,23,n,5+i+7);else null===(p=N.get(o.charAt(0)))||void 0===p||p.call(null,e,i,23,n,5),e.moveTo(n,5+i+7),null===(h=N.get(o.charAt(1)))||void 0===h||h.call(null,e,i,8,n,5+i+7),e.moveTo(n+8+7,5+i+7),null===(E=N.get(o.charAt(2)))||void 0===E||E.call(null,e,i,8,n+8+7,5+i+7);break;case 4:null===(l=N.get(o.substr(0,2)))||void 0===l||l.call(null,e,i,23,n,5),e.moveTo(n,5+i+7),null===(a=N.get(o.charAt(2)))||void 0===a||a.call(null,e,i,8,n,5+i+7),e.moveTo(n+8+7,5+i+7),null===(c=N.get(o.charAt(3)))||void 0===c||c.call(null,e,i,8,n+8+7,5+i+7);break;default:null===(u=N.get(o))||void 0===u||u.call(null,e,r,46/3,n,5)}n+=30})),e.stroke()}),[i,a,r,o]),l.a.createElement("canvas",{className:"kanvaso",height:n+14+3,width:300,ref:a})}var S=new Map(Object.entries({Infinitivo:"Infinitive",Difinito:"Definite","Hava\u0135o":"Possession",Progresivo:"Progressive",Perfekto:"Perfect",Estonteco:"Intention",NominativoVolo:"Slot-1 volition",AkuzativoVolo:"Slot-2 volition",DativoVolo:"Slot-3 volition",PredikativoEsti:"Predicative identity","AtributivoEstiAnta\u016d":"Attributive identity (prefix)","AtributivoEstiMalanta\u016d":"Attributive identity (postfix)",Havado:"Possessive",Imperativo:"Imperative",Aganto:"Slot-1 argument",Patiento:"Slot-2 argument",Ekzistado:"Existential",Invito:"Hortative",Translativo:"Translative","\u011cerundo":"Gerund","Specifa\u011cerundo":"Specific gerund",PartaNominativo:"Slot-1 partial",PartaAkuzativo:"Slot-2 partial",PartaDativo:"Slot-3 partial",Igo:"Causative",Sola:"Standalone",Etigo:"Diminutive",Reflekcio:"Reflexive"}));function y(){var e=Object(c.g)().vorto,n=Object(t.useState)(),o=Object(s.a)(n,2),a=o[0],i=o[1],u=Object(t.useState)("alportado"),T=Object(s.a)(u,2),v=T[0],m=T[1];if(Object(t.useEffect)((function(){m("alportado"),function(e){return h.apply(this,arguments)}(e).then((function(e){i(e),m("alportita")})).catch((function(e){console.error(e.response),404===e.response.status?m("netrovita"):m("eraro")}))}),[e]),"alportado"===v)return l.a.createElement("div",null,"Loading...");if("netrovita"===v)return l.a.createElement("div",null,"Word not found: ",e);if("eraro"===v)return l.a.createElement("div",null,"A server-side error occurred when fetching ",e,".");if(null==a)throw new Error("Unreachable state");return l.a.createElement("div",null,l.a.createElement("h2",{className:"vorto"},a.vorto),l.a.createElement(O,{alteco:25,silaboj:a.silaboj}),l.a.createElement("div",{className:"streko"}),l.a.createElement("p",{className:"vorttipo"},a.vorttipo),l.a.createElement("p",null,a.signifo),a.noto.length>0?l.a.createElement("p",null,"Notes: ",a.noto):null,a.radikoj.length>0?l.a.createElement("p",null,"Roots:"," ",null===a||void 0===a?void 0:a.radikoj.map((function(e){return l.a.createElement(r.b,{to:"/word/".concat(e),key:e},e)})).reduce((function(e,n){return[e,", ",n]}))):null,l.a.createElement("h3",null,"Inflected forms"),l.a.createElement("table",{className:"inflekcio-tabelo"},l.a.createElement("thead",null,l.a.createElement("tr",null,l.a.createElement("th",null,"Inflection"),l.a.createElement("th",null,"Inflected form"))),l.a.createElement("tbody",null,Object.entries(a.inflektitajFormoj).map((function(e){var n=Object(s.a)(e,2),o=n[0],t=n[1];return l.a.createElement("tr",null,l.a.createElement("td",null,S.get(o)||o),l.a.createElement("td",null,t))})))))}var P=function(){return l.a.createElement(r.a,null,l.a.createElement("div",{className:"titolo"},l.a.createElement("h1",null,l.a.createElement(r.b,{to:"/"},"voliste vol Krestia")),l.a.createElement("p",{className:"subtitolo"},"Krestia dictionary")),l.a.createElement(b,null),l.a.createElement("div",{className:"enhavo"},l.a.createElement(c.c,null,l.a.createElement(c.a,{exact:!0,path:"/"},l.a.createElement(u,null)),l.a.createElement(c.a,{path:"/search/:peto"},l.a.createElement(E,null)),l.a.createElement(c.a,{path:"/word/:vorto"},l.a.createElement(y,null)))))};Boolean("localhost"===window.location.hostname||"[::1]"===window.location.hostname||window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));i.a.render(l.a.createElement(P,null),document.getElementById("root")),"serviceWorker"in navigator&&navigator.serviceWorker.ready.then((function(e){e.unregister()})).catch((function(e){console.error(e.message)}))}},[[30,1,2]]]);
//# sourceMappingURL=main.52e0ff82.chunk.js.map