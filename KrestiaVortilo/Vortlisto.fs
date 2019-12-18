﻿namespace KrestiaVortilo

module Vortlisto =
   type Vorto =
   | Klaso of string * Animeco : bool * Radikoj : string list * Signifo : string
   | Verbo of string * Valenco : int * Radikoj : string list * Signifo : string
   | Pridiranto of string * Radikoj : string list * Signifo : string
   | Lokokupilo of string * Signifo : string

   let listo: Vorto list = [
      Klaso("fosmaa", false, [], "earth (ground)")
      Klaso("lugrismaa", false, [], "earth (landscape)")
      Klaso("kunaa", false, [], "water (positive)")
      Klaso("gonaa", false, [], "water (negative)")
      Klaso("tretaa", false, [], "stone (large bulky)")
      Klaso("sestu", false, [], "stone")
      Klaso("dretu", false, [], "stone (pebble)")
      Klaso("trenaa", false, [], "stone (substance)")
      Klaso("kresku", false, [], "flame (fervidity)")
      Klaso("pospu", false, [], "flame (destructive)")
      Klaso("kresmu", false, [], "fire (fervidity)")
      Klaso("posmu", false, [], "fire (destructive)")
      Klaso("dutaa", false, [], "sand (grain of sand)")
      Klaso("vilto", false, [], "sand (grain of dust)")
      Klaso("dunaa", false, [], "sand (flowing)")
      Klaso("vilmo", false, [], "sand (dust)")
      Klaso("rismo", false, [], "smoke (destructive)")
      Klaso("rinemaa", false, [], "smoke (sign of life)")
      Klaso("ritmaa", false, [], "ash (desolate)")
      Klaso("tretkutaa", false, ["tretaa"; "kunaa"], "ice (large)")
      Klaso("sestutaa", false, ["sestu"; "kunaa"], "ice (ice cube)")
      Klaso("seskonaa", false, ["sestu"; "gonaa"], "ice (cold, desolate)")
      Klaso("skunaa", false, ["kunaa"], "ice (purity, clarity)")
      Klaso("lito", false, [], "night (period)")
      Klaso("lirano", false, [], "night (tranquil)")
      Klaso("rititino", false, [], "night (frightening)")
      Klaso("mikaa", false, [], "daytime (period)")
      Klaso("miginaa", false, [], "daytime")
      Klaso("migimataa", false, [], "year")
      Klaso("migimanaa", false, [], "year")
      Klaso("luvitaa", false, [], "rain (raindrop)")
      Klaso("luvemaa", false, [], "rain")
      Klaso("luveritaa", false, ["luveritmaa"], "cloud (ominous)")
      Klaso("lunevikaa", false, ["luverinemaa"], "cloud")
      Klaso("luveritmaa", false, ["luvemaa"; "ritmaa"], "cloud (ominous)")
      Klaso("luverinemaa", false, ["luvemaa"; "rinemaa"], "cloud")
      Klaso("ritmanaa", false, ["ritmaa"; "skunaa"], "fog")
      Klaso("velitaa", false, [], "wind (breeze)")
      Klaso("geluko", false, [], "wind (gust)")
      Klaso("velino", false, [], "wind (breeze)")
      Klaso("geluno", false, [], "wind")
      Klaso("salumu", false, [], "wind (strong)")
      Klaso("kulemu", false, [], "hurricane")
      Klaso("lustaa", false, [], "star")
      Klaso("lunaa", false, [], "sun")
      Klaso("rimepo", false, [], "moon (satellite)")
      Klaso("rimaa", false, [], "moon (the moon)")
      Klaso("gremu", false, [], "sky")
      Klaso("jamataa", false, [], "mountain")
      Klaso("druretaa", false, [], "road")
      Klaso("drurenaa", false, [], "road (symbol)")
      Klaso("brekaa", false, [], "woods (small)")
      Klaso("bretako", false, [], "woods (vast)")
      Klaso("prismataa", false, [], "lake")
      Klaso("gritaa", false, [], "river (stream)")
      Klaso("pristaa", false, ["prismataa"], "river (creek)")
      Klaso("penataa", false, [], "river")
      Klaso("kunataa", false, ["kunaa"], "sea (small)")
      Klaso("kunagretu", false, ["kunaa"; "gremu"], "sea (vast)")
      Klaso("lukukrestu", false, ["lugrismaa"; "kunaa"; "kresmu"; "sestu"], "body")
      Klaso("alipo", false, [], "head (body)")
      Klaso("kelto", false, [], "shoulder")
      Klaso("relalipo", false, ["rel"; "alipo"], "neck")
      Klaso("gerato", false, [], "hand")
      Klaso("fosmato", false, ["fosmaa"; "gerato"], "foot")
      Klaso("pelano", false, [], "skin")
      Klaso("fosrameko", false, ["fosralato"; "gerameko"], "knee")
      Klaso("fosralato", false, ["fosmato"; "geralato"], "leg")
      Klaso("geralato", false, ["gerato"], "arm")
      Klaso("gerameko", false, ["gerato"], "elbow")
      Klaso("geto", false, ["gerato"], "limb")
      Klaso("gerano", false, ["gerano"], "limbs")
      Klaso("elapo", false, [], "chest")
      Klaso("relelapo", false, ["rel"; "elapo"], "belly")
      Klaso("lukumataa", false, ["lukukrestu"; "jamataa"], "back")
      Klaso("kunipu", false, [], "fingernail")
      Klaso("lepitaa", false, [], "cheek")
      Klaso("riretu", false, [], "ear")
      Klaso("mevitaa", false, [], "eye")
      Klaso("prelino", false, [], "hair")
      Klaso("buvipo", false, [], "mouth")
      Klaso("nuketu", false, [], "nose")
      Klaso("buvipeto", false, ["buvito"], "tongue")
      Klaso("buvisko", false, ["buvipo"], "tooth")
      Klaso("buvismo", false, ["buvipo"], "teeth")
      Klaso("lukukunamaa", false, ["lukukrestu"; "kunaa"], "blood")
      Klaso("lukurenaa", false, ["lukukrestu"; "drurenaa"], "blood (ancestry)")
      Klaso("lukuto", false, ["lukukrestu"], "bone")
      Klaso("lukuritaa", false, ["lukukrestu"; "ritmaa"], "bone (symbol of death)")
      Klaso("lukumu", false, ["lukukrestu"], "meat/flesh")
      Klaso("girismo", false, [], "liver")
      Klaso("krestumu", false, [], "guts")
      Verbo("erafos", 1, ["eras"; "fosmaa"], "die")
      Verbo("erakunas", 1, ["eras"; "kunaa"], "die")
      Verbo("greras", 1, ["gremu"; "eras"], "die")
      Verbo("lireras", 1, ["lirano"; "eras"], "die")
      Verbo("ritmas", 1, ["ritmaa"], "die")
      Verbo("liversmas", 1, ["lukufosmas"; "liveras"], "die")
      Verbo("krevip", 2, [], "kill")
      Verbo("livos", 1, [], "sleep (nap)")
      Verbo("liveras", 1, [], "sleep")
      Verbo("veris", 1, [], "live (be alive)")
      Verbo("bemos", 1, [], "live (through everyday)")
      Verbo("emeras", 1, ["hem"; "eras"], "come")
      Verbo("eras", 2, [], "go to")
      Verbo("eramatas", 2, ["eraso"; "fosmato"], "walk to")
      Verbo("gremeras", 2, ["gremu"; "eras"], "fly to")
      Verbo("kumatas", 2, ["kunaa"; "eramatos"], "swim to")
      Verbo("kuneras", 2, ["kunaa"; "eras"], "float to")
      Verbo("kuvos", 1, ["kunaa"; "novos"], "float")
      Verbo("neras", 1, [], "turn around")
      Verbo("kemis", 1, [], "stand")
      Verbo("rumis", 1, [], "sit")
      Verbo("novos", 1, [], "lie (down)")
      Verbo("gedas", 2, [], "fall towards")
      Verbo("buvitok", 2, ["buvipo"], "eat (gentle)")
      Verbo("getisop", 2, [], "eat (chomp)")
      Verbo("buvisok", 2, ["buvipo"], "drink (gentle)")
      Verbo("getisip", 2, [], "drink (gulp)")
      Verbo("buvik", 2, ["buvitok"], "bite (gentle)")
      Verbo("getik", 2, ["getisop"], "bite (harsh)")
      Verbo("rudas", 1, [], "breathe (gentle)")
      Verbo("dulavis", 1, [], "breathe (deep)")
      Verbo("getrulis", 1, [], "breathe (blow)")
      Verbo("mevit", 2, ["mevitaa"], "see (glance)")
      Verbo("merat", 2, [], "see (look at)")
      Verbo("merisot", 2, [], "watch")
      Verbo("gremerisot", 2, ["gremu,merisot"], "watch over")
      Verbo("perat", 2, ["merat"], "stare at")
      Lokokupilo("wel", "something/someone")
      Lokokupilo("wil", "anything/anyone")
      Lokokupilo("hem", "I")
      Lokokupilo("hes", "you (singular)")
      Lokokupilo("het", "third person indefinite (singular)")
      Lokokupilo("heti", "third person animate (singular)")
      Lokokupilo("heta", "third person inanimate (singular)")
      Lokokupilo("hime", "we")
      Lokokupilo("hise", "you (plural)")
      Lokokupilo("hite", "third person indefinite (plural)")
      Lokokupilo("hiti", "third person animate (plural)")
      Lokokupilo("hita", "third person inanimate (plural)")
      Lokokupilo("hori", "everyone")
      Lokokupilo("hemse", "you and I")
      Lokokupilo("wen", "last nominative")
      Lokokupilo("won", "last marked class")

      Klaso("kunapaa", false, [ "kunaa" ], "well (water)")

      Pridiranto("kred", [], "that")
      Pridiranto("tred", [], "this") ]

   let vortaro =
      listo
      |> List.map (fun vorto ->
         match vorto with
         | Klaso(klaso, _, _, _) as v -> (klaso, v)
         | Verbo(verbo, _, _, _) as v -> (verbo, v)
         | Pridiranto(pridiranto, _, _) as v -> (pridiranto, v)
         | Lokokupilo(lokokupilo, _) as v -> (lokokupilo, v))
      |> Map

   let ĉuEkzistas (vorto) = vortaro.ContainsKey(vorto)

   let valenco (verbo) =
      vortaro.TryFind(verbo)
      |> Option.bind (fun verbo ->
         match verbo with
         | Verbo(_, valenco, _, _) -> Some valenco
         | _ -> None)