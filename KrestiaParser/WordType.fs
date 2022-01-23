module KrestiaParser.WordType

type WordType =
   | CountableNoun
   | UncountableNoun
   | CountableAssociativeNoun
   | UncountableAssociativeNoun
   | Record
   | Verb1
   | Verb2
   | Verb3
   | Verb12
   | Verb13
   | Verb23
   | Verb123
   | Verb0
   | Pronoun
   | Modifier
   | Name
   | TerminalDigit
   | NonterminalDigit
   | All

type Inflection =
   | Definite
   | Possession
   | PredicativeIdentity
   | AttributiveIdentityPrefix
   | AttributiveIdentityPostfix
   | Gerund
   | SpecificGerund
   | Possessive
   | Possessive0
   | Existence
   | Translative
   | Translative0
   | Detached
   | Lone
   | Progressive
   | Perfect
   | Intention
   | Hypothetical
   | Desiderative
   | Imperative
   | Hortative
   | Optative
   | Argument1
   | Argument2
   | Argument3
   | Partial1
   | Partial2
   | Partial3
   | Shift2
   | Shift3
   | Commencement
   | Reflection
   | Reflection1
   | Reflection3
   | Reflection0
   | Postfixed
   | Quality
   | SingleForm
   | NameI
   | DigitI
   | Predicate

type WI = WI of (string * Inflection * WordType list)

let private wi suffix inflection wordTypes = WI(suffix, inflection, wordTypes)

let suffixesList =
   [ wi
      "nsa"
      Possession
      [ CountableNoun
        UncountableNoun
        CountableAssociativeNoun
        UncountableAssociativeNoun ]
     wi
        "va"
        AttributiveIdentityPrefix
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "ga"
        AttributiveIdentityPostfix
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "vra"
        SpecificGerund
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "re"
        Quality
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "ra"
        Lone
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "rem"
        Possessive0
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "res"
        Possessive
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "rim"
        Existence
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "lam"
        Translative0
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "las"
        Translative
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]

     wi "io" Perfect [ Verb0; Verb2; Verb3; Verb23 ]
     wi "ia" Hypothetical [ Verb0; Verb2; Verb3; Verb23 ]
     wi
        "ela"
        Intention
        [ Verb0
          Verb1
          Verb12
          Verb13
          Verb123
          Verb2
          Verb23
          Verb3 ]
     wi
        "ea"
        Gerund
        [ Verb0
          Verb12
          Verb123
          Verb2
          Verb3
          Verb23 ]
     wi "ro" Perfect [ Verb12; Verb123 ]
     wi "o" Perfect [ Verb1; Verb13 ]
     wi "e" Hypothetical [ Verb1; Verb13 ]
     wi "ora" Desiderative [ Verb1; Verb12; Verb13; Verb123 ]
     wi "ea" Imperative [ Verb1; Verb13 ]
     wi "ri" Imperative [ Verb12; Verb123 ]
     wi "ie" Optative [ Verb1; Verb13; Verb3 ]
     wi "ra" Optative [ Verb2 ]
     wi "ri" Optative [ Verb23 ]
     wi "a" Hortative [ Verb1; Verb13 ]
     wi "etie" Argument1 [ Verb1; Verb12; Verb13; Verb123 ]
     wi "onia" Argument2 [ Verb12; Verb2; Verb123; Verb23 ]
     wi "eri" Argument3 [ Verb123; Verb13; Verb3; Verb123 ]
     wi "mea" Gerund [ Verb1; Verb13 ]
     wi "elim" Commencement [ Verb0 ]
     wi "elit" Commencement [ Verb12 ]
     wi "elis" Commencement [ Verb1 ]
     wi "elish" Commencement [ Verb13 ]
     wi "elip" Commencement [ Verb123 ]
     wi "em" Partial1 [ Verb1 ]
     wi "ig" Partial1 [ Verb12 ]
     wi "ev" Partial1 [ Verb123 ]
     wi "es" Partial2 [ Verb12 ]
     wi "am" Partial2 [ Verb2 ]
     wi "on" Partial2 [ Verb23 ]
     wi "osh" Partial2 [ Verb123 ]
     wi "ut" Partial3 [ Verb123 ]
     wi "ig" Partial3 [ Verb23 ]
     wi "ris" Reflection [ Verb12 ]
     wi "ish" Reflection [ Verb123 ]
     wi "es" Reflection [ Verb13 ]
     wi "is" Reflection1 [ Verb123 ]
     wi "im" Reflection0 [ Verb123 ]
     wi "rim" Reflection0 [ Verb12 ]
     wi "ret" Shift2 [ Verb12 ]
     wi "rop" Shift2 [ Verb123 ]
     wi "rup" Shift3 [ Verb123 ]
     wi "rosh" Shift3 [ Verb13 ]
     wi "riv" Shift3 [ Verb23 ] ]