module KrestiaParser.DictionaryHelper

open KrestiaParser.Decompose
open KrestiaParser.WordType
open KrestiaParser.Utils

let typeNameOf word =
   let decomposedWord =
      decomposeWord word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   match decomposedWord.baseType with
   | CountableNoun -> "Countable noun"
   | UncountableNoun -> "Uncountable noun"
   | CountableAssociativeNoun -> "Countable associative noun"
   | UncountableAssociativeNoun -> "Uncountable associative noun"
   | Record -> "Record"
   | Verb1 -> "1-Verb"
   | Verb2 -> "2-Verb"
   | Verb3 -> "3-Verb"
   | Verb12 -> "1-2-Verb"
   | Verb13 -> "1-3-Verb"
   | Verb23 -> "2-3-Verb"
   | Verb123 -> "1-2-3-Verb"
   | Verb0 -> "0-Verb"
   | Pronoun -> "Pronoun"
   | Modifier -> "Modifier"
   | Name -> "Identifier"
   | TerminalDigit -> "Terminal digit"
   | NonterminalDigit -> "Non-terminal digit"

let typeCategoryOf word =
   let decomposedWord =
      decomposeWord word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   match decomposedWord.baseType with
   | CountableNoun
   | UncountableNoun -> "Noun"
   | CountableAssociativeNoun
   | UncountableAssociativeNoun -> "Associative noun"
   | Record -> "Record"
   | Verb1
   | Verb2
   | Verb3
   | Verb12
   | Verb13
   | Verb23
   | Verb123
   | Verb0 -> "Verb"
   | Pronoun -> "Pronoun"
   | Modifier -> "Modifier"
   | Name -> "Identifier"
   | TerminalDigit -> "Terminal digit"
   | NonterminalDigit -> "Non-terminal digit"

let inflectionNameOf inflection =
   match inflection with
   | PredicativeIdentity -> "Predicative identity"
   | AttributiveIdentityPostfix -> "Attributive identity (postfix)"
   | AttributiveIdentityPrefix -> "Attributive identity (prefix)"
   | SpecificGerund -> "Specific gerund"
   | Possessive0 -> "Possessive"
   | Translative0 -> "Translative"
   | Argument1 -> "First argument"
   | Argument2 -> "Second argument"
   | Argument3 -> "Third argument"
   | _ -> inflection.ToString()

let stemOfWord word =
   let decomposedWord =
      decomposeWord word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   if isVerb decomposedWord.baseType then
      if decomposedWord.baseType <> Verb13 then
         word.Substring(0, word.Length - 2)
      else
         word.Substring(0, word.Length - 1)
   else
      word

let inflectedFormsOf word =
   let decomposedWord =
      decomposeWord word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   let suffixInflections =
      suffixesList
      |> List.choose
            (fun (WI (suffix, inflection, validTypes)) ->
               if List.contains decomposedWord.baseType validTypes then
                  Some(word + suffix, inflection)
               else
                  None)

   let additionalInflections =
      if isNoun decomposedWord.baseType then
         [ definiteToPredicative word, PredicativeIdentity ]
      else
         []

   List.append additionalInflections suffixInflections
   |> List.map (fun (w, i) -> (w, inflectionNameOf i))

let isReduced original current =
   let decomposedOriginal = decomposeWord original
   let decomposedCurrent = decomposeWord current

   Option.map2
      (fun original current ->
         if isVerb original.baseType
            && isVerb current.baseType then
            Map.tryFind original.baseType reducedForms
            |> Option.map (Set.contains current.baseType)
            |> Option.defaultValue false
         else
            false)
      decomposedOriginal
      decomposedCurrent
   |> Option.defaultValue false

let reduredFormsOf verb =
   option {
      let! decomposedVerb = decomposeWord verb
      if isVerb decomposedVerb.baseType then
         let! reducedTypes = Map.tryFind decomposedVerb.baseType reducedForms
         let stem = stemOfWord verb
         return
            reducedTypes
            |> Set.map (fun t ->
               let ending = Map.find t verbEndings
               stem + ending)
   }