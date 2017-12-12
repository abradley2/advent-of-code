module Day4 exposing (solution)
import Inputs.Day4Inputs exposing (passphrases)
import Debug exposing (log)
import Regex exposing (regex)


splitToWords sentence = Regex.split Regex.All (Regex.regex "\\s+") sentence

getUniqueWordList wordList =
  List.foldr (\cur acc ->
    if List.member cur acc then acc
    else List.append [cur] acc
  ) [] wordList

allWordsInSentenceAreUnique sentence =
  let
    wordList = splitToWords sentence
  in
    (List.length (getUniqueWordList wordList)) == List.length wordList

answer = passphrases
  |> String.split "\n"
  |> List.filter allWordsInSentenceAreUnique
  |> List.length

solution = log ("\n\n" ++ toString answer ++ "\n\n")
