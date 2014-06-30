open System;
(*
http://4clojure.com/problem/92

Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. 
Write a function to parse a Roman-numeral string and return the number it represents. 

You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle. 
You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.
    
(= 14 (__ "XIV"))
    
(= 827 (__ "DCCCXXVII"))
    
(= 3999 (__ "MMMCMXCIX"))
    
(= 48 (__ "XLVIII"))

*)


let numbers = Map.ofList [('I',1);('V',5);('X',10);('L',50);('C',100);('D',500);('M',1000)] 
let readromannumerals (s:string) =
  Array.foldBack(fun ele (prevalue,total) -> 
        let currentvalue = numbers.Item ele 
        if currentvalue >= prevalue then (currentvalue, total+currentvalue) else (currentvalue ,total - currentvalue))
        (s.ToCharArray()) (0,0) 
        |> snd
 

readromannumerals "XIV"
readromannumerals "DCCCXXVII"
readromannumerals "MMMCMXCIX"
readromannumerals "XLVIII"

(*

Filter Perfect Squares
Given a string of comma separated integers, write a function which returns a 
new comma separated string that only contains the numbers which are perfect squares.

(= (__ "4,5,6,7,8,9") "4,9")

(= (__ "15,16,25,36,37") "16,25,36") 

*)

let sqr  = ( Int32.Parse >> float >> Math.Sqrt)
let filterPerfectSquares (s:string) =
 String.Join(",", s.Split [|','|] |> Array.filter(fun x ->
                                            let sqrroot = sqr x 
                                            sqrroot > Math.Truncate sqrroot |> not))
filterPerfectSquares "15,16,25,36,37"                           

(*
http://www.4clojure.com/problem/53
Longest Increasing Sub-Seq
 
Difficulty:	Hard
Topics:	seqs

Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. 
If two sub-sequences have the same length, use the one that occurs first. 
An increasing sub-sequence must have a length of 2 or greater to qualify.

(= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (__ [5 6 1 3 2 7]) [5 6])
(= (__ [2 3 3 4 5]) [3 4 5])
(= (__ [7 6 5 4]) [])

*)

let rec longestincreasingsubseq l innerlist (previousnumber:Option<int>) lastsuccess=
    match l with
    |head::tail  when previousnumber.IsNone -> longestincreasingsubseq tail innerlist  (Some(head)) lastsuccess
    |head::tail  when previousnumber.Value = (head - 1)  -> 
        longestincreasingsubseq tail ((previousnumber.Value::innerlist.Head)::innerlist.Tail)  (Some(head)) true
    |head::tail  -> match lastsuccess  with
                      |true -> longestincreasingsubseq tail ([]::(previousnumber.Value::innerlist.Head)::innerlist.Tail) (Some(head)) false
                      |_ -> longestincreasingsubseq tail ([]::innerlist) (Some(head)) false
    |_ -> 
        let list =  match lastsuccess  with
                      |true  -> (previousnumber.Value::innerlist.Head)::innerlist.Tail
                      |_ -> innerlist
        list |> 
        List.map(fun x -> List.rev x) |>
        List.maxBy(fun x-> List.length x) 

let seq = [1;0;1;2;3;0;4;5]
printfn "%A" (longestincreasingsubseq seq [[]] None false)
let seq1 = [5;6;1;3;2;7]
printfn "%A"(longestincreasingsubseq seq1 [[]] None false)
let seq2 = [2;3;3;4;5]
printfn "%A"(longestincreasingsubseq seq2 [[]] None false)
let seq3 = [7;6;5;4]
printfn "%A" (longestincreasingsubseq seq3 [[]] None false)
(* 
http://www.4clojure.com/problem/77

Anagram Finder
 
Difficulty:	Medium
Topics:	
Write a function which finds all the anagrams in a vector of words. 
A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. 
Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. 
Each sub-set should have at least two words. Words without any anagrams should not be included in the result.
	

(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
	

(= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})


*)

let rec anagramfider (l:seq<string>)=
         l|> Seq.map(fun s -> ((s.ToCharArray() 
                                |> Array.map(fun x -> Convert.ToInt32(x)) 
                                |> Array.fold(fun acc x -> acc+x) 0 )),s)
           |> Seq.groupBy fst
           |> Seq.filter( fun x ->  (Seq.length (snd(x))) > 1)
           |> Seq.map(fun x -> snd(x) |> Seq.map(fun y -> snd(y)))

let anagrams = ["meat";"mat";"team";"mate";"eat"]
let anagrams2 = ["veer";"lake";"item";"kale";"mite";"ever"]
printfn "%A" (anagramfider anagrams) 
printfn "%A" (anagramfider anagrams2) 
