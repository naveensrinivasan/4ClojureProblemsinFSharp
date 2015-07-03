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

(* 
https://www.4clojure.com/problem/67
Prime Numbers
 
Difficulty:	Medium
Topics:	primes


Write a function which returns the first x number of prime numbers.
test not run	

(= (__ 2) [2 3])
test not run	

(= (__ 5) [2 3 5 7 11])
test not run	

(= (last (__ 100)) 541)
*)

let prime x =
    let rec inner counter=
        match counter < x with
        |true when ((x % counter) <> 0) -> (inner (counter + 1))
        |true -> (x, false)
        | _ -> (x,(x % counter = 0))
    inner 2
let getprimes howmany =
    let rec inner start list=
        match  (List.length list) < howmany with
        |true ->
            let isprime = prime start
            if (snd isprime) then ( inner (start+ 1) (fst(isprime)::list)) else (inner (start+1) list)
        |_ -> list |> List.rev
    inner 2 []

getprimes 2 
getprimes 5
getprimes 100 |> List.last

(*
A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
test not run	

(= (__ 6) true)
test not run	

(= (__ 7) false)
test not run	

(= (__ 496) true)
test not run	

(= (__ 500) false)
test not run	

(= (__ 8128) true)

*)
let isPerfectNumber x =
    let rec getdivisors counter list=
        match counter < x with
        |true when (x % counter = 0) -> getdivisors (counter+1) (counter::list)
        |true -> getdivisors (counter+1) list
        |_ -> list
    let divisors = getdivisors 1 [] |> List.reduce (+)
    if (divisors= x) then true else false

isPerfectNumber 6
isPerfectNumber 7
isPerfectNumber 496

