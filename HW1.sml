(* 
Tested on Windows 10 64-bit
*)

(*
Works: 
This boolean function takes in a value x and a list y and checks to see if x is in y
*)

fun inList (x,y) = if(null y) then false else if (hd(y) = x) then 
true else inList(x, tl y);

(* b: The type is (''a * ''a list) -> bool and not ('a * 'a list) -> bool because
(''a * ''a list) represents only equality testing are allowed. The list needs 
to be something that is comparable. As opposed to ('a * 'a list) that represents lists of all types. 

''a is a type variable that can only be substituted by types that support
equality testing
'a can be any type of all. 

*)

(*
Works:
This function takes in a single list argument and checks for duplicate values. It returns a list without the duplicate values
*)

fun removeDuplicates [] = [] | removeDuplicates (x::rest) = if(inList(x, rest))
then removeDuplicates rest else x::removeDuplicates rest;


(*
Works:
This function is expected to take in two lists and output a list with only the intercepted values. It uses inList to check for the
intercepted values and calls removeDuplicate to remove the repeated values. 
*)


fun listIntersect (x,y) = 
if(null x) then [] else if (null y) then [] else if (inList(hd(x),y)) then
removeDuplicates(hd(x)::(listIntersect (tl(x),y))) else listIntersect (tl(x),y);



(*
Works:
This function takes three int arguments, a min, step, and max. It will create a list with min+step values until it reaches the max.
It excludes the max. Each iteration of this function will increase the min so that it can add up the values properly.
*)

fun range min step max = if  ( (step = 0) orelse (min >= max andalso step >= 0) 
orelse (min <= max andalso step <= 0) ) then [] else min::range (min+step) step max;



(*
Works:
This function will take in a int value and a int list and output only the elements of the list that add up to 
less than the sum argument. In this function, each iteration will decrease the sum argument by the first element of the list
and then reiterate until sum is less than or equal to zero
*)

fun numbersToSum sum L = if(L = [] orelse (sum - hd(L)) <= 0) then [] else
(hd L) ::(numbersToSum (sum - (hd L) ) (tl L) );



(*
Works:
This function will iterate through a list until it reaches the specified index and replace that value in the list with 
the given argument value. This function reduce the argument n by 1 for every iteration and repeat until n equals 0;
*)

fun replace n v []= []
|   replace n v (x::rest) =  if(n=0) then (v::rest) else x::(replace (n-1) v rest);



(* 
Works:
Included below are the groupN helper functions. Each function has a single purpose. The groupNleft is expected to take a integer
and a list and group them based on the integer using a left handed rule. The same is expected on the groupNright that will group 
them based on the integer and the right handed rule. Both helper functions made use of the reverse and reverseAppend functions. 
Reverse will call reverseAppend and reverse the list argument. 
*)

fun reverseAppend ([], L) = L | reverseAppend((x::rest), L) = reverseAppend(rest,x::L);

fun reverse L = if null L then [] else (reverseAppend (hd(L), []))::(reverse(tl L));
  

fun groupNrightHelper ([],buf,v) = [buf] | groupNrightHelper ((x::rest),buf,v) = if (length(buf) = v) then 
buf::(groupNrightHelper (rest,[x],v)) else (groupNrightHelper (rest,(x::buf),v));


fun groupNleftHelper ([],buf,v) = [buf] | groupNleftHelper ((x::rest),buf,v) = if (length(buf) = v) then 
buf::(groupNleftHelper (rest,[x],v)) else (groupNleftHelper (rest,(x::buf),v));



fun groupNright v L = if(null L orelse v = 0) then [] else reverse(groupNrightHelper(L,[],v));


fun groupNleft v L = if(null L orelse v = 0) then [] else reverseAppend(groupNleftHelper(reverseAppend(L,[]),[],v), []);


 
 (*--------------Test Functions----------------*)
 
fun myTest_inList () = 
let
	val inListT1 = (inList(8, [7]) = false)
	val inListT2 = (inList(1, []) = false)
	val inListT3 = (inList(1, [1,2,3]) = true)
	val inListT4 = (inList([1], [[1]]) = true)
	val inListT5 = (inList([1], [[3],[5]]) = false)
	val inListT6 = (inList("c", ["b", "c", "z"]) = true)
	val inListT7 = (inList("d", ["b", "c", "z"]) = false)
	val inListT8 = (inList("k", ["b", "c", "k"]) = true)
in 
	print ("\n------------\n inListTester: \n" ^
	"test1: " ^ Bool.toString(inListT1) ^ "  " ^
	"test2: " ^ Bool.toString(inListT2) ^ "  " ^
	"test3: " ^ Bool.toString(inListT3) ^ "  " ^
	"test4: " ^ Bool.toString(inListT4) ^ "  " ^
	"test5: " ^ Bool.toString(inListT5) ^ "  " ^
	"test6: " ^ Bool.toString(inListT6) ^ "  " ^
	"test7: " ^ Bool.toString(inListT7) ^ "  " ^
	"test8: " ^ Bool.toString(inListT8) ^ "\n")
end;
 
 
 
 
fun myTest_removeDuplicates () = 
let
	val removeDuplicatesT1 = ( (removeDuplicates [1, 5, 1, 3, 4, 3, 5]) = [1,4,3,5])
	val removeDuplicatesT2 = ( (removeDuplicates ["a", "e", "c", "a", "a", "b", "c", "d"]) = ["e","a","b","c","d"])
	val removeDuplicatesT3 = ( (removeDuplicates [] )  = [])
	val removeDuplicatesT4 = ( (removeDuplicates [1,2,3,4,5]) = [1,2,3,4,5])
	val removeDuplicatesT5 = ( (removeDuplicates [5,5,5,5,5]) = [5])
	val removeDuplicatesT6 = ( (removeDuplicates [1,4,5,3,3,3]) = [1,4,5,3])
	val removeDuplicatesT7 = ( (removeDuplicates [1,1,1,1,2]) = [1,2])
in 
	print ("\n------------\n removeDuplicatesTester: \n" ^
	"test1: " ^ Bool.toString(removeDuplicatesT1) ^ "  " ^
	"test2: " ^ Bool.toString(removeDuplicatesT2) ^ "  " ^
	"test3: " ^ Bool.toString(removeDuplicatesT3) ^ "  " ^
	"test4: " ^ Bool.toString(removeDuplicatesT4) ^ "  " ^
	"test5: " ^ Bool.toString(removeDuplicatesT5) ^ "  " ^
	"test6: " ^ Bool.toString(removeDuplicatesT6) ^ "  " ^
	"test7: " ^ Bool.toString(removeDuplicatesT7) ^ "\n")
end; 
 
  


fun myTest_listIntersect () = 
let
	val listIntersectT1 = ( (listIntersect ([1],[1]) ) = [1])
	val listIntersectT2 = ( (listIntersect([1,2,3],[1,1,2]) ) = [1,2])
	val listIntersectT3 = ( (listIntersect([[2,3],[1,2],[2,3]],[[1],[2,3]]) ) = [[2,3]])
	val listIntersectT4 = ( (listIntersect([1,2,3],[]) ) = [])
	val listIntersectT5 = ( (listIntersect([], []) ) = [])
	val listIntersectT6 = ( (listIntersect(["t","w","b"], ["b", "c", "z"]) ) = ["b"])
	val listIntersectT7 = ( (listIntersect([1,1,1,1], [2,2,2,2]) ) = [])
	val listIntersectT8 = ( (listIntersect([1], [2,3,6,2,1]) ) = [1])
	val listIntersectT9 = ( (listIntersect([6,8,3,2], [4,5,1,0]) ) = [])
in 
	print ("\n------------\n listIntersectTester: \n" ^
	"test1: " ^ Bool.toString(listIntersectT1) ^ "  " ^
	"test2: " ^ Bool.toString(listIntersectT2) ^ "  " ^
	"test3: " ^ Bool.toString(listIntersectT3) ^ "  " ^
	"test4: " ^ Bool.toString(listIntersectT4) ^ "  " ^
	"test5: " ^ Bool.toString(listIntersectT5) ^ "  " ^
	"test6: " ^ Bool.toString(listIntersectT6) ^ "  " ^
	"test7: " ^ Bool.toString(listIntersectT7) ^ "  " ^
	"test8: " ^ Bool.toString(listIntersectT8) ^ "  " ^
	"test9: " ^ Bool.toString(listIntersectT9) ^ "\n")
end;


fun myTest_range () = 
let
	val rangeT1 = ( (range 0 5 30) = [0,5,10,15,20,25])
	val rangeT2 = ( (range 5 ~1 0) = [5,4,3,2,1])
	val rangeT3 = ( (range 10 1 10) = [])
	val rangeT4 = ( (range 1 ~1 10) = [])
	val rangeT5 = ( (range 2 1 7) = [2,3,4,5,6])
	val rangeT6 = ( (range 5 ~2 0) = [5,3,1])
	val rangeT7 = ( (range 0 0 0) = [])
	val rangeT8 = ( (range 4 10 0) = [])
in 
	print ("\n------------\n rangeTester: \n" ^
	"test1: " ^ Bool.toString(rangeT1) ^ "  " ^
	"test2: " ^ Bool.toString(rangeT2) ^ "  " ^
	"test3: " ^ Bool.toString(rangeT3) ^ "  " ^
	"test4: " ^ Bool.toString(rangeT4) ^ "  " ^
	"test5: " ^ Bool.toString(rangeT5) ^ "  " ^
	"test6: " ^ Bool.toString(rangeT6) ^ "  " ^
	"test7: " ^ Bool.toString(rangeT7) ^ "  " ^
	"test8: " ^ Bool.toString(rangeT8) ^ "\n")
end;



fun myTest_numbersToSum () = 
let
	val numbersToSumT1 = ( (numbersToSum 100 [10, 20, 30, 40] ) = [10, 20, 30])
	val numbersToSumT2 = ( (numbersToSum 30 [5, 4, 6, 10, 4, 2, 1, 5] ) = [5, 4, 6, 10, 4])
	val numbersToSumT3 = ( (numbersToSum 1 [2] ) = [])
	val numbersToSumT4 = ( (numbersToSum 1 [] ) = [])
	val numbersToSumT5 = ( (numbersToSum 0 [5,6,7,8] ) = [])
	val numbersToSumT6 = ( (numbersToSum 20 [5,5,5,5,5] ) = [5,5,5])
	val numbersToSumT7 = ( (numbersToSum 1 [2,5,3] ) = [])
	val numbersToSumT8 = ( (numbersToSum 1 [0,0,0] ) = [0,0,0])
in 
	print ("\n------------\n numbersToSumTester: \n" ^
	"test1: " ^ Bool.toString(numbersToSumT1) ^ "  " ^
	"test2: " ^ Bool.toString(numbersToSumT2) ^ "  " ^
	"test3: " ^ Bool.toString(numbersToSumT3) ^ "  " ^
	"test4: " ^ Bool.toString(numbersToSumT4) ^ "  " ^
	"test5: " ^ Bool.toString(numbersToSumT5) ^ "  " ^
	"test6: " ^ Bool.toString(numbersToSumT6) ^ "  " ^
	"test7: " ^ Bool.toString(numbersToSumT7) ^ "  " ^
	"test8: " ^ Bool.toString(numbersToSumT8) ^ "\n")
end;






fun myTest_replace () = 
let
	val replaceT1 = ( (replace 3 40 [1, 2, 3, 4, 5, 6] ) = [1,2,3,40,5,6])
	val replaceT2 = ( (replace 0 "X" ["a", "b", "c", "d"] ) = ["X","b","c","d"])
	val replaceT3 = ( (replace 4 false [true, false, true, true, true] ) = [true,false,true,true,false])
	val replaceT4 = ( (replace 20 50 [1,2,3,4,5] ) = [1,2,3,4,5])
	val replaceT5 = ( (replace 4 50 [1,2,3,4,5] ) = [1,2,3,4,50])
	val replaceT6 = ( (replace 2 50 [1,2,3,4,5] ) = [1,2,50,4,5])
	val replaceT7 = ( (replace 3 50 [1,2,3,4,5] ) = [1,2,3,50,5])
in 
	print ("\n------------\n replaceTester: \n" ^
	"test1: " ^ Bool.toString(replaceT1) ^ "  " ^
	"test2: " ^ Bool.toString(replaceT2) ^ "  " ^
	"test3: " ^ Bool.toString(replaceT3) ^ "  " ^
	"test4: " ^ Bool.toString(replaceT4) ^ "  " ^
	"test5: " ^ Bool.toString(replaceT5) ^ "  " ^
	"test6: " ^ Bool.toString(replaceT6) ^ "  " ^
	"test7: " ^ Bool.toString(replaceT7) ^ "\n")
end;




fun myTest_groupNleft () = 
let
	val groupNleftT1 = ( (groupNleft 2 [1, 2, 3, 4, 5] ) = [[1],[2,3],[4,5]])
	val groupNleftT2 = ( (groupNleft 3 [1, 2, 3, 4, 5] ) = [[1,2],[3,4,5]])
	val groupNleftT3 = ( (groupNleft 2 [1,2,3,4,5] ) = [[1], [2,3], [4,5]])
	val groupNleftT4 = ( (groupNleft 4 [1,2,3,4,5] ) = [[1], [2,3,4,5]])
	val groupNleftT5 = ( (groupNleft 1 [1,2,3,4,5] ) = [[1], [2], [3], [4], [5]])
	val groupNleftT6 = ( (groupNleft 5 [1,2,3,4,5] ) = [[1,2,3,4,5]])
in 
	print ("\n------------\n groupNleftTester: \n" ^
	"test1: " ^ Bool.toString(groupNleftT1) ^ "  " ^
	"test2: " ^ Bool.toString(groupNleftT2) ^ "  " ^
	"test3: " ^ Bool.toString(groupNleftT3) ^ "  " ^
	"test4: " ^ Bool.toString(groupNleftT4) ^ "  " ^
	"test5: " ^ Bool.toString(groupNleftT5) ^ "  " ^
	"test6: " ^ Bool.toString(groupNleftT6) ^ "\n") 
end;




fun myTest_groupNright () = 
let
	val groupNrightT1 = ( (groupNright 2 [1, 2, 3, 4, 5] ) = [[1, 2], [3, 4], [5]])
	val groupNrightT2 = ( (groupNright 3 [1, 2, 3, 4, 5] ) = [[1, 2, 3], [4, 5]])
	val groupNrightT3 = ( (groupNright 2 [1,2,3,4,5] ) = [[1,2], [3,4], [5]])
	val groupNrightT4 = ( (groupNright 1 [1,2,3,4,5] ) = [[1], [2], [3], [4], [5]])
	val groupNrightT5 = ( (groupNright 5 [1,2,3,4,5] ) = [[1,2,3,4,5]])
	val groupNrightT6 = ( (groupNright 3 [1,2,3,4,5] ) = [[1,2,3], [4,5]])
in 
	print ("\n------------\n groupNrightTester: \n" ^
	"test1: " ^ Bool.toString(groupNrightT1) ^ "  " ^
	"test2: " ^ Bool.toString(groupNrightT2) ^ "  " ^
	"test3: " ^ Bool.toString(groupNrightT3) ^ "  " ^
	"test4: " ^ Bool.toString(groupNrightT4) ^ "  " ^
	"test5: " ^ Bool.toString(groupNrightT5) ^ "  " ^
	"test6: " ^ Bool.toString(groupNrightT6) ^ "\n")
end;


