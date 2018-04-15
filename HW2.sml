(* Tested Using Windows 10 64-bit *)

(* 
	Problem 1: 

	a) countInList function takes a value and a list and returns the number of occurrences of that value in
	   the input list.
	   It is tail recursive in that it must reach the end of the recursive calls before counting
*)
fun countInList [] v = 0 | countInList  (x::rest) v = if(v = x) then 1+countInList rest v else countInList rest v; 

(* 
	b) zipTail function takes two lists, pairs up the corresponding elements from two lists, and returns a
	   merged list of tuples. 
	   zipTail is also tail recursive.
*)
fun zipTail [] L2 = [] 
|   zipTail L1 [] = [] 
|   zipTail (x::rest1) (y::rest2) = (x,y)::(zipTail rest1 rest2);


(* 
	c) histogram function takes a list as input and returns a list of tuples where the first elements in the
	   tuples are the unique elements from the input list and the second elements are the number of
	   occurrences of those elements in the tuple. 
*)

fun inList (n,[]) = false
| inList(n,x::rest) = if n=x then true else inList(n,rest);

fun removeDuplicates [] = []
| removeDuplicates (x::rest) = if inList(x,rest) then (removeDuplicates rest)
 else x::(removeDuplicates rest)

fun map f [] = [] | map f (x::rest) = (f x)::(map f rest);

fun histogram L = if(null L) then [] else removeDuplicates(zipTail L (map (countInList L) L));


(* 
	Problem 2: 

	a)  map function was previously defined in problem 1 - c
	Uses the fold version shown in class that is, ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
	Function deepSum is given a list of int lists and it returns the sum of all numbers in all sublists
	of the input list.
	add function will perform basic addition of a tuple. 
	addup function will use fold and add with 0 as a base
*)
fun fold f base [] = base | fold f base (x::rest) = f(x,(fold f base rest));

fun add (x,y) = x+y;

fun addUp L = fold add 0 L;

fun deepSum L = if(null L) then 0 else addUp (map addUp L);

(* 
	b) Function deepSumOption is given a list of int option lists and it returns the sum of all
	   int option values in all sublists of the input list. 
	   Non-Recursive
*)

datatype option = NONE | SOME of int;

(* This function will replace all NONE in list with SOME(0) for easy evaluation *)
fun replace (NONE) = SOME(0) | replace (SOME(x)) = SOME(x);


(* This function will simply add two SOME() types.
   
   Another option is to use this version of the function add2,
   fun add2 ((NONE),(SOME(x)))= SOME(0+x) | add2 ((SOME(y)),(NONE))= SOME(0+y)| add2 (SOME(a),SOME(b)) = SOME(a+b); 
   This will avoid having to use replace() *)
   
fun add2 (SOME(x),SOME(y)) = SOME(x+y); 

(*Uses fold and add2 functions with SOME(0) as base *)
fun addUp2 L = fold add2 (SOME(0)) (map replace L);

(*Uses map and addUp2 functions *)
fun deepSumOptionHelper L = addUp2(map addUp2 L);

(*Returns NONE if empty or 0 *)
fun deepSumOption L = if( (deepSumOptionHelper L) = SOME(0) ) then (NONE) else (deepSumOptionHelper L);

(* 
	Problem 3: 
	unzip  takes a list of tuples as input and produces a list including two lists as output.
	Non-recursive and the inverse of zip function 
	unzipHelper returns first element in tuple
	unzipHelper2 return second element in tuple
	unzip appends both lists together onto an empty list
*)
fun unzipHelper (x,y) = x;

fun unzipHelper2 (x,y) = y;

fun unzip L = if(null L) then [] else (map unzipHelper L)::(map unzipHelper2 L)::[];

(*
	Problem 4:

	a)
*)

datatype either = ImAString of string | ImAnInt of int;

(*
	b)
*)

datatype eitherTree = eLEAF of either | eINTERIOR of (either*eitherTree*eitherTree);

(* 
	c) 
*)
(* eitherSearchHelper will take in three tuple of boolean values and return true if any of them are true *)

fun eitherSearchHelper (x,y,z) = if(x = true orelse y = true orelse z = true) then true else false;

(* For eitherSearch function, it will traverse through entire tree searching for value v. If it finds a string, it returns false and moves to next node in tree*)
fun eitherSearch v (eLEAF(ImAnInt(x))) = if (v=x) then true else false
	| eitherSearch v (eLEAF(ImAString(x))) = false
	| eitherSearch v (eINTERIOR(ImAString(t1),t2,t3)) = (eitherSearchHelper(false,eitherSearch v (t2),eitherSearch v (t3)))
	| eitherSearch v (eINTERIOR(t1,t2,t3)) = (eitherSearchHelper(eitherSearch v (eLEAF(t1)),eitherSearch v (t2),eitherSearch v (t3)));
(* 
	d)	TEST Case for eitherSearch()
	This tree will contain four levels and each bottom interior node (nodes at level 3) will have two leaf nodes (leaves at level 4)
	This tree will not contain the int value 100 and not contain the string "c"
	Originally made the test to search for both a int value and a string value. However, I commented out the
	string search part. 
	Test will search for one value that is in list and one value that isn't in list. Both test results should return true. 

	Sorry, I know its a bit much...
*)

fun eitherTest () =
let 
	val (L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16) = (eLEAF(ImAnInt(3)),eLEAF(ImAnInt(1)),eLEAF(ImAnInt(2)),eLEAF(ImAnInt(4)),eLEAF(ImAnInt(5)),eLEAF(ImAnInt(6))
		,eLEAF(ImAnInt(7)),eLEAF(ImAnInt(8)),eLEAF(ImAString("a")),eLEAF(ImAString("b")),eLEAF(ImAString("d")),eLEAF(ImAString("e")),eLEAF(ImAString("f")),eLEAF(ImAString("g"))
		,eLEAF(ImAString("h")),eLEAF(ImAString("i")))
	val t14 = eINTERIOR(ImAString("s"),L15,L16)
	val t13 = eINTERIOR(ImAString("r"),L13,L14)
	val t12 = eINTERIOR(ImAString("p"),L11,L12)
	val t11 = eINTERIOR(ImAString("w"),L9,L10)
	val t10 = eINTERIOR(ImAnInt(30),L7,L8)
	val t9 = eINTERIOR(ImAnInt(29),L5,L6)
	val t8 = eINTERIOR(ImAnInt(28),L3,L4)
	val t7 = eINTERIOR(ImAnInt(27),L1,L2)
	val t6 = eINTERIOR(ImAString("t"),t13,t14)
	val t5 = eINTERIOR(ImAString("z"),t11,t12)
	val t4 = eINTERIOR(ImAnInt(24),t9,t10)
	val t3 = eINTERIOR(ImAnInt(23),t7,t8)
	val t2 = eINTERIOR(ImAString("o"),t5,t6)
	val t1 = eINTERIOR(ImAnInt(21),t3,t4)
	val root = eINTERIOR(ImAnInt(20),t1,t2) 
	val result = ( (eitherSearch 30 (root)) = true ) 
	val result2 = ( (eitherSearch 100 (root)) = false )
	(* val result3 = ( (eitherSearch "a" root) = true ) *)
	(* val result4 = ( (eitherSearch "c" root) = false ) *)

in
	print ("\n------------\n eitherTest: \n" ^
	"All tests results should return true \n" ^
	"testing for a int value in list: " ^ Bool.toString(result) ^ " \n" ^
	"testing for a int value NOT in list: " ^ Bool.toString(result2) ^ "\n")
end;


(*
	Problem 5:

	a) findMin and findMax uses the same concept as in problem 4. The only difference is that it does a different comparison and different variable names
	findMin and findMax are also identical except for their helper functions. 
	
	findMin will find smallest int value in tree. 
	findMax will find largest int value in tree.
*)

datatype 'a Tree = LEAF of 'a | NODE of ('a Tree) * ('a Tree);

fun findMinHelper (x,y) = if(x < y) then x else y;

fun findMin (LEAF(x)) = x
	| findMin (NODE(t1,t2)) = (findMinHelper(findMin (t1),findMin (t2)));

fun findMaxHelper (x,y) = if(x < y) then y else x;
	
fun findMax (LEAF(x)) = x
	| findMax (NODE(t1,t2)) = (findMaxHelper(findMax (t1),findMax (t2)));

(*
	b) 
	   Base case: it returns the leaf if argument is a leaf 
	   This function is similar to findMin and findMax and it makes use of both functions
*)

datatype 'a myTree = myLEAF of 'a | myNODE of 'a*'a*('a myTree)*('a myTree);

fun minmaxTree (LEAF(x)) = myLEAF(x)
| minmaxTree (NODE(t1,t2)) = (myNODE(findMin (NODE(t1,t2)), findMax (NODE(t1,t2)), minmaxTree (t1), minmaxTree (t2)));

(*
	c) Test functions for this are optional and not required. 
	
*)

val L1 = LEAF(1);
val L2 = LEAF(2);
val L3 = LEAF(3);
val N1 = NODE(L1,L2);
val N2 = NODE(N1,L1);
val N3 = NODE(N1,N2);
val t1 = NODE(N2,N3);
minmaxTree t1;


val L1 = LEAF(3);
val L2 = LEAF(5);
val L3 = LEAF(11);
val N1 = NODE(L1,L2);
val N2 = NODE(N1,L1);
val N3 = NODE(N1,N2);
val t1 = NODE(N2,N3);
minmaxTree t1;


val L1 = LEAF(121);
val L2 = LEAF(23);
val L3 = LEAF(345);
val N1 = NODE(L1,L2);
val N2 = NODE(N1,L1);
val N3 = NODE(N1,N2);
val t1 = NODE(N2,N3);
minmaxTree t1;


(*
	
	Here's a test with the same tree from problem 4 without the strings.
	This test will use the findMin and findMax functions to check for the correct elements in each of the nodes. 
	
	This test function is not entirely complete. 
	
	Test functions for findMin and findMax functions is not this. It will be located at the bottom of the document. 
	
	
fun minMaxTreeTest () =
let 
	val (L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16) = (LEAF((3)),LEAF((1)),LEAF((2)),LEAF((4)),LEAF((5)),LEAF((6))
		,LEAF((7)),LEAF((8)),LEAF((56)),LEAF((57)),LEAF((58)),LEAF((59)),LEAF((60)),LEAF((61))
		,LEAF((62)),LEAF((63)))
	val t14 = NODE((50),L15,L16)
	val t13 = NODE((51),L13,L14)
	val t12 = NODE((52),L11,L12)
	val t11 = NODE((53),L9,L10)
	val t10 = NODE((100),L7,L8)
	val t9 = NODE((29),L5,L6)
	val t8 = NODE((28),L3,L4)
	val t7 = NODE((27),L1,L2)
	val t6 = NODE((54),t13,t14)
	val t5 = NODE((55),t11,t12)
	val t4 = NODE((24),t9,t10)
	val t3 = NODE((23),t7,t8)
	val t2 = NODE(("o"),t5,t6)
	val t1 = NODE((21),t3,t4)
	val root = NODE((20),t1,t2) 
	val result = ( (findMin (root)) = 1 ) 
	val result2 = ( (findMax (root)) = 100 )
	val result3 = ( (findMin (root)) = 1 ) 
	val result4 = ( (findMax (root)) = 100 )
	val result5 = ( (findMin (root)) = 1 ) 
	val result6 = ( (findMax (root)) = 100 )
in
	print ("\n------------\n eitherTest: \n" ^
	"All tests results should return true \n" ^
	"testing for first element of root: " ^ Bool.toString(result) ^ " \n" ^
	"testing for second element of root: " ^ Bool.toString(result2) ^ " \n" ^
	"testing for first element of t1: " ^ Bool.toString(result3) ^ " \n" ^
	"testing for second element of t1: " ^ Bool.toString(result4) ^ " \n" ^
	"testing for first element of t2: " ^ Bool.toString(result5) ^ " \n" ^
	"testing for second element of t2: " ^ Bool.toString(result6) ^ "\n")
end;


*)


(*------------Test Functions Below---------------*)

 
fun countInListTest () = 
let
	val countInListT1 = (countInList ["3","5","5","-","4","5","1"] "5" = 3)
	val countInListT2 = (countInList [] "5" = 0)
	val countInListT3 = (countInList [true, false, false, false, true, true, true] true = 4)
	val countInListT4 = (countInList [[],[1,2],[3,2],[5,6,7],[8],[]] [] = 2)
	val countInListT5 = (countInList [1,2,3,4,5,1] 1 = 2)
	val countInListT6 = (countInList [1,2,3,4,5,1] 2 = 1)
	val countInListT7 = (countInList [1,2,3,4,5,1] 3 = 1)
	val countInListT8 = (countInList [1,1,1,1,1,5] 5 = 1)
in 
	print ("\n------------\n countInListTester: \n" ^
	"test1: " ^ Bool.toString(countInListT1) ^ "  " ^
	"test2: " ^ Bool.toString(countInListT2) ^ "  " ^
	"test3: " ^ Bool.toString(countInListT3) ^ "  " ^
	"test4: " ^ Bool.toString(countInListT4) ^ "  " ^
	"test5: " ^ Bool.toString(countInListT5) ^ "  " ^
	"test6: " ^ Bool.toString(countInListT6) ^ "  " ^
	"test7: " ^ Bool.toString(countInListT7) ^ "  " ^
	"test8: " ^ Bool.toString(countInListT8) ^ "\n")
end;



fun zipTailTest () = 
let
	val zipTailT1 = (zipTail [1,2,3,4,5] ["one","two"] = [(1,"one"),(2,"two")])
	val zipTailT2 = (zipTail [1] [1,2,3,4] = [(1,1)])
	val zipTailT3 = (zipTail [1,2,3,4,5] [] = [])
	val zipTailT4 = (zipTail [] [1,2,3,4,5] = [])
	val zipTailT5 = (zipTail [1,3] [2,4] = [(1,2), (3,4)])
	val zipTailT6 = (zipTail ["one", "two", "three"] [1,2,3,4,5] = [("one", 1), ("two", 2), ("three",3)])
	val zipTailT7 = (zipTail [] [] = [])
	val zipTailT8 = (zipTail [1] [1] = [(1,1)])
in 
	print ("\n------------\n zipTailTester: \n" ^
	"test1: " ^ Bool.toString(zipTailT1) ^ "  " ^
	"test2: " ^ Bool.toString(zipTailT2) ^ "  " ^
	"test3: " ^ Bool.toString(zipTailT3) ^ "  " ^
	"test4: " ^ Bool.toString(zipTailT4) ^ "  " ^
	"test5: " ^ Bool.toString(zipTailT5) ^ "  " ^
	"test6: " ^ Bool.toString(zipTailT6) ^ "  " ^
	"test7: " ^ Bool.toString(zipTailT7) ^ "  " ^
	"test8: " ^ Bool.toString(zipTailT8) ^ "\n")
end;



fun histogramTest () = 
let
	val histogramT1 = (histogram [1,3,2,2,3,0,3] = [(1,1),(2,2),(0,1),(3,3)])
	val histogramT2 = (histogram [[1,2],[3],[],[3],[1,2]] = [([],1),([3],2),([1,2],2)])
	val histogramT3 = (histogram [] = [])
	val histogramT4 = (histogram [true, false, false, false, true, true, true] = [(false,3),(true,4)])
	val histogramT5 = (histogram [1,2,3,4,5,1,2,3,4,5] = [(1,2),(2,2),(3,2),(4,2),(5,2)])
	val histogramT6 = (histogram [[],[],[],[],[]] = [([],5)])
	val histogramT7 = (histogram ["a","b","a"] = [("b",1),("a",2)])
	val histogramT8 = (histogram [1] = [(1,1)])
in 
	print ("\n------------\n histogramTester: \n" ^
	"test1: " ^ Bool.toString(histogramT1) ^ "  " ^
	"test2: " ^ Bool.toString(histogramT2) ^ "  " ^
	"test3: " ^ Bool.toString(histogramT3) ^ "  " ^
	"test4: " ^ Bool.toString(histogramT4) ^ "  " ^
	"test5: " ^ Bool.toString(histogramT5) ^ "  " ^
	"test6: " ^ Bool.toString(histogramT6) ^ "  " ^
	"test7: " ^ Bool.toString(histogramT7) ^ "  " ^
	"test8: " ^ Bool.toString(histogramT8) ^ "\n")
end;




fun deepSumTest () = 
let
	val deepSumT1 = (deepSum [[1,2,3],[4,5],[6,7,8,9],[]] = 45)
	val deepSumT2 = (deepSum [[10,10],[10,10,10],[10]] = 60)
	val deepSumT3 = (deepSum [[]] = 0)
	val deepSumT4 = (deepSum [] = 0)
	val deepSumT5 = (deepSum [[0,0,0],[0,0,0,0]] = 0)
	val deepSumT6 = (deepSum [[1,2,3],[4,5,6,7]] = 28)
	val deepSumT7 = (deepSum [[1],[],[],[]] = 1)
	val deepSumT8 = (deepSum [[],[],[0]] = 0)
in 
	print ("\n------------\n deepSumTester: \n" ^
	"test1: " ^ Bool.toString(deepSumT1) ^ "  " ^
	"test2: " ^ Bool.toString(deepSumT2) ^ "  " ^
	"test3: " ^ Bool.toString(deepSumT3) ^ "  " ^
	"test4: " ^ Bool.toString(deepSumT4) ^ "  " ^
	"test5: " ^ Bool.toString(deepSumT5) ^ "  " ^
	"test6: " ^ Bool.toString(deepSumT6) ^ "  " ^
	"test7: " ^ Bool.toString(deepSumT7) ^ "  " ^
	"test8: " ^ Bool.toString(deepSumT8) ^ "\n")
end;




fun deepSumOptionTest () = 
let
	val deepSumOptionT1 = (deepSumOption [[SOME(1),SOME(2),SOME(3)],[SOME(4),SOME(5)],[SOME(6),NONE],[],[NONE]] = SOME 21)
	val deepSumOptionT2 = (deepSumOption [[SOME(10),NONE],[SOME(10), SOME(10), SOME(10),NONE,NONE]] = SOME 40)
	val deepSumOptionT3 = (deepSumOption [[NONE]] = NONE)
	val deepSumOptionT4 = (deepSumOption [] = NONE)
	val deepSumOptionT5 = (deepSumOption [[NONE,NONE],[NONE],[NONE]] = NONE)
	val deepSumOptionT6 = (deepSumOption [[NONE,NONE],[NONE],[NONE],[SOME(1)]] = SOME 1)
	val deepSumOptionT7 = (deepSumOption [[SOME(1)],[SOME(1)],[SOME(1)]] = SOME 3)
	val deepSumOptionT8 = (deepSumOption [[NONE,NONE,NONE,NONE,NONE,SOME(5)]] = SOME 5)
in 
	print ("\n------------\n deepSumOptionTester: \n" ^
	"test1: " ^ Bool.toString(deepSumOptionT1) ^ "  " ^
	"test2: " ^ Bool.toString(deepSumOptionT2) ^ "  " ^
	"test3: " ^ Bool.toString(deepSumOptionT3) ^ "  " ^
	"test4: " ^ Bool.toString(deepSumOptionT4) ^ "  " ^
	"test5: " ^ Bool.toString(deepSumOptionT5) ^ "  " ^
	"test6: " ^ Bool.toString(deepSumOptionT6) ^ "  " ^
	"test7: " ^ Bool.toString(deepSumOptionT7) ^ "  " ^
	"test8: " ^ Bool.toString(deepSumOptionT8) ^ "\n")
end;





fun unzipTest () = 
let
	val unzipT1 = (unzip [(1,2),(3,4),(5,6)] = [[1,3,5],[2,4,6]])
	val unzipT2 = (unzip [("1","a"),("5","b"),("8","c")] = [["1","5","8"],["a","b","c"]])
	val unzipT3 = (unzip [] = [])
	val unzipT4 = (unzip [(1,2),(4,5),(7,8)] = [[1,4,7],[2,5,8]])
	val unzipT5 = (unzip [("a","b"),("e","f"),("i","j")] = [["a","e","i"],["b","f","j"]])
	val unzipT6 = (unzip [(0,0), (0,0)] = [[0,0],[0,0]])
	val unzipT7 = (unzip [(1,3),(4,2)] = [[1,4],[3,2]])
	val unzipT8 = (unzip [(0,0),(1,2)] = [[0,1],[0,2]])
in 
	print ("\n------------\n unzipTester: \n" ^
	"test1: " ^ Bool.toString(unzipT1) ^ "  " ^
	"test2: " ^ Bool.toString(unzipT2) ^ "  " ^
	"test3: " ^ Bool.toString(unzipT3) ^ "  " ^
	"test4: " ^ Bool.toString(unzipT4) ^ "  " ^
	"test5: " ^ Bool.toString(unzipT5) ^ "  " ^
	"test6: " ^ Bool.toString(unzipT6) ^ "  " ^
	"test7: " ^ Bool.toString(unzipT7) ^ "  " ^
	"test8: " ^ Bool.toString(unzipT8) ^ "\n")
end;





fun findMinTest () = 
let
	val findMinT1 = (findMin (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(8))),LEAF(4))) = 4)
	val findMinT2 = (findMin (NODE(NODE(NODE(LEAF(0),LEAF(11)),LEAF(6)),NODE(LEAF(3),LEAF(10)))) = 0)
	val findMinT3 = (findMin (LEAF(5)) = 5)
	val findMinT4 = (findMin (NODE(NODE(LEAF(10),NODE(LEAF(8),LEAF(2))),LEAF(9))) = 2)
	val findMinT5 = (findMin (NODE(NODE(LEAF(14),NODE(LEAF(21),LEAF(32))),LEAF(8))) = 8)
	val findMinT6 = (findMin (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(8))),LEAF(1))) = 1)
	val findMinT7 = (findMin (LEAF(8)) = 8)
	val findMinT8 = (findMin (LEAF(1)) = 1)
in 
	print ("\n------------\n findMinTester: \n" ^
	"test1: " ^ Bool.toString(findMinT1) ^ "  " ^
	"test2: " ^ Bool.toString(findMinT2) ^ "  " ^
	"test3: " ^ Bool.toString(findMinT3) ^ "  " ^
	"test4: " ^ Bool.toString(findMinT4) ^ "  " ^
	"test5: " ^ Bool.toString(findMinT5) ^ "  " ^
	"test6: " ^ Bool.toString(findMinT6) ^ "  " ^
	"test7: " ^ Bool.toString(findMinT7) ^ "  " ^
	"test8: " ^ Bool.toString(findMinT8) ^ "\n")
end;




fun findMaxTest () = 
let
	val findMaxT1 = (findMax (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(8))),LEAF(4))) = 8)
	val findMaxT2 = (findMax (NODE(NODE(NODE(LEAF(0),LEAF(11)),LEAF(6)),NODE(LEAF(3),LEAF(10)))) = 11)
	val findMaxT3 = (findMax (LEAF(5)) = 5)
	val findMaxT4 = (findMax (NODE(NODE(LEAF(10),NODE(LEAF(8),LEAF(2))),LEAF(9))) = 10)
	val findMaxT5 = (findMax (NODE(NODE(LEAF(14),NODE(LEAF(21),LEAF(32))),LEAF(8))) = 32)
	val findMaxT6 = (findMax (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(12))),LEAF(1))) = 12)
	val findMaxT7 = (findMax (LEAF(8)) = 8)
	val findMaxT8 = (findMax (LEAF(1)) = 1)
in 
	print ("\n------------\n findMaxTester: \n" ^
	"test1: " ^ Bool.toString(findMaxT1) ^ "  " ^
	"test2: " ^ Bool.toString(findMaxT2) ^ "  " ^
	"test3: " ^ Bool.toString(findMaxT3) ^ "  " ^
	"test4: " ^ Bool.toString(findMaxT4) ^ "  " ^
	"test5: " ^ Bool.toString(findMaxT5) ^ "  " ^
	"test6: " ^ Bool.toString(findMaxT6) ^ "  " ^
	"test7: " ^ Bool.toString(findMaxT7) ^ "  " ^
	"test8: " ^ Bool.toString(findMaxT8) ^ "\n")
end;



