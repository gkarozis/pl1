fun parse file =
    let
    (* A function to read an integer from specified input. *)
        fun readInt input = 
        Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

    (* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
    val t = readInt inStream
        (* A function to read N integers from the open file. *)
    fun readInts 0 acc = rev acc (* Replace with 'rev acc' for proper order. *)
      | readInts i acc = readInts (i - 1) (readInt inStream :: acc)

        fun readAll 0 acc = rev acc
          | readAll t acc = let
                                val _=TextIO.inputLine inStream
                            in
                                readAll (t-1) (readInts 2 [] :: acc)
                            end
    in
   	(t,  readAll t [])
    end

fun logTwo a = Math.log10(a)/Math.log10(2.0)
fun takemin a = trunc (logTwo a + 0.00000000001)
fun powof2 0 = 1
  | powof2 a = 2*(powof2 (a-1))
fun afairesh a = a-powof2 (takemin (Real.fromInt a))
fun takeList 0 li = li
  | takeList a li = takeList (afairesh a) ( powof2 (takemin (Real.fromInt a)) :: li)
fun findpow 0 lis = lis
  | findpow b []  = []
  | findpow b (1::xs) = (1::findpow b xs)
  | findpow b (x::xs) = let 
                            val temp = x div 2
                        in 
                            findpow (b-1) (temp::temp::xs)
                        end
fun sol [a,b] =
    let
      val c = takeList a []
      val d = List.length(c)
    in
      if d > b orelse a<b then []
      else findpow (b-d) c
    end
  | sol li = []

fun ko []=[1]
  | ko (x::xs) = ko xs

(* a function to have a better output *)
fun final []      k lis p = lis@[p]
  | final (x::xs) k lis p = let
                              val y = powof2 k
                            in
                              if y = x then final xs k lis (p+1)
                              else final (x::xs) (k+1) (lis@[p]) 0
                            end
fun forall lis = if sol lis = [] then []
                 else final (sol lis) 0 [] 0

fun printList xs = print(concat["[",String.concatWith "," (map Int.toString xs),"]","\n"]);

fun solution li =
   let
      val _=map printList li
   in
     ()
   end
 
(* Dummy solver & requested interface. *)
fun solve (n, sizelist) = 
    let  
       val x =  hd (hd sizelist)
       val y = Real.fromInt x
       val sol_list = map forall sizelist
    in 
       solution sol_list
    end
fun powers2 fileName = solve (parse fileName ) 