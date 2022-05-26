fun parse file =
    let
    (* A function to read an integer from specified input. *)
        fun readInt input = 
        Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

    (* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
    val k = readInt inStream
        (* A function to read N integers from the open file. *)
        val _=TextIO.inputLine inStream
        fun dwstaOla 0 li = rev li
          | dwstaOla T li = let
                               val v = readInt inStream
                               val e = readInt inStream
                               val graph = Array.array(v, [] : int list)
                               fun readEdges 2 = let
                                   val v1=readInt inStream
                                   val v2=readInt inStream
                                   val _=TextIO.inputLine inStream
                                   val _=Array.update(graph,v1-1, v2::Array.sub(graph,v1-1))
                                   val _=Array.update(graph,v2-1, v1::Array.sub(graph,v2-1))
                               in
                                   graph
                               end
                               fun readEdges2 graph 0 = graph
                                 | readEdges2 graph e = readEdges2 (readEdges 2) (e-1)
                            in
                               dwstaOla (T-1) ((readEdges2 graph e) ::li)
                            end 
    in
   	(k,dwstaOla k [])
    end

fun  conn visited y [] graph = visited
  |  conn visited y (x::xs) graph =let
                                      val _=Array.update(visited, y, 1)
                                   in
                                      if (Array.sub(visited, x-1)=0) then
                                      conn visited (x-1) (xs@(Array.sub(graph,x-1))) graph
                                      else
                                      conn visited y xs graph
                                   end 

fun detectHelp 0 = false
  | detectHelp 1 = true

fun detectConn visited ~1  = true
  | detectConn visited lisLength = (detectHelp (Array.sub(visited, lisLength))) andalso (detectConn visited (lisLength-1) )

fun updateParents [] y par visited li = li
  | updateParents (x::xs) y par visited li = let 
                                                val _ = if (Array.sub(par, y-1))<>x then (Array.update(par, x-1, y))
                                                        else Array.update(par, x-1, Array.sub(par,x-1))
                                             in 
                                                if (Array.sub(visited, x-1))=0 then updateParents xs y par visited (x::li)
                                                else updateParents xs y par visited li
                                     end  

fun detectCycle visited graph par [] k = k
  | detectCycle visited graph par (x::xs) k =if (k>=0) then k
                                              else
                                                  let
                                                     val _ = Array.update(visited, x-1, Array.sub(visited, x-1)+1)
                                                     val stack = updateParents (Array.sub(graph, x-1)) x par visited []
                                                  in
                                                     if (Array.sub(visited, x-1))=2 
                                                     then detectCycle visited graph par xs x
                                                    else detectCycle visited graph par (stack@xs) ~1
                                                  end


fun findVerticesOfCycle cur visited par k li = if (cur = k) 
                                            then li
                                            else 
                                                let
                                                   val cur2=Array.sub(par, cur-1)
                                                   val _=Array.update(visited, cur2-1, ~1)
                                                in 
                                                   findVerticesOfCycle cur2 visited par k ((cur2)::li)
                                                end

fun treesHelp visited par [] li = li
  | treesHelp visited par (x::xs) li = if Array.sub(par, x-1) = ~1 orelse Array.sub(visited, x-1) = 1
                        then treesHelp visited par xs li
                        else treesHelp visited par xs (x::li)

fun dfsForTrees visited par graph [] k = k    
  | dfsForTrees visited par graph (x::xs) k = if (k=(~1)) then k
                                              else
                                                let
                                                   val _ = Array.update(visited, x-1, Array.sub(visited, x-1)+1) 
                                                   val vert = treesHelp visited par (Array.sub(graph, x-1)) []
                                                in
                                                   if Array.sub(visited, x-1)=2 
                                                   then dfsForTrees visited par graph xs ~1
                                                   else dfsForTrees visited par graph (vert@xs) (k+1)
                                                end

fun treesSize [] par visited y graph li = li
  | treesSize (x::xs) par visited y graph li = let
                                    val visited = Array.array(y, 0)
                                    val size = dfsForTrees visited par graph [x] 0
                                 in 
                                    if size = (~1) then []
                                    else treesSize xs par visited y graph (size::li)
                                 end

fun takeString [] str = ""
  | takeString [x] str = let 
                            val y = Int.toString(x)
                         in 
                            str^y^"\n"
                         end
  | takeString (x::xs) str = let
                                val y = Int.toString(x)
                             in 
                                takeString xs (str^y^" ")
                             end

fun takeStr li = let 
                        val x = Int.toString( List.length(li))
                     in
                        "CORONA "^x^"\n"
                     end

fun coronaFinal somarray =
    let
       val y = somarray
       val visited = Array.array(Array.length(y), 0)
       val _ = conn visited 0 (Array.sub(y,0)) y
       val a = detectConn visited (Array.length(visited)-1) (* a bool that shows connectivity of the graph *)
       val met = if  a = false then "NO CORONA\n"
                 else
                     let
                        val visited = Array.array(Array.length(y), 0)
                        val par = Array.array(Array.length(y),~1)
                        val c = detectCycle visited y par [1] ~1
                        val b = if c=(~1) then false else true 
                        val met = if b = false then "NO CORONA\n"
                                  else
                                      let
                                         val cur = Array.sub(par, c-1)
                                         val _ = Array.update(visited, cur-1, ~1)
                                         val _ = Array.update(visited, c-1, ~1)
                                         val vertices = findVerticesOfCycle cur visited par c [cur]
                                         val par = visited
                                         val visited = Array.array(Array.length(y), 0)
                                         val met = treesSize vertices par visited (Array.length(y)) y []
                                         val met = ListMergeSort.sort(fn (s,t) => s>t)  met
                                         val str = if met = [] then "NO CORONA\n"
                                                   else (takeStr met)^(takeString met "")
                                      in
                                         str
                                      end
                      in
                          met
                      end
    in 
       print(met)
    end

fun final x =
    let 
       val _=map coronaFinal x
    in
      ()
    end

fun solve (k, sizelist) = 
    let  
       val x = sizelist
    in 
       final x
    end

fun coronograph fileName = solve (parse fileName)