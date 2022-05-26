fun parse file =
    let
  	    (* Open input file. *)
      	val inStream = TextIO.openIn file

        fun readLines acc =
          let
            val newLine = TextIO.inputLine inStream
          in
            if newLine = NONE
            then (rev acc)
            else ( readLines ( explode (valOf newLine ) :: acc ))
        end;

        val map = readLines []
        val M = length (hd map) - 1
        val N = length map
    in
   	    (N,M,map)
end

fun find_coordinates mapA i j N M c = if (Array2.sub(mapA, i, j) = c) then (i,j)
                                         else 
                                             if(j=M) then find_coordinates mapA (i+1) 0 N M c
                                             else find_coordinates mapA i (j+1) N M c

fun getAirports mapA i j N M airports = if i=N andalso j = (M-1) then airports
                                        else if (Array2.sub(mapA, i, j) = #"A")
                                        then
                                            if (j=M) then getAirports mapA (i+1) 0 N M ((i,j)::airports)
                                            else getAirports mapA i (j+1) N M ((i,j)::airports)
                                        else 
                                            if (j=M) then getAirports mapA (i+1) 0 N M airports
                                        else getAirports mapA i (j+1) N M airports

fun queue_airports mapTimes   []    q k (x,y) = ()
  | queue_airports mapTimes ((i,j)::xs) q k (x,y) = let 
                                                       val _ = if (i,j)<>(x,y) then 
                                                          let
                                                             val _ = Array2.update(mapTimes, i, j, k)
                                                          in
                                                             Queue.enqueue(q, ((i, j), k))
                                                          end
                                                               else ()
                                                       in
                                                         queue_airports mapTimes xs q k (x,y)
                                                    end

fun check_bountries (i,j) N M q k mapTimes mapA = if Int.<(i,0) orelse Int.<(j,0) orelse Int.>(i,N-1) 
                                                                orelse Int.>(j,M-1) orelse Array2.sub(mapTimes, i, j) <> (~1)
                                                                orelse Array2.sub(mapA, i, j) = #"X" 
                                                  then ()
                                                  else
                                                    let
                                                       val _ = Array2.update(mapTimes, i, j, k)
                                                    in
                                                      Queue.enqueue(q, ((i, j), k))
                                                    end

fun fill_queue (i,j) N M q k mapTimes mapA = let
                                                val _ = check_bountries (i-1,j) N M q (k+2) mapTimes mapA
                                                val _ = check_bountries (i+1,j) N M q (k+2) mapTimes mapA
                                                val _ = check_bountries (i,j-1) N M q (k+2) mapTimes mapA
                                                val _ = check_bountries (i,j+1) N M q (k+2) mapTimes mapA
                                             in 
                                                ()
                                             end

fun floodFill mapA mapTimes q N M airChecked =
                                     if Queue.isEmpty(q) then ()
                                     else 
                                         let 
                                            val checked2 = airChecked
                                            val current = Queue.head(q)
                                            val _ = Queue.dequeue(q)
                                            val coord = #1 current
                                            val time = #2 current
                                            val checked = if Array2.sub(mapA, #1 coord, #2 coord) = #"A" andalso not checked2 
                                                          then 
                                                             let
                                                                val _ = fill_queue coord N M q time mapTimes mapA
                                                                val airports = getAirports mapA 0 0 (N-1) (M-1) []
                                                                val _ = queue_airports mapTimes airports q (time+5) coord
                                                             in
                                                               true
                                                             end
                                                          else
                                                              let

                                                                 val _ = fill_queue coord N M q time mapTimes mapA
                                                              in
                                                                 checked2
                                                              end
                                         in
                                            floodFill mapA mapTimes q N M checked
                                         end

fun legal x = x = #"R" orelse x = #"U" orelse x = #"L" orelse x = #"D" orelse x = #"S"

fun check_bountries2 (i,j) N M q (par1,par2) k mapTimes mapA = 
                      if i<0 orelse j<0 orelse i>(N-1) orelse j>(M-1) 
                             orelse Array2.sub(mapTimes, i, j) <= k
                             orelse legal (Array2.sub(mapA, i, j))
                             orelse Array2.sub(mapTimes, i, j) = (~1)
                      then ()
                      else
                           let
                              val _ = if Array2.sub(mapA, i, j) = #"T" then ()
                                      else if (i+1)<=(N-1) andalso legal (Array2.sub(mapA, i+1, j)) then Array2.update(mapA, i, j, #"U")                               
                            else if (j+1)<=(M-1) andalso legal (Array2.sub(mapA, i, j+1)) then Array2.update(mapA, i, j, #"L") 
                            else if (j-1)>=0 andalso legal (Array2.sub(mapA, i, j-1)) then Array2.update(mapA, i, j, #"R") 
                            else Array2.update(mapA, i, j, #"D")
                           in 
                              Queue.enqueue(q, ((i, j), k))
                           end
 
fun fill_queue2 (i,j) N M q k mapTimes mapA = let
                                                   val _ = check_bountries2 (i-1,j) N M q (i,j) (k+1) mapTimes mapA
                                                   val _ = check_bountries2 (i+1,j) N M q (i,j) (k+1) mapTimes mapA
                                                   val _ = check_bountries2 (i,j-1) N M q (i,j) (k+1) mapTimes mapA
                                                   val _ = check_bountries2 (i,j+1) N M q (i,j) (k+1) mapTimes mapA
                                                 in 
                                                   ()
                                                 end


fun get_path (i,j) mapA str =  if Array2.sub(mapA, i, j) = #"S" then str
                              else 
                                  let
                                     val recog = Array2.sub(mapA, i, j)
                                  in
                                     if recog = #"D" then get_path (i-1,j) mapA (Char.toString(recog)^str) 
                                     else if recog = #"L" then get_path (i, j+1) mapA (Char.toString(recog)^str) 
                                     else if recog = #"R" then get_path (i, j-1) mapA (Char.toString(recog)^str) 
                                     else get_path (i+1,j) mapA (Char.toString(recog)^str) 
                                  end 

fun take [] (t1,t2) = true
  | take (((i,j),_)::xs) (t1,t2) = if (i,j) = (t1,t2) then false
                   else take xs (t1,t2)

fun notin q (t1,t2) = let
                         val li = Queue.contents(q)
                      in
                         take li (t1,t2)
                      end

fun find_path (t1,t2) mapA N M q = if (t1+1)<=(N-1) andalso legal (Array2.sub(mapA, t1+1, t2)) andalso notin q (t1+1,t2) then get_path (t1+1,t2) mapA "U"  
                             else if (t2+1)<=(M-1) andalso legal (Array2.sub(mapA, t1, t2+1)) andalso notin q (t1,t2+1) then get_path (t1, t2+1) mapA "L"   
                             else if (t2-1)>=0 andalso legal (Array2.sub(mapA, t1, t2-1)) andalso notin q (t1,t2-1) then get_path (t1, t2-1) mapA "R" 
                             else get_path (t1-1, t2) mapA "D" 

fun floodFill2 mapA mapTimes q N M = if Queue.isEmpty(q) then "IMPOSSIBLE"
                                       else
                                           let
                                              val current = Queue.head(q)
                                              val coord = #1 current 
                                           in
                                              if Array2.sub(mapA, #1 coord, #2 coord) = #"T" then find_path coord mapA N M q
                                              else
                                                  let
                                                     val _ = Queue.dequeue(q)
                                                     val time = #2 current
                                                     val _ = fill_queue2 coord N M q time mapTimes mapA
                                                  in
                                                     floodFill2 mapA mapTimes q N M                                        
                                                  end
                                           end

fun solve (N,M,map) =
    let 
       val mapA = Array2.fromList(map)
       val queue = Queue.mkQueue() : ((int*int)*int) Queue.queue
       val w_coordinates = find_coordinates mapA 0 0 (N-1) (M-1) #"W"
       val q = Queue.mkQueue(): ((int*int)*int) Queue.queue
       val mapTimes = Array2.array(N, M, ~1)
       val _ = Queue.enqueue(q, (w_coordinates,0))
       val _ = Array2.update(mapTimes, #1 w_coordinates, #2 w_coordinates, 0)
       val _ = floodFill mapA mapTimes q N M false
       val s_coordinates = find_coordinates mapA 0 0 (N-1) (M-1) #"S"
       val _ = Queue.enqueue(q, (s_coordinates, 0))
       val t = find_coordinates mapA 0 0 (N-1) (M-1) #"T"
       val path = floodFill2 mapA mapTimes q N M 
    in
       if path = "IMPOSSIBLE" then print(path^"\n")
       else print(Int.toString(size(path))^"\n"^path^"\n")
    end
fun stayhome fileName = solve (parse fileName)