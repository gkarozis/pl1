import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.LinkedList;
import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.*;

public class Stayhome {

  public static class CoronaT{

         public CoronaT(int x, int y, int t){
          this.x=x;
          this.y=y;
          this.t=t;
         }

         public int x,y,t;

         @Override //Source https://www.geeksforgeeks.org/overriding-equals-method-in-java/
         public boolean equals(Object o) { 
  
          // If the object is compared with itself then return true   
          if (o == this) { 
              return true; 
          } 
  
          /* Check if o is an instance of Complex or not 
            "null instanceof [type]" also returns false */
          if (!(o instanceof CoronaT)) { 
              return false; 
          } 
          
          // typecast o to Complex so that we can compare data members  
          CoronaT c = (CoronaT) o; 
          
          // Compare the data members and return accordingly  
          return Double.compare(x, c.x) == 0
                  && Double.compare(y, c.y) == 0
                  && Double.compare(t, c.t) ==0 ; 
         } 
   }

  public static void appendAirports (Queue<CoronaT> q, List<Point> airports, int i, int j, int[][] mapT, int time){
      while(!airports.isEmpty()){
        Point p = airports.remove(0);
        if (i!=p.x || j!=p.y){
           mapT[p.x][p.y] = time;
           q.add(new CoronaT(p.x, p.y, time));
        }
      }
  }

  public static void find_path (int i, int j, int k, char[][] grid, int N, int M, Queue<CoronaT> q){
     if (i+1 <= N-1 && legal(grid[i+1][j]) && !q.contains(new CoronaT(i+1,j,k))){
        grid[i][j]='U';
     }
     else if (j+1 <= M-1 && legal(grid[i][j+1]) && !q.contains(new CoronaT(i,j+1,k))){
        grid[i][j]='L';
     }
     else if (j-1 >= 0 && legal(grid[i][j-1]) && !q.contains(new CoronaT(i,j-1,k))){
        grid[i][j]='R';
     }
     else{ 
        grid[i][j]='D';
     }
  }

  public static boolean legal(char x){
     if (x == 'R' || x == 'U' || x == 'L' || x == 'D' || x == 'S'){
        return true;
     }
     else{
        return false;
     }
  }  

  public static void main (String[] args) throws Exception {
      FileReader fileReader = new FileReader(args[0]);
      BufferedReader bufferedReader = new BufferedReader(fileReader);
      List<char []> lines = new ArrayList<char []>();
      String line = null;
      int N = 0;
      int M = 0;
      int s_i, s_j, t_i, t_j, w_i, w_j;
      s_i = s_j = w_i = w_j = t_i = t_j = 0;
      while ((line = bufferedReader.readLine()) != null) {
          if (N == 0) {
        	  M = (int)line.length();
          }
    	  lines.add(line.toCharArray());
          N++;
      }
      bufferedReader.close();
      char[][] grid = lines.toArray(new char[lines.size()][]);
      int[][] mapT = new int[N][M];

      Queue<CoronaT> q = new LinkedList<CoronaT>(){
          @Override
          public boolean equals(Object obj) {
             return (this == obj);
          }
      };
      List<Point> airports = new ArrayList<>();
      for (int i = 0; i < N; i++) {
    	  for (int j = 0; j < M; j++) {
    	  //System.out.print(grid[i][j]);
          mapT[i][j]=-1;
          if (grid[i][j]=='S'){
             s_i = i;
             s_j = j;
          }
          else if (grid[i][j]=='T'){
             t_i = i;
             t_j = j;
          }
          else if (grid[i][j]=='W'){
             w_i = i;
             w_j = j;
             mapT[i][j]=0;
             q.add(new CoronaT(i, j, 0));
          }
          else if (grid[i][j]=='A'){
             airports.add(new Point(i,j));
          }
    	  //System.out.print(mapT[i][j]);
    	  }
    	  //System.out.println();
      }
      q.add(new CoronaT(w_i,w_j,0));
      for (int i = 0; i < N; i++) {
    	  for (int j = 0; j < M; j++) {
            //System.out.print(mapT[i][j]);
    	  }
    	  //System.out.println();
      }
      boolean checked = false;
      while(q.peek() != null){
         //System.out.println(q.peek().x+" "+q.peek().y + " " + q.peek().t);
         CoronaT curr = q.remove();
         int i = curr.x;
         int j = curr.y;
         int time = curr.t;
         if (grid[i][j] == 'A' && !checked){
            appendAirports(q, airports, i, j, mapT, time+5);
            checked=true;
         }
         else{
            if ((i-1)>=0 && grid[i-1][j] !='X' && mapT[i-1][j] ==-1){
                mapT[i-1][j] = time+2;
                q.add(new CoronaT(i-1, j, time+2));
            }
            if (i+1 <= N-1 && grid[i+1][j] !='X' && mapT[i+1][j] ==-1){
               mapT[i+1][j] = time+2;
               q.add(new CoronaT(i+1, j, time+2));
            }
            if (j-1 >= 0 && grid[i][j-1] !='X' && mapT[i][j-1] ==-1){
      	       mapT[i][j-1] = time+2;
       	       q.add(new CoronaT(i, j-1, time+2));
            }
            if (j+1 <= M-1 && grid[i][j+1] !='X' && mapT[i][j+1] ==-1){
               mapT[i][j+1] = time+2;
               q.add(new CoronaT(i, j+1, time+2));
            }
         }
      }
      q.add(new CoronaT(s_i,s_j,0));
      
      while (q.peek()!=null){
  	 CoronaT curr=q.peek();
         int i = curr.x;
         int j = curr.y;
         int k = curr.t;
 	 if (grid[i][j]=='T'){
           Queue<CoronaT> q2 = new LinkedList<CoronaT>(q);
           find_path(i, j, k+1, grid, N, M, q2);
           break;
         }
         else{
           q.remove();
           if (i-1>=0 && !legal(grid[i-1][j]) && mapT[i-1][j]>k+1 && mapT[i-1][j]!=-1){
             q.add(new CoronaT(i-1,j,k+1));
             if (grid[i-1][j] =='T'){
                continue;
             }
             else if (i<=N-1 && legal(grid[i][j])){
                grid[i-1][j]='U';
             }
             else if (j+1<=M-1 && legal(grid[i-1][j+1])){
                grid[i-1][j]='L';
             }
             else if (j-1>0 && legal(grid[i-1][j-1])){
                grid[i-1][j]='R';
             }
             else{
                grid[i-1][j]='D';
             }
           }
           if (i+1<=N-1 && !legal(grid[i+1][j]) && mapT[i+1][j]>k+1 && mapT[i+1][j]!=-1){
              q.add(new CoronaT(i+1,j,k+1));
              if (grid[i+1][j] =='T'){
                 continue;
              }
              else if (i+2<=N-1 && legal(grid[i+2][j])){
                 grid[i+1][j]='U';
              }
              else if (j+1<=M-1 && legal(grid[i+1][j+1])){
                 grid[i+1][j]='L';
              }
              else if (j-1>=0 && legal(grid[i+1][j-1])){
                 grid[i+1][j]='R';
              }
              else{
                 grid[i+1][j]='D';
              }
           }
           if (j+1<=M-1 && !legal(grid[i][j+1]) && mapT[i][j+1]>k+1 && mapT[i][j+1]!=-1){
              q.add(new CoronaT(i,j+1,k+1));
              if (grid[i][j+1] =='T'){
                 continue;
              }
              else if (i+1<=N-1 && legal(grid[i+1][j+1])){
                 grid[i][j+1]='U';
              }
              else if (j+2<=M-1 && legal(grid[i][j+2])){
                 grid[i][j+1]='L';
              }
              else if (j<=M-1 && legal(grid[i][j])){
                 grid[i][j+1]='R';
              }
              else{
                 grid[i][j+1]='D';
              }
           }
           if (j-1>=0 && !legal(grid[i][j-1]) && mapT[i][j-1]>k+1 && mapT[i][j-1]!=-1){
              q.add(new CoronaT(i,j-1,k+1));
              if (grid[i][j-1] =='T'){
                 continue;
              }
              else if (i+1<=N-1 && legal(grid[i+1][j-1])){
                 grid[i][j-1]='U';
              }
              else if (j<=M-1 && legal(grid[i][j])){
                 grid[i][j-1]='L';
              }
              else if (j-2>=0 && legal(grid[i][j-2])){
                 grid[i][j-1]='R';
              }
              else{
                 grid[i][j-1]='D';
              }
           }
         }
     }
      String st="";
      if (q.peek()!=null){
     while (t_i!=s_i || t_j!=s_j){
        if (grid[t_i][t_j]=='U'){
             	st='U'+st;
        t_i=t_i+1;
            }
        else if (grid[t_i][t_j]=='L'){
        st='L'+st;
        t_j=t_j+1;
            }
        else if (grid[t_i][t_j]=='R'){
        st='R'+st;
        t_j=t_j-1;
            }
        else{
        st='D'+st;
        t_i=t_i-1;
            }
         }
      }
      else{
      st="IMPOSSIBLE" ;
      }
      if (st=="IMPOSSIBLE"){
        System.out.print("IMPOSSIBLE");
      }
      else{
        System.out.println(st.length());
    System.out.println(st);
      }
  }
}