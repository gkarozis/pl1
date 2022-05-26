import sys
import collections
from collections import deque

q=deque()


with open(sys.argv[1], 'r') as infile:
    grid = infile.readlines()

grid = [list(line.strip('\n')) for line in grid]
N, M = len(grid), len(grid[0])

mapTimes = []


mapT = [[-1 for i in range(M)] for i in range(N) ]

airports = []

for i in range(N):
  for j in range(M):
    if grid[i][j]=='S':
       s_coordinates = (i,j)
    elif grid[i][j] == 'T':
       t_coordinates = (i,j)
    elif grid[i][j] == 'W':
       w_coordinates = (i,j)
    elif grid[i][j] == 'A':
       airports.append((i,j))

q.append((w_coordinates,0))
x=w_coordinates[0]
y=w_coordinates[1]
mapT[x][y]=0


def append_airports (q, airports, coord, mapT, time):

   while airports!=[]:
     if airports[0] != coord: 
       x = airports[0][0]
       y = airports[0][1]
       mapT[x][y] = time 
       q.append((airports.pop(),time))
     else: del airports[0]

k=0
checked = False
while q: 
   curr = q.popleft()
   coord = curr[0]
   (i,j) = coord
   time = curr[1]
   if grid[i][j] == 'A' and not checked:
      append_airports(q, airports, coord, mapT, time+5)
      checked=True
   else:
     if (i-1)>=0 and grid[i-1][j] !='X' and mapT[i-1][j] ==-1:
         mapT[i-1][j] = time+2
         q.append(((i-1,j),time+2))
     if (i+1)<=(N-1) and grid[i+1][j] !='X' and mapT[i+1][j] ==-1:
         mapT[i+1][j] = time+2
         q.append(((i+1,j),time+2))
     if (j-1)>=0 and grid[i][j-1] !='X' and mapT[i][j-1] ==-1:
         mapT[i][j-1] = time+2
         q.append(((i,j-1),time+2))
     if (j+1)<=(M-1) and grid[i][j+1] !='X' and mapT[i][j+1] ==-1:
         mapT[i][j+1] = time+2
         q.append(((i,j+1),time+2))
     k=k+1

def legal (x): 
   if x == 'R' or x == 'U' or x == 'L' or x == 'D' or x == 'S':
      return True
   else:
      return False

q.append((s_coordinates,0))


def find_path (curr, grid, N, M, q):
   (i,j)=curr[0]
   k=curr[1]
   if (i+1)<=N-1 and legal(grid[i+1][j]) and ((i+1,j),k) not in q:
      grid[i][j]='U'
   elif (j+1)<=(M-1) and legal(grid[i][j+1]) and ((i,j-1),k) not in q:
      grid[i][j]='L'
   elif (j-1)>=0 and legal(grid[i][j-1]) and ((i,j-1),k) not in q:
      grid[i][j]='R'
   else: 
      grid[i][j]='D' 

while q:
   curr=q[0]
   (i,j) = curr[0] 
   k = curr[1]
   if grid[i][j]=='T':
      d = collections.deque(q)
      find_path(((i,j),k+1), grid, N, M, d)
      break;
   else:
      q.popleft()
      if i-1>=0 and not legal(grid[i-1][j]) and mapT[i-1][j]>k+1 and mapT[i-1][j]!=-1:
         q.append(((i-1,j),k+1))
         if grid[i-1][j] =='T':
            continue
         elif i<=N-1 and legal(grid[i][j]):
              grid[i-1][j]='U'
         elif j+1<=M-1 and legal(grid[i-1][j+1]):
              grid[i-1][j]='L'
         elif j-1>0 and legal(grid[i-1][j-1]):
              grid[i-1][j]='R'
         else:
              grid[i-1][j]='D'
      if i+1<=N-1 and not legal(grid[i+1][j]) and mapT[i+1][j]>k+1 and mapT[i+1][j]!=-1:
         q.append(((i+1,j),k+1))
         if grid[i+1][j] =='T':
            continue
         elif i+2<=N-1 and legal(grid[i+2][j]):
            grid[i+1][j]='U'
         elif j+1<=M-1 and legal(grid[i+1][j+1]):
            grid[i+1][j]='L'
         elif j-1>=0 and legal(grid[i+1][j-1]):
            grid[i+1][j]='R'
         else:
            grid[i+1][j]='D'
      if j+1<=M-1 and not legal(grid[i][j+1]) and mapT[i][j+1]>k+1 and mapT[i][j+1]!=-1:
         q.append(((i,j+1),k+1))
         if grid[i][j+1] =='T':
            continue
         elif i+1<=N-1 and legal(grid[i+1][j+1]):
            grid[i][j+1]='U'
         elif j+2<=M-1 and legal(grid[i][j+2]):
            grid[i][j+1]='L'
         elif j<=M-1 and legal(grid[i][j]):
            grid[i][j+1]='R'
         else:
            grid[i][j+1]='D'
      if j-1>=0 and not legal(grid[i][j-1]) and mapT[i][j-1]>k+1 and mapT[i][j-1]!=-1:
         q.append(((i,j-1),k+1))
         if grid[i][j-1] =='T':
            continue
         elif i+1<=N-1 and legal(grid[i+1][j-1]):
            grid[i][j-1]='U'
         elif j<=M-1 and legal(grid[i][j]):
            grid[i][j-1]='L'
         elif j-2>=0 and legal(grid[i][j-2]):
            grid[i][j-1]='R'
         else:
            grid[i][j-1]='D'

st=""
if q:
  (t1,t2)=t_coordinates
  while (t1,t2)!=s_coordinates:
     if grid[t1][t2]=='U':
        st='U'+st
        t1=t1+1
     elif grid[t1][t2]=='L':
        st='L'+st
        t2=t2+1
     elif grid[t1][t2]=='R':
        st='R'+st
        t2=t2-1
     else:
        st='D'+st
        t1=t1-1
else:
  st="IMPOSSIBLE" 

if st=="IMPOSSIBLE":
  print("IMPOSSIBLE")
else:
  print(len(st))
  print(st)
