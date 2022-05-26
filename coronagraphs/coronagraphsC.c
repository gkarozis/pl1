#include <iostream>
#include <fstream>
#include <vector>
using namespace std;


// A simple representation of graph using STL 
#include<bits/stdc++.h> 
using namespace std; 
  
// A utility function to add an edge in an 
// undirected graph. 
void addEdge(vector<int> adj[], int u, int v) 
{ 
    adj[u].push_back(v); 
    adj[v].push_back(u); 
} 
  
// A utility function to print the adjacency list 
// representation of graph 
void printGraph(vector<int> const adj[], int V) 
{ 
    for (int v = 1; v <= V; v++) 
    { 
        cout << "\n Adjacency list of vertex "
             << v << "\n head "; 
        for (auto x : adj[v]) 
           cout << "-> " << x; 
        printf("\n"); 
    } 
} 

void DfsUtil(vector<int> const adj[], int v, int visited[]){
   visited[v]=1;
   for(auto it = adj[v].begin(); it!=adj[v].end(); it++){
      if(visited[*it]==0){
         DfsUtil(adj, *it, visited);
      }
   }
}

bool DetectConnectivity(vector<int> const adj[], int visited[], int V){
   DfsUtil(adj,1,visited);
   for(int i=1; i<V; i++){
      if (visited[i]==0) return false;
   }
   return true;
}

void showstack(stack <int> s) 
{ 
    while (!s.empty()) 
    { 
        cout << '\t' << s.top(); 
        s.pop(); 
    } 
    cout << '\n'; 
} 

void DFS_help(vector<int> const graph[], stack<int>* s, int visited[],int V, bool &z, int par[]){
   int k=s->top();
   visited[k]++;
   if(visited[k]==2){
     int cur=par[k];
     visited[par[k]]=-1;
     visited[k]=-1;
     while(cur!=k){
        cur=par[cur];
        visited[cur]=-1;
     }
     z=true;
     return;
   }
   s->pop();
   for(auto it=graph[k].begin(); it!=graph[k].end(); it++){
      if(par[k]!=*it){
         par[*it]=k;
      }
      if(visited[*it]==0){
        s->push(*it);
      }
   }
   return;
}

bool DFS(vector<int> const graph[], stack<int>* s,  int visited[], int V, bool &z, int par[]){
   s->push(1);
   while(!s->empty()){
     DFS_help(graph, s, visited, V, z, par);
     if(z==true) return z;
   }
   return z;
}

int count_vertices(int i, vector<int> const graph[], int &sum, int visited[]){
  for(auto it=graph[i].begin(); it!=graph[i].end(); it++){
      if (visited[*it]==false){ 
         visited[*it]=true;
         sum=sum+1; 
         count_vertices(*it, graph, sum, visited);
      }
      else continue;
  }
  return sum;
}

int main(int argc, char **argv)
{ 
   int N;
   ifstream inFile;
   inFile.open(argv[1]);
   inFile >> N;
   for(int i=0; i<N; i++){
     int v,e,v1,v2;
     inFile >> v;
     inFile >> e;
     vector<int> graph[v+1];
     for(int j=0; j<e; j++){
       inFile >> v1;
       inFile >> v2;
       addEdge(graph, v1, v2);
     }
     int *visited=new int[v+1];
     int *par=new int[v+1];
     stack <int> s;
     stack <int> s2;
     for (int j=0; j<=v; j++)
        visited[j]=0;
     bool x=DetectConnectivity(graph, visited, v+1);
     if(!x) { printf("NO CORONA\n"); }
     else{
        for (int j=0; j<=v; j++)
           visited[j]=0;
        bool z=false;
        bool y=DFS(graph, &s, visited, v+1, z, par);
        for (int i=1; i<=v; i++){
           if(visited[i]==-1){
              int k=graph[i].size();
              int count=0;
              for(int j=0; j<k; j++){
                 //printf("i=%d j=%d visited[j]=%d \n", i, graph[i].at(count), visited[graph[i].at(count)]);
                 if(visited[graph[i].at(count)]==-1){
                    graph[i].erase(graph[i].begin()+count);
                    count--;
                 }
                 count++;
              }
           }
        }
        while(!s.empty()) s.pop();
        for (int i=1; i<=v; i++){
           if(visited[i]==-1) s.push(i);
           visited[i] = par[i] = 0;
        }
        z=false;
        y=DFS(graph, &s2, visited,v+1, z, par);
        if(y){
           printf("NO  CORONA\n");
        }
        else{
            vector<int> tupwsh;
            printf("CORONA %d\n", (int)s.size());
            for(int j=1; j<=v; j++) visited[j] = 0;
            while(!s.empty()){
               int sum=0;
               int x = count_vertices(s.top(), graph, sum, visited);
               if(x==0) tupwsh.push_back(1);
               else tupwsh.push_back(x);
               s.pop();
            }
            sort(tupwsh.begin(), tupwsh.end());
            for(unsigned int i=0; i<tupwsh.size()-1; i++){
              printf("%d ",tupwsh[i]);
            }
            printf("%d\n",(int) tupwsh.back());
            tupwsh.clear();
        }
     }
     delete [] visited;
     delete [] par;
     for(int j=0; j<=v; j++){
        graph[j].clear();
     }
   }   
   return 0;
}