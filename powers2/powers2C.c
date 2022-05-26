#include <stdio.h>
#include <stdlib.h>


int main(int argc, char *argv[])
{
    int T, i, flag=1,j,c,r1,r2;
    FILE *fp;
    fp = fopen(argv[1], "r");
    r1 = fscanf(fp,"%d", &T);
    if (r1<=0) exit(-1);
    int N[T], K[T];
    for (c=0;c<T;c++)
    {
        r2 = fscanf(fp, "%d %d", &N[c], &K[c]);
        if (r2<0) exit(-1);
    }
    fclose(fp);
    for (c=0; c<T; c++)
    {
        if (K[c]>N[c])
        {
            printf("[]\n");
        }
        else if (N[c]==K[c])
        {
            printf("[%d]\n", N[c]);
        }
        else
        {
            int p=1, A[K[c]],sum = N[c], B[K[c]];
            for (i=0; i<K[c]; i++)
            {
                A[i] = 0;
                B[i] = 0;
            }
            i = 0;
            while (sum>0)
            {
                A[i] = p;
                p = p*2;
                if (A[i] > sum)
                {
                    if (i==K[c])
                    {
                        flag = 0;
                        break;
                    }
                    else
                    {
                        flag=1;
                        p = 1;
                        A[i] = A[i]/2;
                        sum = sum - A[i];
                        i++;
                    }
                }
            }
            if (flag == 0)
            {
                printf("[]\n");
                flag = 1;
            }
            else
            {
                int no_of_0 = 0;
                j = 0;
                for (i=K[c]-1; i>-1; i--)
                {
                    if (A[i]==0) no_of_0++;
                    else break;
                }
                for (i=K[c]-1; i>-1; i--)
                {
                    if (A[i]==0) continue;
                    else if (A[i]==1)
                    {
                        B[j] = 1;
                        j++;
                        A[i] = 0;
                    }
                    else
                    {
                        if (no_of_0<=0)
                        {
                            B[j] = A[i];
                            j++;
                            A[i] = 0;
                        }
                        else if (no_of_0==1)
                        {
                            B[j] = A[i]/2;
                            j++;
                            B[j] = A[i]/2;
                            j++;
                            A[i]=0;
                            no_of_0=0;
                        }
                        else
                        {
                            int x = no_of_0 + 1;
                            int temp[x], t;
                            for (t=0; t<x; t++) temp[t]=0;
                            t=0;
                            temp[0] = A[i];
                            A[i] = 0;
                            while (1)
                            {
                                temp[t] = temp[t]/2;
                                temp[t+1] = temp[t];
                                t++;
                                no_of_0--;
                                if (no_of_0==0) break;
                                if (t==x-1) break; //gemhse o pinakas
                                if (temp[t]==1)
                                {
                                    B[j] = 1;
                                    j++;
                                    B[j] = 1;
                                    j++;
                                    temp[t] = 0;
                                    temp[t-1] = 0;
                                    t = t-2;
                                }
                                if (temp[0]==0) break; //analythike o pinakas
                            }
                            int y;
                            for (y=t; y>-1; y--)
                            {
                                if (temp[y] > 0)
                                {
                                    B[j] = temp[y];
                                    j++;
                                }
                            }
                        }
                    }
                }
                
                int power = 1;
                i=0;
                j=0;
                printf("[");
                while (i<K[c])
                {
                    if (B[i] == power)
                    {
                        i++;
                        j++;
                    }
                    else
                    {
                        printf("%d", j);
                        j = 0;
                        power = power*2;
                        printf(",");
                    }
                }
                printf("%d",j);
                printf("]\n");
            }
        }
    }
    return 0;
}