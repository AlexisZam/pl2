#define MAXN 1000000
#define MAXV 2000000

/*@ predicate samenum(int N, int *x, int r) =
  @   \exists integer i, j; x[i] == x[j] && i < j && r == j - i;
  @*/

/*@ predicate best(int N, int *x, int r) =
  @   samenum(N, x, r) && \forall int rr; samenum(N, x, rr) ==> r >= rr;
  @*/

// ensures \result != 0 <==> \exists int i, j;
//          x[i] == x[j] && i < j && \result == j - i && \forall int ii, jj;
//          x[i] != x[ii] && x[ii] == x[jj] && ii < jj ==> \result >= jj - ii;

/*@ requires 2 <= N <= MAXN;
  @ requires \valid_read(x + (0 .. N - 1));
  @ requires \forall int i; 0 <= i < N ==> 1 <= x[i] <= MAXV;
  @ assigns \nothing;
  @ ensures best(N, x, \result);
  @*/
int samenum(int N, int *x) {
    /*@ requires \valid(p + (0 .. MAXV));
      @ requires \forall int i; 0 <= i <= MAXV ==> 0 <= p[i] <= MAXN;
      @*/
    int p[MAXV + 1];
    /*@ loop invariant 0 <= i <= MAXV + 1;
      @ loop assigns i, p[0 .. MAXV];
      @ loop variant MAXV - i;
      @*/
    for (int i = 0; i <= MAXV; ++i)
        p[i] = -1;
    //@ assert \forall int i; 0 <= i <= MAXV ==> p[i] == -1;
    int best = 0;
    //@ assert best == 0;
    /*@ loop invariant 0 <= i <= N;
      @ loop invariant \forall int i; 0 <= i < N ==> best >= \at(best, Pre);
      @ loop invariant \forall int i; 0 <= i < N ==> p[i] >= \at(p[i], Pre);
      @ loop invariant \forall int i; 0 <= i < N ==> best >= i - p[x[i]];
      @ loop assigns i, p[0 .. N - 1], best;
      @ loop variant N - 1 - i;
      @*/
    for (int i = 0; i < N; ++i)
        if (p[x[i]] == -1)
            p[x[i]] = i;
        else if (i - p[x[i]] > best)
            best = i - p[x[i]];
    return best;
}