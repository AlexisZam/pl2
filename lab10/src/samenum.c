#define MAXN 1000000
#define MAXV 2000000

/*@ predicate foo(int N, int *x, int r) = \exists int i, j; x[i] == x[j] && i < j && r == j - i;
  @*/

/*@ predicate bar(int N, int *x, int r) = foo(N, x, r) && \forall int rr; foo(N, x, rr) ==> r >= rr;
  @*/

/*@ requires 2 <= N <= MAXN;
  @ requires \valid_read(x + (0 .. N - 1));
  @ requires \forall int i; 0 <= i < N ==> 1 <= x[i] <= MAXV;
  @ ensures \result != 0 <==> bar(N, x, \result);
  @*/
int samenum(int N, int *x) {
    int p[MAXV + 1];
    /*@ loop assigns i, p[0 .. MAXV];
      @ loop invariant 0 <= i <= MAXV + 1;
      @ loop invariant \forall int j; 0 <= j < i ==> p[j] == -1;
      @ loop variant MAXV - i;
      @*/
    for (int i = 0; i <= MAXV; ++i)
        p[i] = -1;
    int best = 0;
    /*@ loop assigns best, i, p[0 .. MAXV];
      @ loop invariant 0 <= i <= N;
      @ loop invariant \forall int j; 0 <= j < i ==> p[x[j]] == j || best >= j - p[x[j]];
      @ loop variant N - 1 - i;
      @*/
    for (int i = 0; i < N; ++i)
        if (p[x[i]] == -1)
            p[x[i]] = i;
        else if (i - p[x[i]] > best)
            best = i - p[x[i]];
    return best;
}
