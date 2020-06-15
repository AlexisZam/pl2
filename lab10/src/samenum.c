/* Frama-C 21.0 (Scandium)
   Alt-Ergo 2.3.2
   frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 300 -wp-verbose 0 samenum.c -then -report
*/

#define MAXN 1000000
#define MAXV 2000000

/*@ predicate foo(integer N, int *x, integer r, integer i, integer j) =
      x[i] == x[j] && 0 <= i < j < N && r == j - i;
*/

/*@ predicate samenum{L}(integer N, int *x, integer r) =
      (\exists int i, j; foo(N, x, r, i, j)) &&
      (\forall int i, j, s; foo(N, x, s, i, j) ==> s <= r) ==> r != 0;
*/

/*@ requires  2 <= N <= MAXN &&
              \valid_read(x + (0 .. N - 1)) &&
              \forall int i; 0 <= i < N ==> 1 <= x[i] <= MAXV;
    ensures   samenum(N, x, \result);
*/
int samenum(int N, int *x) {
    int p[MAXV + 1];
    /*@ loop assigns    i, p[0 .. MAXV];
        loop invariant  0 <= i <= MAXV + 1 &&
                        \forall int j; 0 <= j < i ==> p[j] == -1;
        loop variant    MAXV - i;
    */
    for (int i = 0; i <= MAXV; ++i)
        p[i] = -1;
    int best = 0;
    /*@ loop assigns    best, i, p[0 .. MAXV];
        loop invariant  0 <= i <= N &&
                        \forall int j; 0 <= j <= MAXV ==> -1 <= p[j] < N &&
                        \forall int j; 0 <= j < i ==> p[x[j]] != -1 &&
                        samenum(i, x, best);
        loop variant    N - 1 - i;
    */
    for (int i = 0; i < N; ++i)
        if (p[x[i]] == -1)
            p[x[i]] = i;
        else if (i - p[x[i]] > best)
            best = i - p[x[i]];
    return best;
}
