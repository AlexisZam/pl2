#pragma JessieIntegerModel(math)
#pragma JessieFloatModel(math)

/*@ predicate profit (integer n, double *x, real k) =
  @   \exists integer i, integer j;
  @     0 <= i <= j < n &&
  @     k == x[j] / x[i];
  @*/

/*@ predicate best (integer n, double *x, real k) =
  @   profit(n, x, k) &&
  @   (\forall real kk; profit(n, x, kk) ==> kk <= k);
  @*/

/*@ requires  n > 0;
  @ requires  \valid_range(x, 0, n-1);
  @ requires  \forall integer i; 0 <= i < n ==> x[i] > 0.0;
  @ ensures   best(n, x, \result);
  @*/
double profit(int n, double x[]) {
    double k, best = 1.0, min = x[0];
    int i;

    /*@ loop invariant  1 <= i <= n;
    @ loop invariant  \exists integer k; 0 <= k < i && min == x[k];
    @ loop invariant  \forall integer k; 0 <= k < i ==> min <= x[k];
    @ loop invariant  best(i, x, best);
    @ loop variant    n-i;
    @*/
    for (i = 1; i < n; i++) {
        k = x[i] / min;
        if (k > best)
            best = k;
        if (x[i] < min)
            min = x[i];
    }
    return best;
}

/*@ lemma best_one:
  @   \forall double *x;
  @     best(1, x, 1.0);
  @
  @ lemma best_ij_update:
  @   \forall integer n, double *x, real best, real min;
  @     (\exists integer k; 0 <= k < n && min == x[k]) &&
  @     (\forall integer k; 0 <= k < n ==> min <= x[k]) &&
  @     best(n, x, best) &&
  @     x[n] / min > best ==>
  @       best(n+1, x, x[n] / min);
  @
  @ lemma best_ij_no_update:
  @   \forall integer n, double *x, real best, real min;
  @     (\exists integer k; 0 <= k < n && min == x[k]) &&
  @     (\forall integer k; 0 <= k < n ==> min <= x[k]) &&
  @     best(n, x, best) &&
  @     x[n] / min <= best ==>
  @       best(n+1, x, best);
  @*/
