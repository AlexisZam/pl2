#pragma JessieIntegerModel(math)
#pragma JessieFloatModel(math)

/*@ predicate profit_ij (integer n, integer i, integer j, double *x, real k) =
  @   \exists integer ii, integer jj;
  @     0 <= ii <= jj < n &&
  @     (ii < i || ii == i && jj < j) &&
  @     k == x[jj] / x[ii];
  @*/

/*@ predicate best_ij (integer n, integer i, integer j, double *x, real k) =
  @   profit_ij(n, i, j, x, k) &&
  @   (\forall real kk; profit_ij(n, i, j, x, kk) ==> kk <= k);
  @*/

/*@ predicate profit (integer n, double *x, real k) =
  @   best_ij(n, n, n, x, k);
  @*/

/*@ requires  n > 0;
  @ requires  \valid_range(x, 0, n-1);
  @ requires  \forall integer i; 0 <= i < n ==> x[i] > 0.0;
  @ ensures   profit(n, x, \result);
  @*/
double profit(int n, double x[]) {
    double k, best = 1.0;
    int i, j;

    /*@ loop invariant  0 <= i <= n;
    @ loop invariant  i == 0 || best_ij(n, i, i, x, best);
    @ loop variant    n-i;
    @*/
    for (i = 0; i < n; i++)
        /*@ loop invariant  i <= j <= n;
      @ loop invariant  i == 0 && j == 0 || best_ij(n, i, j, x, best);
      @ loop variant    n-j;
      @*/
        for (j = i; j < n; j++) {
            k = x[j] / x[i];
            if (k > best)
                best = k;
        }
    return best;
}

/*@ lemma best_ij_update:
  @   \forall integer n, integer i, integer j, double *x, real best;
  @     0 <= i <= j < n &&
  @     best_ij(n, i, j, x, best) &&
  @     x[j] / x[i] > best ==>
  @       best_ij(n, i, j+1, x, x[j] / x[i]);
  @
  @ lemma best_ij_no_update:
  @   \forall integer n, integer i, integer j, double *x, real best;
  @     0 <= i <= j < n &&
  @     best_ij(n, i, j, x, best) &&
  @     x[j] / x[i] <= best ==>
  @       best_ij(n, i, j+1, x, best);
  @
  @ lemma best_ij_zero:
  @   \forall integer n, integer i, double *x, real best;
  @     0 <= i < n &&
  @     best_ij(n, i, n, x, best) ==>
  @       best_ij(n, i+1, 0, x, best);
  @
  @ lemma best_ij_idemp:
  @   \forall integer n, integer i, double *x, real best;
  @     0 <= i <= n &&
  @     best_ij(n, i, 0, x, best) ==>
  @       best_ij(n, i, i, x, best);
  @*/
