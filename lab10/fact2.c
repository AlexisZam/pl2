#pragma JessieIntegerModel(math)

/*@ logic integer factorial (integer n) =
  @   (n > 0) ? n * factorial(n-1)
  @           : 1;
  @*/

/*@ requires  n >= 0;
  @ ensures   \result == factorial(n);
  @*/
int f(int n) {
    int i, p;
    /*@ loop invariant
    @   2 <= i;
    @ loop invariant
    @   n == 0 ==> p == 1;
    @ loop invariant
    @   n > 0 ==> i <= n+1 && p == factorial(i-1);
    @ loop variant
    @   n-i;
    @*/
    for (i = 2, p = 1; i <= n; i++)
        p *= i;
    return p;
}

/*@ lemma mult_commut:
  @   \forall integer p, integer q;
  @     p * q == q * p;
  @*/

/*@ lemma factorial_zero:
  @   factorial(0) == 1;
  @*/

/*@ lemma factorial_one:
  @   factorial(1) == 1;
  @*/

/*@ lemma factorial_succ:
  @   \forall integer n;
  @     n > 0 ==>
  @       factorial(n) == n * factorial(n-1);
  @*/
