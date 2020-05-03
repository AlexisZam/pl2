#pragma JessieIntegerModel(math)

/*@ predicate prime(int n) =
  @   n >= 0 &&
  @   n != 1 &&
  @     \forall integer m; n % m == 0 ==> (m == n || m == 1);
  @*/

/*@ behavior prime:
  @   assumes prime(n);
  @   ensures \result != 0;
  @ behavior not_prime:
  @   assumes !prime(n);
  @   ensures \result == 0;
  @*/
int prime(int n) {
    int p;

    if (n < 2)
        return 0;

    /*@ loop invariant
    @   2 <= p <= n;
    @ loop invariant
    @   \forall integer i; 2 <= i < p ==> n % i != 0;
    @ loop variant
    @   n-p;
    @*/
    for (p = 2; n % p != 0; p++)
        ;
    return p == n;
}

/*@ lemma mod_not_zero_not_eq:
  @   \forall integer n, integer p; n % p != 0 ==> p != n;
  @*/

/*@ lemma prime_equiv:
  @   \forall int n;
  @     prime(n) <==>
  @       n >= 2 &&
  @       \forall integer i; 2 <= i < n ==> n % i != 0;
  @*/
