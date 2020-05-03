#pragma JessieIntegerModel(math)

/*@ logic integer POW (integer x, integer n) =
  @   (n > 0) ? x * POW(x, n-1)
  @           : 1;
  @*/

/*@ lemma pow_zero:
  @   \forall integer x;
  @     POW(x, 0) == 1;
  @*/

/*@ requires  n >= 0;
  @ ensures   \result == POW(x, n);
  @*/
unsigned long pow(unsigned long x, unsigned int n) {
    unsigned long r, i;

    /*@ loop invariant 1 <= i <= n+1;
    @ loop invariant r == POW(x, i-1);
    @ loop variant n-i;
    @*/
    for (r = 1, i = 1; i <= n; i++)
        r *= x;
    return r;
}
