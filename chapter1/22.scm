#| Exercise 1.22.

Most Lisp implementations include a primitive called runtime that
returns an integer that specifies the amount of time the system has
been running (measured, for example, in microseconds). The following
timed-prime-test procedure, when called with an integer n, prints n
and checks to see if n is prime. If n is prime, the procedure prints
three asterisks followed by the amount of time used in performing the
test. |#

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      '()))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

#| Using this procedure, write a procedure search-for-primes that
checks the primality of consecutive odd integers in a specified
range. Use your procedure to find the three smallest primes larger
than 1000; larger than 10,000; larger than 100,000; larger than
1,000,000. Note the time needed to test each prime. Since the testing
algorithm has order of growth of (√n), you should expect that testing
for primes around 10,000 should take about √10 times as long as
testing for primes around 1000. Do your timing data bear this out? How
well do the data for 100,000 and 1,000,000 support the √n prediction?
Is your result compatible with the notion that programs on your
machine run in time proportional to the number of steps required for
the computation? |#

;;; Solution
;; First need an implementation for the prime? procedure.

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Now implement procedure for checking for primes within a given interval

(define (search-for-primes range-start range-end)
  (cond ((> range-start range-end)
         (fresh-line)
         (display "Finished Search"))

        ((= 0 (remainder range-start 2))
         (search-for-primes (+ 1 range-start) range-end))

        (else (timed-prime-test range-start)
              (search-for-primes (+ range-start 2) range-end))))

(search-for-primes 1000 1050)
#|
1001
1003
1005
1007
1009 *** 4
1011
1013 *** 5
1015
1017
1019 *** 4
1021 *** 4
1023
1025
1027
1029
1031 *** 5
1033 *** 5
1035
1037
1039 *** 4
1041
1043
1045
1047
1049 *** 5
Finished Search
|#

(search-for-primes 10000 10050)
#|
10001
10003
10005
10007 *** 9
10009 *** 9
10011
10013
10015
10017
10019
10021
10023
10025
10027
10029
10031
10033
10035
10037 *** 9
10039 *** 9
10041
10043
10045
10047
10049
Finished Search
|#

(search-for-primes 100000 100050)
#|
100001
100003 *** 25
100005
100007
100009
100011
100013
100015
100017
100019 *** 24
100021
100023
100025
100027
100029
100031
100033
100035
100037
100039
100041
100043 *** 24
100045
100047
100049 *** 25
Finished Search
|#

#| Conclusion
n = 1000   -> t = 5
n = 10000  -> t = 10
n = 100000 -> t = 25

Expected growth factor equal to √(10), which is roughly equal to 3.
These results do not match this pattern, possibly because modern PCs
are too fast to demonstrate this behavior. |#

(search-for-primes 10000000000 10000000050) ; -> t = 4660
(search-for-primes 100000000000 100000000070) ; -> t = 14830

;; Using larger numbers confirms the x3 effect.
