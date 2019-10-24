Writing Functions
================
Matthew Parker
10/24/19

## Get started

We’re going to write some functions.

Here’s z scores

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = 0.3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.06253328 -0.41180015  0.40476836 -0.79809155  0.19402703
    ##  [6]  1.12915338  1.13851270  2.51787280  0.42572243 -0.62697184
    ## [11]  0.52696936  0.18660439 -1.21114446  1.44046895 -0.08799312
    ## [16] -0.62372241 -1.42467748  0.80856895 -1.81622772 -0.92052212
    ## [21] -0.63089153  0.95158552 -0.56013984 -1.77688880 -1.05449534
    ## [26]  0.43840162  0.97186247  0.43425773  0.45814750 -0.02082357

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  0.0817739  0.8923232 -0.2145450 -1.1783622 -1.8581488  0.2612304
    ##  [7]  0.6116541  0.2922892 -0.1222973  1.2938891 -1.0327788 -0.5786194
    ## [13] -0.1703679 -0.7748839  0.9806747  2.6577446 -0.3881653 -0.1348237
    ## [19] -0.9517071 -0.8234807  1.4893789  0.3162718  0.1573217 -0.4709020
    ## [25] -1.4357354 -1.3225746  0.4682759  1.6215727  0.1195887  0.2134034

Now a function.

``` r
z_score = function(x) {
  
  if (!is.numeric(x)) {
    stop("x should be numeric")
  } else if (length(x) < 3) {
    stop("x should be longer than 3")
  }
  
  (x - mean(x)) / sd(x)
  
}
```

Try out the
    function

``` r
z_score(x = x_again)
```

    ##  [1]  0.0817739  0.8923232 -0.2145450 -1.1783622 -1.8581488  0.2612304
    ##  [7]  0.6116541  0.2922892 -0.1222973  1.2938891 -1.0327788 -0.5786194
    ## [13] -0.1703679 -0.7748839  0.9806747  2.6577446 -0.3881653 -0.1348237
    ## [19] -0.9517071 -0.8234807  1.4893789  0.3162718  0.1573217 -0.4709020
    ## [25] -1.4357354 -1.3225746  0.4682759  1.6215727  0.1195887  0.2134034

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): x should be longer than 3

``` r
z_score(x = "my name is jeff")
```

    ## Error in z_score(x = "my name is jeff"): x should be numeric

``` r
z_score(x = c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(x = c(TRUE, TRUE, FALSE, TRUE)): x should be numeric

``` r
z_score(x = iris)
```

    ## Error in z_score(x = iris): x should be numeric

## Multiple outputs

``` r
mean_and_sd = function(input_x) {
  
  if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  }
  
  list(
    mean_input = mean(input_x),
    sd_input = sd(input_x),
    z_score = (input_x - mean(input_x)) / sd(input_x)
  )
}
```

Test this function

``` r
mean_and_sd(input_x = x_again)
```

    ## $mean_input
    ## [1] 5.984198
    ## 
    ## $sd_input
    ## [1] 0.3213806
    ## 
    ## $z_score
    ##  [1]  0.0817739  0.8923232 -0.2145450 -1.1783622 -1.8581488  0.2612304
    ##  [7]  0.6116541  0.2922892 -0.1222973  1.2938891 -1.0327788 -0.5786194
    ## [13] -0.1703679 -0.7748839  0.9806747  2.6577446 -0.3881653 -0.1348237
    ## [19] -0.9517071 -0.8234807  1.4893789  0.3162718  0.1573217 -0.4709020
    ## [25] -1.4357354 -1.3225746  0.4682759  1.6215727  0.1195887  0.2134034

## Multiple inputs
