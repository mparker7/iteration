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

    ##  [1] -1.97800245  1.38383798  0.13052475 -0.72813573  0.25288764
    ##  [6] -0.11824257  2.06221941  0.15147035 -0.53733147 -0.37633414
    ## [11]  0.01590728  0.15219264 -0.32277531 -0.65283733 -0.16915099
    ## [16] -1.65729119 -0.15747822  0.83740694 -0.71075283  0.45515731
    ## [21] -0.61902568 -0.17542844 -2.06728683 -0.91218473  1.75239514
    ## [26]  0.04058361  0.79727650  1.64583390  0.80403560  0.70052885

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1] -0.344682201  2.426077775 -0.303647085  0.773166537  0.319215708
    ##  [6] -1.386003784 -0.284835860 -0.004309907  1.404130198 -0.828880795
    ## [11]  1.645445261 -0.608380929 -0.882036074 -1.834326277  1.296896786
    ## [16] -0.235388748  0.585179313 -0.043096011  0.386284432  0.930611927
    ## [21] -1.194383538 -0.811332514  0.213889555  1.140961415 -0.563413934
    ## [26] -1.099509701  0.713926294  0.028733877 -1.319717211 -0.120574510

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

    ##  [1] -0.344682201  2.426077775 -0.303647085  0.773166537  0.319215708
    ##  [6] -1.386003784 -0.284835860 -0.004309907  1.404130198 -0.828880795
    ## [11]  1.645445261 -0.608380929 -0.882036074 -1.834326277  1.296896786
    ## [16] -0.235388748  0.585179313 -0.043096011  0.386284432  0.930611927
    ## [21] -1.194383538 -0.811332514  0.213889555  1.140961415 -0.563413934
    ## [26] -1.099509701  0.713926294  0.028733877 -1.319717211 -0.120574510

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
    ## [1] 5.996131
    ## 
    ## $sd_input
    ## [1] 0.2643175
    ## 
    ## $z_score
    ##  [1] -0.344682201  2.426077775 -0.303647085  0.773166537  0.319215708
    ##  [6] -1.386003784 -0.284835860 -0.004309907  1.404130198 -0.828880795
    ## [11]  1.645445261 -0.608380929 -0.882036074 -1.834326277  1.296896786
    ## [16] -0.235388748  0.585179313 -0.043096011  0.386284432  0.930611927
    ## [21] -1.194383538 -0.811332514  0.213889555  1.140961415 -0.563413934
    ## [26] -1.099509701  0.713926294  0.028733877 -1.319717211 -0.120574510

## Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

``` r
sim_reggresion = function(n, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )

  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
  
}

sim_reggresion(n = 30, beta0 = 3, beta1 = 5)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      3.35      4.73

``` r
sim_reggresion(30, 4, 5)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      3.66      5.06

``` r
sim_reggresion(30)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      1.84      2.85
