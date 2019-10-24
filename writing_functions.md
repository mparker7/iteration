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

    ##  [1]  1.6781232280  0.1449994280 -1.5819540393  0.0003664904  0.3435680035
    ##  [6]  1.1399645355 -0.5157442925 -0.5309554570  0.6562544283 -0.7744882756
    ## [11]  0.3788563852 -1.6707919274  0.1573989912 -1.5549245169  0.0226154515
    ## [16]  1.4659263062  0.1963686688  0.3277887600  0.2488666949  1.1851307391
    ## [21] -1.4648237180 -0.5481354794 -0.4487623752  1.4152886121 -1.0668269657
    ## [26] -1.3994342927  0.5298904852 -0.4535682490  0.3362989045  1.7827034762

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  0.09277053 -0.78393464  1.10636698  0.26655946 -1.71291566
    ##  [6]  0.37778419  0.74486394  1.26970351  0.73138940 -2.54283564
    ## [11] -0.15295045  1.34915072 -0.95694057 -0.17079743  0.72186811
    ## [16] -1.93029939  0.04977274 -0.16064209 -0.73445213  0.67210015
    ## [21]  1.29278342 -0.18520230  0.40737543  1.31869771 -0.50776617
    ## [26] -0.96767299  0.63721549  0.52042597  0.33697623 -1.08939450

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

Try out the function

``` r
z_score(x = x_again)
```

    ##  [1]  0.09277053 -0.78393464  1.10636698  0.26655946 -1.71291566
    ##  [6]  0.37778419  0.74486394  1.26970351  0.73138940 -2.54283564
    ## [11] -0.15295045  1.34915072 -0.95694057 -0.17079743  0.72186811
    ## [16] -1.93029939  0.04977274 -0.16064209 -0.73445213  0.67210015
    ## [21]  1.29278342 -0.18520230  0.40737543  1.31869771 -0.50776617
    ## [26] -0.96767299  0.63721549  0.52042597  0.33697623 -1.08939450

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
    ## [1] 5.964092
    ## 
    ## $sd_input
    ## [1] 0.2708813
    ## 
    ## $z_score
    ##  [1]  0.09277053 -0.78393464  1.10636698  0.26655946 -1.71291566
    ##  [6]  0.37778419  0.74486394  1.26970351  0.73138940 -2.54283564
    ## [11] -0.15295045  1.34915072 -0.95694057 -0.17079743  0.72186811
    ## [16] -1.93029939  0.04977274 -0.16064209 -0.73445213  0.67210015
    ## [21]  1.29278342 -0.18520230  0.40737543  1.31869771 -0.50776617
    ## [26] -0.96767299  0.63721549  0.52042597  0.33697623 -1.08939450

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
    ## 1      3.09      4.86

``` r
sim_reggresion(30, 4, 5)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      3.98      4.93

``` r
sim_reggresion(30)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      1.80      3.31

## Scrape lots of napoleon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Now as a function

``` r
read_page_reviews = function(page_url) {
  
  dynamite_html = read_html(page_url)

  review_titles = dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)
 
  reviews
   
}
```

``` r
read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5")
```

    ## # A tibble: 10 x 3
    ##    title                      stars      text                              
    ##    <chr>                      <chr>      <chr>                             
    ##  1 "shut up tina you fat lar… 5.0 out o… i LOVE napoleon.                  
    ##  2 "Laughter is the Best Med… 5.0 out o… FAST SHIPPING! Love this Movie! L…
    ##  3 "New condition\n         … 5.0 out o… Classic for the kids to watch.    
    ##  4 "Napoleon, give me some o… 5.0 out o… Cul                               
    ##  5 "Yes rent\n            "   5.0 out o… Always an amazing movie, classic! 
    ##  6 "Cult classic.\n         … 5.0 out o… I should’ve bought this movie a l…
    ##  7 "DIDN'T WORK\n           … 1.0 out o… I paid for the rental, but it's n…
    ##  8 "I\n            "          5.0 out o… I love this movie! My kids love t…
    ##  9 "Laugh out loud\n        … 5.0 out o… Introduced my grandsons to this m…
    ## 10 "Stupid, funny, goofy\n  … 5.0 out o… Stupid but funny!

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
