LoGRTryLong
================
Mohd Azmi
01/11/2019

# Pre-amble

## Library

``` r
library(haven)
library(knitr)
library(tidyr)
library(psych)
library(ggplot2)
library(dplyr)
library(broom)
library(corrplot)
library(mfp)
library(generalhoslem)
library(ResourceSelection)
library(LogisticDx)
```

## Dataset

Import dataset

``` r
LogDS <- read_dta("diabetes.dta")
kable(head(LogDS))
```

| codesub   | age | dmdx | height | weight | waist | hip | msbpr | mdbpr | hba1c |  fbs | mogtt2h | totchol | ftrigliz |  hdl |  ldl | gender | crural |      bmi |
| :-------- | --: | ---: | -----: | -----: | ----: | --: | ----: | ----: | ----: | ---: | ------: | ------: | -------: | ---: | ---: | -----: | -----: | -------: |
| R-S615112 |  70 |    0 |   1.54 |   40.0 |  76.0 |  61 |   135 |  80.0 |   5.2 | 3.99 |    3.22 |    5.43 |     1.06 | 1.65 | 2.69 |      0 |      1 | 16.86625 |
| MAA615089 |  20 |    0 |   1.74 |   54.6 |  83.0 |  62 |   105 |  58.0 |   5.3 | 4.26 |    6.49 |    5.13 |     1.17 | 1.59 | 2.79 |      1 |      1 | 18.03409 |
| M-M616372 |  29 |    0 |   1.54 |   37.0 |  83.0 |  63 |    91 |  60.0 |   4.8 | 4.94 |    5.15 |    5.55 |     0.72 | 2.24 | 2.55 |      0 |      1 | 15.60128 |
| MFM615361 |  25 |    0 |   1.60 |   48.4 |  83.5 |  64 |   117 |  68.5 |   4.8 | 4.60 |    3.85 |    4.01 |     1.12 | 1.21 | 1.83 |      1 |      1 | 18.90625 |
| R-A615780 |  37 |    0 |   1.44 |   44.5 |  85.0 |  64 |   102 |  78.0 |   5.1 | 4.60 |    7.71 |    5.21 |     0.78 | 1.43 | 2.40 |      0 |      1 | 21.46026 |
| SAS615496 |  43 |    0 |   1.46 |   45.5 |  90.0 |  64 |   124 |  65.5 |   5.1 | 4.42 |    5.65 |    6.19 |     1.11 | 2.18 | 2.93 |      0 |      1 | 21.34547 |

Convert categorical data

``` r
LogDS$dmdxcat <- as_factor(LogDS$dmdx)
LogDS$gendercat <- as_factor(LogDS$gender)
LogDS$cruralcat <- as_factor(LogDS$crural)
kable(head(LogDS))
```

| codesub   | age | dmdx | height | weight | waist | hip | msbpr | mdbpr | hba1c |  fbs | mogtt2h | totchol | ftrigliz |  hdl |  ldl | gender | crural |      bmi | dmdxcat | gendercat | cruralcat |
| :-------- | --: | ---: | -----: | -----: | ----: | --: | ----: | ----: | ----: | ---: | ------: | ------: | -------: | ---: | ---: | -----: | -----: | -------: | :------ | :-------- | :-------- |
| R-S615112 |  70 |    0 |   1.54 |   40.0 |  76.0 |  61 |   135 |  80.0 |   5.2 | 3.99 |    3.22 |    5.43 |     1.06 | 1.65 | 2.69 |      0 |      1 | 16.86625 | no      | female    | rural     |
| MAA615089 |  20 |    0 |   1.74 |   54.6 |  83.0 |  62 |   105 |  58.0 |   5.3 | 4.26 |    6.49 |    5.13 |     1.17 | 1.59 | 2.79 |      1 |      1 | 18.03409 | no      | male      | rural     |
| M-M616372 |  29 |    0 |   1.54 |   37.0 |  83.0 |  63 |    91 |  60.0 |   4.8 | 4.94 |    5.15 |    5.55 |     0.72 | 2.24 | 2.55 |      0 |      1 | 15.60128 | no      | female    | rural     |
| MFM615361 |  25 |    0 |   1.60 |   48.4 |  83.5 |  64 |   117 |  68.5 |   4.8 | 4.60 |    3.85 |    4.01 |     1.12 | 1.21 | 1.83 |      1 |      1 | 18.90625 | no      | male      | rural     |
| R-A615780 |  37 |    0 |   1.44 |   44.5 |  85.0 |  64 |   102 |  78.0 |   5.1 | 4.60 |    7.71 |    5.21 |     0.78 | 1.43 | 2.40 |      0 |      1 | 21.46026 | no      | female    | rural     |
| SAS615496 |  43 |    0 |   1.46 |   45.5 |  90.0 |  64 |   124 |  65.5 |   5.1 | 4.42 |    5.65 |    6.19 |     1.11 | 2.18 | 2.93 |      0 |      1 | 21.34547 | no      | female    | rural     |

alternatively, dplyr::mutate can also be use

``` r
LogDS2 <- LogDS %>%
  mutate (dmdxcat = as_factor(dmdx),
          gendercat = as_factor(gender),
          cruralcat = as_factor(crural))
kable(head(LogDS2))
```

| codesub   | age | dmdx | height | weight | waist | hip | msbpr | mdbpr | hba1c |  fbs | mogtt2h | totchol | ftrigliz |  hdl |  ldl | gender | crural |      bmi | dmdxcat | gendercat | cruralcat |
| :-------- | --: | ---: | -----: | -----: | ----: | --: | ----: | ----: | ----: | ---: | ------: | ------: | -------: | ---: | ---: | -----: | -----: | -------: | :------ | :-------- | :-------- |
| R-S615112 |  70 |    0 |   1.54 |   40.0 |  76.0 |  61 |   135 |  80.0 |   5.2 | 3.99 |    3.22 |    5.43 |     1.06 | 1.65 | 2.69 |      0 |      1 | 16.86625 | no      | female    | rural     |
| MAA615089 |  20 |    0 |   1.74 |   54.6 |  83.0 |  62 |   105 |  58.0 |   5.3 | 4.26 |    6.49 |    5.13 |     1.17 | 1.59 | 2.79 |      1 |      1 | 18.03409 | no      | male      | rural     |
| M-M616372 |  29 |    0 |   1.54 |   37.0 |  83.0 |  63 |    91 |  60.0 |   4.8 | 4.94 |    5.15 |    5.55 |     0.72 | 2.24 | 2.55 |      0 |      1 | 15.60128 | no      | female    | rural     |
| MFM615361 |  25 |    0 |   1.60 |   48.4 |  83.5 |  64 |   117 |  68.5 |   4.8 | 4.60 |    3.85 |    4.01 |     1.12 | 1.21 | 1.83 |      1 |      1 | 18.90625 | no      | male      | rural     |
| R-A615780 |  37 |    0 |   1.44 |   44.5 |  85.0 |  64 |   102 |  78.0 |   5.1 | 4.60 |    7.71 |    5.21 |     0.78 | 1.43 | 2.40 |      0 |      1 | 21.46026 | no      | female    | rural     |
| SAS615496 |  43 |    0 |   1.46 |   45.5 |  90.0 |  64 |   124 |  65.5 |   5.1 | 4.42 |    5.65 |    6.19 |     1.11 | 2.18 | 2.93 |      0 |      1 | 21.34547 | no      | female    | rural     |

# Data Exploration

## Data summary

``` r
kable(summary(LogDS [, 20:22]))
```

|  | dmdxcat  |  gendercat  | cruralcat  |
|  | :------- | :---------: | :--------- |
|  | no :3363 | female:2497 | urban:2001 |
|  | yes: 457 | male :1323  | rural:1819 |

``` r
kable(describe(LogDS [, 2:19]))
```

|          | vars |    n |        mean |         sd |    median |     trimmed |       mad |       min |       max |     range |        skew |    kurtosis |        se |
| -------- | ---: | ---: | ----------: | ---------: | --------: | ----------: | --------: | --------: | --------: | --------: | ----------: | ----------: | --------: |
| age      |    1 | 3820 |  47.9196335 | 14.4684859 |  48.00000 |  48.0215969 | 14.826000 | 18.000000 |  89.00000 |  71.00000 | \-0.0266645 | \-0.5621208 | 0.2340946 |
| dmdx     |    2 | 3820 |   0.1196335 |  0.3245750 |   0.00000 |   0.0245419 |  0.000000 |  0.000000 |   1.00000 |   1.00000 |   2.3431685 |   3.4913528 | 0.0052515 |
| height   |    3 | 3820 |   1.5682906 |  0.0849144 |   1.56000 |   1.5652291 |  0.088956 |  1.290000 |   1.96000 |   0.67000 |   0.3607610 |   0.0845608 | 0.0013739 |
| weight   |    4 | 3820 |  63.2549660 | 13.2276273 |  62.00000 |  62.5842899 | 13.343400 | 30.000000 | 128.50000 |  98.50000 |   0.5470008 |   0.4297622 | 0.2140180 |
| waist    |    5 | 3818 |  85.9211524 | 12.4917722 |  86.00000 |  85.6155301 | 13.343400 | 50.800000 | 134.00000 |  83.20000 |   0.2508684 | \-0.0981115 | 0.2021651 |
| hip      |    6 | 3818 |  97.5079754 | 10.1617305 |  97.00000 |  97.3297448 |  9.711030 | 61.000000 | 148.00000 |  87.00000 |   0.1635996 |   0.6317992 | 0.1644560 |
| msbpr    |    7 | 3820 | 132.7203770 | 22.0504169 | 129.75000 | 131.1485897 | 21.127050 | 76.500000 | 210.00000 | 133.50000 |   0.6658443 |   0.3102403 | 0.3567674 |
| mdbpr    |    8 | 3820 |  77.9776178 | 11.3444799 |  77.50000 |  77.6951898 | 11.119500 | 41.500000 | 123.00000 |  81.50000 |   0.2365319 | \-0.0817571 | 0.1835494 |
| hba1c    |    9 | 3820 |   5.7817016 |  1.3791466 |   5.40000 |   5.4883508 |  0.444780 |  3.800000 |  15.00000 |  11.20000 |   3.0867754 |  11.0211430 | 0.0223141 |
| fbs      |   10 | 3817 |   5.6120278 |  2.3297237 |   5.16000 |   5.2312831 |  1.111950 |  2.500000 |  28.01000 |  25.51000 |   3.2043097 |  15.4918176 | 0.0377089 |
| mogtt2h  |   11 | 3400 |   7.2058441 |  3.1370823 |   6.61000 |   6.7950294 |  2.253552 |  0.170000 |  29.83000 |  29.66000 |   1.8347785 |   5.4475458 | 0.0538005 |
| totchol  |   12 | 3820 |   5.7698115 |  1.2124141 |   5.70000 |   5.7297284 |  1.141602 |  2.130000 |  14.91000 |  12.78000 |   0.5451984 |   1.7449786 | 0.0196164 |
| ftrigliz |   13 | 3820 |   1.5113639 |  0.9954019 |   1.25000 |   1.3415380 |  0.578214 |  0.180000 |   7.99000 |   7.81000 |   2.8742941 |  11.4115916 | 0.0161052 |
| hdl      |   14 | 3820 |   1.3551047 |  0.3555564 |   1.33000 |   1.3355007 |  0.326172 |  0.120000 |   4.43000 |   4.31000 |   0.8313685 |   2.8770872 | 0.0057528 |
| ldl      |   15 | 3820 |   3.5431728 |  1.0842630 |   3.46000 |   3.4993717 |  1.052646 |  0.510000 |   9.42000 |   8.91000 |   0.4828493 |   0.5405287 | 0.0175430 |
| gender   |   16 | 3820 |   0.3463351 |  0.4758638 |   0.00000 |   0.3079188 |  0.000000 |  0.000000 |   1.00000 |   1.00000 |   0.6456666 | \-1.5835289 | 0.0076993 |
| crural   |   17 | 3820 |   0.4761780 |  0.4994976 |   0.00000 |   0.4702225 |  0.000000 |  0.000000 |   1.00000 |   1.00000 |   0.0953588 | \-1.9914278 | 0.0080817 |
| bmi      |   18 | 3820 |  25.6882584 |  4.8231041 |  25.28257 |  25.4625306 |  4.793846 |  9.240941 |  42.37362 |  33.13267 |   0.4286514 | \-0.0334351 | 0.0780360 |

## Data Visualization

Histogram for numerical data

``` r
ggplot(LogDS, aes(age)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$age),
                            sd = sd(LogDS$age)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-1.png)<!-- -->

``` r
ggplot(LogDS, aes(height)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$height),
                            sd = sd(LogDS$height)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-2.png)<!-- -->

``` r
ggplot(LogDS, aes(weight)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$weight),
                            sd = sd(LogDS$weight)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-3.png)<!-- -->

``` r
ggplot(LogDS, aes(hba1c)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$hba1c),
                            sd = sd(LogDS$hba1c)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-4.png)<!-- -->

``` r
ggplot(LogDS, aes(fbs)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$fbs),
                            sd = sd(LogDS$fbs)))
```

    ## Warning: Removed 3 rows containing non-finite values (stat_bin).

    ## Warning: Removed 101 rows containing missing values (geom_path).

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-5.png)<!-- -->

``` r
ggplot(LogDS, aes(mogtt2h)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$mogtt2h),
                            sd = sd(LogDS$mogtt2h)))
```

    ## Warning: Removed 420 rows containing non-finite values (stat_bin).
    
    ## Warning: Removed 101 rows containing missing values (geom_path).

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-6.png)<!-- -->

``` r
ggplot(LogDS, aes(totchol)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$totchol),
                            sd = sd(LogDS$totchol)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-7.png)<!-- -->

``` r
ggplot(LogDS, aes(ftrigliz)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$ftrigliz),
                            sd = sd(LogDS$ftrigliz)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-8.png)<!-- -->

``` r
ggplot(LogDS, aes(hdl)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$hdl),
                            sd = sd(LogDS$hdl)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-9.png)<!-- -->

``` r
ggplot(LogDS, aes(ldl)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(LogDS$ldl),
                            sd = sd(LogDS$ldl)))
```

![](LogRTryLong_files/figure-gfm/Historgram%20numerical-10.png)<!-- -->

Bar chart for categorical data

``` r
ggplot(LogDS, aes(dmdxcat)) +
  geom_bar()
```

![](LogRTryLong_files/figure-gfm/Barchart%20categorical-1.png)<!-- -->

``` r
ggplot(LogDS, aes(gendercat)) +
  geom_bar()
```

![](LogRTryLong_files/figure-gfm/Barchart%20categorical-2.png)<!-- -->

``` r
ggplot(LogDS, aes(cruralcat)) +
  geom_bar()
```

![](LogRTryLong_files/figure-gfm/Barchart%20categorical-3.png)<!-- -->

Correlation Matrix among Numerical Data

``` r
LogDS_Num <- LogDS %>%
  select_if(is.numeric)
Cor_LogDS_Num <- cor(LogDS_Num, use = "complete.obs", method = "pearson")
kable(round(Cor_LogDS_Num,2))
```

|          |    age |   dmdx | height | weight |  waist |    hip |  msbpr |  mdbpr |  hba1c |    fbs | mogtt2h | totchol | ftrigliz |    hdl |    ldl | gender | crural |    bmi |
| -------- | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | ------: | ------: | -------: | -----: | -----: | -----: | -----: | -----: |
| age      |   1.00 |   0.02 | \-0.21 | \-0.07 |   0.14 |   0.01 |   0.46 |   0.23 |   0.18 |   0.20 |    0.23 |    0.32 |     0.13 |   0.16 |   0.26 |   0.02 |   0.04 |   0.04 |
| dmdx     |   0.02 |   1.00 | \-0.01 |   0.03 |   0.02 |   0.02 |   0.03 |   0.03 |   0.12 |   0.10 |    0.11 |    0.01 |     0.00 |   0.00 |   0.00 |   0.00 | \-0.03 |   0.03 |
| height   | \-0.21 | \-0.01 |   1.00 |   0.43 |   0.15 |   0.12 | \-0.06 |   0.00 | \-0.03 | \-0.04 |  \-0.15 |  \-0.07 |     0.07 | \-0.18 | \-0.05 |   0.67 | \-0.09 | \-0.09 |
| weight   | \-0.07 |   0.03 |   0.43 |   1.00 |   0.78 |   0.78 |   0.18 |   0.28 |   0.17 |   0.13 |    0.15 |    0.05 |     0.22 | \-0.23 |   0.12 |   0.27 | \-0.07 |   0.85 |
| waist    |   0.14 |   0.02 |   0.15 |   0.78 |   1.00 |   0.58 |   0.27 |   0.29 |   0.23 |   0.16 |    0.23 |    0.12 |     0.22 | \-0.21 |   0.14 |   0.12 |   0.05 |   0.77 |
| hip      |   0.01 |   0.02 |   0.12 |   0.78 |   0.58 |   1.00 |   0.15 |   0.23 |   0.15 |   0.14 |    0.19 |    0.08 |     0.18 | \-0.13 |   0.16 | \-0.04 | \-0.13 |   0.80 |
| msbpr    |   0.46 |   0.03 | \-0.06 |   0.18 |   0.27 |   0.15 |   1.00 |   0.71 |   0.17 |   0.19 |    0.25 |    0.23 |     0.20 |   0.00 |   0.20 |   0.09 |   0.12 |   0.23 |
| mdbpr    |   0.23 |   0.03 |   0.00 |   0.28 |   0.29 |   0.23 |   0.71 |   1.00 |   0.16 |   0.12 |    0.23 |    0.17 |     0.20 | \-0.07 |   0.16 |   0.08 |   0.05 |   0.31 |
| hba1c    |   0.18 |   0.12 | \-0.03 |   0.17 |   0.23 |   0.15 |   0.17 |   0.16 |   1.00 |   0.47 |    0.54 |    0.17 |     0.18 |   0.01 |   0.16 |   0.00 |   0.04 |   0.20 |
| fbs      |   0.20 |   0.10 | \-0.04 |   0.13 |   0.16 |   0.14 |   0.19 |   0.12 |   0.47 |   1.00 |    0.58 |    0.15 |     0.20 |   0.03 |   0.14 |   0.01 |   0.02 |   0.17 |
| mogtt2h  |   0.23 |   0.11 | \-0.15 |   0.15 |   0.23 |   0.19 |   0.25 |   0.23 |   0.54 |   0.58 |    1.00 |    0.18 |     0.22 |   0.02 |   0.20 | \-0.12 | \-0.02 |   0.26 |
| totchol  |   0.32 |   0.01 | \-0.07 |   0.05 |   0.12 |   0.08 |   0.23 |   0.17 |   0.17 |   0.15 |    0.18 |    1.00 |     0.26 |   0.30 |   0.76 |   0.01 |   0.10 |   0.10 |
| ftrigliz |   0.13 |   0.00 |   0.07 |   0.22 |   0.22 |   0.18 |   0.20 |   0.20 |   0.18 |   0.20 |    0.22 |    0.26 |     1.00 | \-0.20 |   0.15 |   0.13 |   0.09 |   0.20 |
| hdl      |   0.16 |   0.00 | \-0.18 | \-0.23 | \-0.21 | \-0.13 |   0.00 | \-0.07 |   0.01 |   0.03 |    0.02 |    0.30 |   \-0.20 |   1.00 |   0.25 | \-0.24 | \-0.04 | \-0.15 |
| ldl      |   0.26 |   0.00 | \-0.05 |   0.12 |   0.14 |   0.16 |   0.20 |   0.16 |   0.16 |   0.14 |    0.20 |    0.76 |     0.15 |   0.25 |   1.00 |   0.01 |   0.05 |   0.16 |
| gender   |   0.02 |   0.00 |   0.67 |   0.27 |   0.12 | \-0.04 |   0.09 |   0.08 |   0.00 |   0.01 |  \-0.12 |    0.01 |     0.13 | \-0.24 |   0.01 |   1.00 |   0.03 | \-0.09 |
| crural   |   0.04 | \-0.03 | \-0.09 | \-0.07 |   0.05 | \-0.13 |   0.12 |   0.05 |   0.04 |   0.02 |  \-0.02 |    0.10 |     0.09 | \-0.04 |   0.05 |   0.03 |   1.00 | \-0.02 |
| bmi      |   0.04 |   0.03 | \-0.09 |   0.85 |   0.77 |   0.80 |   0.23 |   0.31 |   0.20 |   0.17 |    0.26 |    0.10 |     0.20 | \-0.15 |   0.16 | \-0.09 | \-0.02 |   1.00 |

``` r
corrplot(Cor_LogDS_Num)
```

![](LogRTryLong_files/figure-gfm/Corrplot%20for%20Numerical%20Data-1.png)<!-- -->

# Logistic Regression Modelling

## Univariable - Simple Logistic Regression

### Model01 - age

Model01

  - outcome: dmstatus
  - predictor - age

<!-- end list -->

``` r
Model01 <- glm(dmdxcat ~ age,
               data = LogDS,
               family = binomial (link = "logit"))
summary(Model01)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1379  -0.5451  -0.4180  -0.2955   2.6637  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.642893   0.219626  -21.14   <2e-16 ***
    ## age          0.051116   0.003863   13.23   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2797.7  on 3819  degrees of freedom
    ## Residual deviance: 2602.0  on 3818  degrees of freedom
    ## AIC: 2606
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
kable(tidy(Model01))
```

| term        |    estimate | std.error |  statistic | p.value |
| :---------- | ----------: | --------: | ---------: | ------: |
| (Intercept) | \-4.6428926 | 0.2196262 | \-21.13998 |       0 |
| age         |   0.0511159 | 0.0038629 |   13.23237 |       0 |

### Model02 - Gender

Model02

  - outcome: dm status
  - predictor - gender

<!-- end list -->

``` r
Model02 <- glm(dmdxcat ~ gender,
               data = LogDS,
               family = binomial (link = "logit"))
summary(Model02)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ gender, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5392  -0.5392  -0.4859  -0.4859   2.0953  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.07719    0.06362 -32.650   <2e-16 ***
    ## gender       0.22229    0.10251   2.168   0.0301 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2797.7  on 3819  degrees of freedom
    ## Residual deviance: 2793.1  on 3818  degrees of freedom
    ## AIC: 2797.1
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
kable(tidy(Model02))
```

| term        |    estimate | std.error |   statistic |   p.value |
| :---------- | ----------: | --------: | ----------: | --------: |
| (Intercept) | \-2.0771908 | 0.0636206 | \-32.649639 | 0.0000000 |
| gender      |   0.2222904 | 0.1025100 |    2.168476 | 0.0301225 |

## Multivariable - Multiple Logistic Regression

### Model11 - age & gender

Model11

  - outcome: dm
  - predictor: age, gender

<!-- end list -->

``` r
Model11 <- glm(dmdxcat ~ age + gendercat,
               family = binomial(link = "logit"),
               data = LogDS)
summary(Model11)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + gendercat, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1116  -0.5453  -0.4190  -0.2970   2.6278  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -4.671495   0.220615 -21.175   <2e-16 ***
    ## age            0.050726   0.003867  13.117   <2e-16 ***
    ## gendercatmale  0.135135   0.105856   1.277    0.202    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2797.7  on 3819  degrees of freedom
    ## Residual deviance: 2600.4  on 3817  degrees of freedom
    ## AIC: 2606.4
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
kable(tidy(Model11))
```

| term          |    estimate | std.error |   statistic |   p.value |
| :------------ | ----------: | --------: | ----------: | --------: |
| (Intercept)   | \-4.6714946 | 0.2206152 | \-21.174850 | 0.0000000 |
| age           |   0.0507257 | 0.0038672 |   13.116915 | 0.0000000 |
| gendercatmale |   0.1351354 | 0.1058564 |    1.276592 | 0.2017465 |

Calculate OR and 95% CI

``` r
Table_Model11 <- tidy(Model11)
OR_Model11 <- exp(coefficients(Model11))
ORCI_Model11 <- exp(confint(Model11))
kable(cbind(Table_Model11 [, 1:5], OR_Model11, ORCI_Model11))
```

|               | term          |    estimate | std.error |   statistic |   p.value | OR\_Model11 |     2.5 % |    97.5 % |
| ------------- | :------------ | ----------: | --------: | ----------: | --------: | ----------: | --------: | --------: |
| (Intercept)   | (Intercept)   | \-4.6714946 | 0.2206152 | \-21.174850 | 0.0000000 |   0.0093583 | 0.0060275 | 0.0143167 |
| age           | age           |   0.0507257 | 0.0038672 |   13.116915 | 0.0000000 |   1.0520343 | 1.0441696 | 1.0601249 |
| gendercatmale | gendercatmale |   0.1351354 | 0.1058564 |    1.276592 | 0.2017465 |   1.1446918 | 0.9290441 | 1.4071688 |

## Inference - MLE method (Wald Test)

### Wald Test for Age

\(W = \beta/SE_\beta\)

Example 1, calculate Wald test for Model01 (age)

``` r
0.0511159/0.0038629
```

    ## [1] 13.23252

p-value for above.

``` r
2*pnorm(-abs(0.0511159/0.0038629))
```

    ## [1] 5.694585e-40

### Wald Test for Gender

Wald Test for Model02 (gender)

``` r
0.2222904/0.1025100
```

    ## [1] 2.168475

p-value for above

``` r
2*pnorm(-abs(0.2222904/0.1025100))
```

    ## [1] 0.03012254

## Inference - SE method (Confident Interval)

### CI for Age

CI for Model01

``` r
kable(tidy(confint(Model01)))
```

    ## Waiting for profiling to be done...

    ## Warning: 'tidy.matrix' is deprecated.
    ## See help("Deprecated")

| .rownames   |      X2.5.. |     X97.5.. |
| :---------- | ----------: | ----------: |
| (Intercept) | \-5.0809899 | \-4.2197791 |
| age         |   0.0436191 |   0.0587673 |

the confident interval didn’t cross 1

## Inference - Likelihood Ratio Test

### LR Test fot Model01 with Model11

  - Model01 - age as covariate
  - Model11 - age and gender as covariate

to explore LR when gender is added to Model01

``` r
anova(Model11, Model01, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: dmdxcat ~ age + gendercat
    ## Model 2: dmdxcat ~ age
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      3817     2600.4                     
    ## 2      3818     2602.0 -1  -1.6174   0.2035

### LR Test fot Model02 with Model11

  - Model02 - gender as covariate
  - Model11 - age and gender as covariate

to explore LR when age is added to Mofrl02

``` r
anova(Model11, Model02, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: dmdxcat ~ age + gendercat
    ## Model 2: dmdxcat ~ gender
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1      3817     2600.4                          
    ## 2      3818     2793.1 -1  -192.68 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model Selection

### Explore Univariable

``` r
SLogMod_AgeBPSugar <- LogDS %>%
  dplyr::select(age, msbpr, mdbpr, fbs, mogtt2h, hdl) %>%
  purrr::map(~glm(dmdxcat ~ .x,
                  family = binomial,
                  data = LogDS)) %>%
  purrr::map(tidy) %>%
  bind_rows()
ResultSLogMod01 <- SLogMod_AgeBPSugar %>%
  mutate(model = c("b0", "age",
                   "b0", "msbpr",
                   "b0", "mdbpr",
                   "b0", "fbs",
                   "b0", "mogtt2h",
                   "b0", "hdl")) %>%
  dplyr::select(model, everything())
kable(ResultSLogMod01)
```

| model   | term        |    estimate | std.error |   statistic |  p.value |
| :------ | :---------- | ----------: | --------: | ----------: | -------: |
| b0      | (Intercept) | \-4.6428926 | 0.2196262 | \-21.139979 | 0.000000 |
| age     | .x          |   0.0511159 | 0.0038629 |   13.232369 | 0.000000 |
| b0      | (Intercept) | \-4.6855839 | 0.3023582 | \-15.496800 | 0.000000 |
| msbpr   | .x          |   0.0197130 | 0.0021274 |    9.266430 | 0.000000 |
| b0      | (Intercept) | \-4.6032505 | 0.3579917 | \-12.858541 | 0.000000 |
| mdbpr   | .x          |   0.0327641 | 0.0043676 |    7.501674 | 0.000000 |
| b0      | (Intercept) | \-5.9050029 | 0.1967860 | \-30.007224 | 0.000000 |
| fbs     | .x          |   0.6167881 | 0.0287735 |   21.435952 | 0.000000 |
| b0      | (Intercept) | \-6.0966685 | 0.3579614 | \-17.031635 | 0.000000 |
| mogtt2h | .x          |   0.1861566 | 0.0306516 |    6.073305 | 0.000000 |
| b0      | (Intercept) | \-1.4444420 | 0.1995225 |  \-7.239495 | 0.000000 |
| hdl     | .x          | \-0.4128648 | 0.1467045 |  \-2.814261 | 0.004889 |

### Explore Multicollinearity & Confounder

Correlation Matrix

Correlation Matrix among msbp, dbp, fbs & mogtt2h

``` r
LogDS_BPSugar <- LogDS %>%
  dplyr::select("msbpr", "mdbpr", "fbs", "mogtt2h")
Cor_LogDS_BPSugar <- cor(LogDS_BPSugar, use = "complete.obs", method = "pearson")
kable(round(Cor_LogDS_BPSugar,2))
```

|         | msbpr | mdbpr |  fbs | mogtt2h |
| ------- | ----: | ----: | ---: | ------: |
| msbpr   |  1.00 |  0.71 | 0.19 |    0.25 |
| mdbpr   |  0.71 |  1.00 | 0.12 |    0.23 |
| fbs     |  0.19 |  0.12 | 1.00 |    0.58 |
| mogtt2h |  0.25 |  0.23 | 0.58 |    1.00 |

``` r
corrplot(Cor_LogDS_BPSugar)
```

![](LogRTryLong_files/figure-gfm/Corrplot%20for%20BP%20and%20Sugar-1.png)<!-- -->

``` r
pairs(LogDS_BPSugar)
```

![](LogRTryLong_files/figure-gfm/Corrplot%20for%20BP%20and%20Sugar-2.png)<!-- -->

### Model21 - age, msbp, fbs, hdl

``` r
Model21 <- glm(dmdxcat ~ age + msbpr + fbs + hdl,
               family = binomial(link = "logit"),
               data = LogDS)
summary(Model21)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + msbpr + fbs + hdl, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4471  -0.4072  -0.2674  -0.1545   3.2182  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.663638   0.497228 -15.413  < 2e-16 ***
    ## age          0.055469   0.005431  10.214  < 2e-16 ***
    ## msbpr       -0.001279   0.002995  -0.427 0.669400    
    ## fbs          0.603557   0.029528  20.440  < 2e-16 ***
    ## hdl         -0.705870   0.186668  -3.781 0.000156 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1829.6  on 3812  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1839.6
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model21))
```

| term        |    estimate | std.error |    statistic |   p.value |
| :---------- | ----------: | --------: | -----------: | --------: |
| (Intercept) | \-7.6636378 | 0.4972279 | \-15.4127279 | 0.0000000 |
| age         |   0.0554692 | 0.0054306 |   10.2142573 | 0.0000000 |
| msbpr       | \-0.0012788 | 0.0029950 |  \-0.4269716 | 0.6694000 |
| fbs         |   0.6035566 | 0.0295279 |   20.4401809 | 0.0000000 |
| hdl         | \-0.7058703 | 0.1866676 |  \-3.7814292 | 0.0001559 |

sbp not significant

### Model22 - age, dbp, fbs, hdl

``` r
Model22 <- glm(dmdxcat ~ age + mdbpr + fbs + hdl,
               family = binomial(link = "logit"),
               data = LogDS)
summary(Model22)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + hdl, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4274  -0.4061  -0.2645  -0.1506   3.2026  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -8.619209   0.600377 -14.356  < 2e-16 ***
    ## age          0.053878   0.005010  10.755  < 2e-16 ***
    ## mdbpr        0.011081   0.005571   1.989 0.046703 *  
    ## fbs          0.596706   0.029305  20.362  < 2e-16 ***
    ## hdl         -0.690474   0.187613  -3.680 0.000233 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1825.8  on 3812  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1835.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model22))
```

| term        |    estimate | std.error |   statistic |   p.value |
| :---------- | ----------: | --------: | ----------: | --------: |
| (Intercept) | \-8.6192088 | 0.6003768 | \-14.356332 | 0.0000000 |
| age         |   0.0538784 | 0.0050098 |   10.754661 | 0.0000000 |
| mdbpr       |   0.0110806 | 0.0055710 |    1.988982 | 0.0467032 |
| fbs         |   0.5967062 | 0.0293053 |   20.361708 | 0.0000000 |
| hdl         | \-0.6904737 | 0.1876130 |  \-3.680309 | 0.0002330 |

dbp significant

### Model32 - age, dbp, fbs, hdl, mogtt

``` r
Model32 <- glm(dmdxcat ~ age + mdbpr + fbs + hdl + mogtt2h,
               family = binomial(link = "logit"),
               data = LogDS)
summary(Model32)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + hdl + mogtt2h, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6486  -0.1457  -0.1234  -0.1070   3.3437  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.368847   1.474097  -4.999 5.77e-07 ***
    ## age          0.001666   0.012853   0.130  0.89685    
    ## mdbpr        0.013174   0.015518   0.849  0.39591    
    ## fbs          0.120251   0.107406   1.120  0.26289    
    ## hdl         -0.070759   0.481250  -0.147  0.88311    
    ## mogtt2h      0.135505   0.051215   2.646  0.00815 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 408.12  on 3399  degrees of freedom
    ## Residual deviance: 378.72  on 3394  degrees of freedom
    ##   (420 observations deleted due to missingness)
    ## AIC: 390.72
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
kable(tidy(Model32))
```

| term        |    estimate | std.error |   statistic |   p.value |
| :---------- | ----------: | --------: | ----------: | --------: |
| (Intercept) | \-7.3688466 | 1.4740969 | \-4.9988888 | 0.0000006 |
| age         |   0.0016663 | 0.0128529 |   0.1296413 | 0.8968502 |
| mdbpr       |   0.0131740 | 0.0155180 |   0.8489524 | 0.3959078 |
| fbs         |   0.1202508 | 0.1074062 |   1.1195888 | 0.2628890 |
| hdl         | \-0.0707591 | 0.4812499 | \-0.1470319 | 0.8831068 |
| mogtt2h     |   0.1355051 | 0.0512154 |   2.6457907 | 0.0081500 |

mogtt as confounder?

#### Confounder fbs

Model22 - age, dbp, fbs, hdl

``` r
Model22_01 <- tidy(Model22)
Model32_01 <- tidy(Model32)
((Model32_01[2,2] - Model22_01[2,2])/Model22_01[2,2])*100
```

    ##    estimate
    ## 1 -96.90735

#### Confounder mogtt

Model23 - age, dbp, mogtt, hdl

``` r
Model23 <- glm(dmdxcat ~ age + mdbpr + mogtt2h + hdl,
               family = binomial(link = "logit"),
               data = LogDS)
Model23_01 <- tidy(Model23)
((Model32_01[2,2]-Model23_01[2,2])/Model23_01[2,2])*100
```

    ##   estimate
    ## 1 137.2233

mogtt has higher changes, thus mogtt selected as compared to fbs

### Model23 - age, dbp, mogtt, hdl

``` r
summary(Model23)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + mogtt2h + hdl, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5926  -0.1457  -0.1222  -0.1056   3.3340  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -6.9682939  1.4141087  -4.928 8.32e-07 ***
    ## age          0.0007024  0.0128165   0.055    0.956    
    ## mdbpr        0.0125339  0.0153684   0.816    0.415    
    ## mogtt2h      0.1793428  0.0324848   5.521 3.37e-08 ***
    ## hdl         -0.0732444  0.4768473  -0.154    0.878    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 408.12  on 3399  degrees of freedom
    ## Residual deviance: 379.92  on 3395  degrees of freedom
    ##   (420 observations deleted due to missingness)
    ## AIC: 389.92
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
kable(tidy(Model23))
```

| term        |    estimate | std.error |   statistic |   p.value |
| :---------- | ----------: | --------: | ----------: | --------: |
| (Intercept) | \-6.9682939 | 1.4141087 | \-4.9276934 | 0.0000008 |
| age         |   0.0007024 | 0.0128165 |   0.0548047 | 0.9562940 |
| mdbpr       |   0.0125339 | 0.0153684 |   0.8155614 | 0.4147511 |
| mogtt2h     |   0.1793428 | 0.0324848 |   5.5208219 | 0.0000000 |
| hdl         | \-0.0732444 | 0.4768473 | \-0.1536014 | 0.8779240 |

### Explore Interaction

Preliminary Main Effect Model - Model41

  - outcome: dm
  - covariate: age, dbp, fbs, gender

Simple Logistic Regression for each covariates

``` r
SLogR_Model41 <- LogDS %>%
  dplyr::select(age, mdbpr, fbs, gendercat) %>%
  purrr::map(~glm(dmdxcat ~ .x,
                  family = binomial,
                  data = LogDS)) %>%
  purrr::map(tidy) %>%  bind_rows()

ResultSLogRMod41 <- SLogR_Model41 %>%
  mutate(model = c("b0", "age",
                   "b0", "mdbpr",
                   "b0", "fbs",
                   "b0", "gendercat")) %>%
  dplyr::select(model, everything())
kable(ResultSLogRMod41)
```

| model     | term        |    estimate | std.error |   statistic |   p.value |
| :-------- | :---------- | ----------: | --------: | ----------: | --------: |
| b0        | (Intercept) | \-4.6428926 | 0.2196262 | \-21.139979 | 0.0000000 |
| age       | .x          |   0.0511159 | 0.0038629 |   13.232369 | 0.0000000 |
| b0        | (Intercept) | \-4.6032505 | 0.3579917 | \-12.858541 | 0.0000000 |
| mdbpr     | .x          |   0.0327641 | 0.0043676 |    7.501674 | 0.0000000 |
| b0        | (Intercept) | \-5.9050029 | 0.1967860 | \-30.007224 | 0.0000000 |
| fbs       | .x          |   0.6167881 | 0.0287735 |   21.435952 | 0.0000000 |
| b0        | (Intercept) | \-2.0771908 | 0.0636206 | \-32.649639 | 0.0000000 |
| gendercat | .xmale      |   0.2222904 | 0.1025100 |    2.168476 | 0.0301225 |

Multiple Logistic Regression

``` r
Model41 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat,
               family = binomial(link = "logit"),
               data = LogDS)
summary(Model41)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat, family = binomial(link = "logit"), 
    ##     data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3504  -0.4095  -0.2679  -0.1535   3.1943  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -9.511547   0.553597 -17.181   <2e-16 ***
    ## age            0.051388   0.004976  10.326   <2e-16 ***
    ## mdbpr          0.011534   0.005575   2.069   0.0385 *  
    ## fbs            0.599771   0.029351  20.435   <2e-16 ***
    ## gendercatmale  0.124927   0.128424   0.973   0.3307    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785  on 3816  degrees of freedom
    ## Residual deviance: 1839  on 3812  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1849
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41))
```

| term          |    estimate | std.error |    statistic |   p.value |
| :------------ | ----------: | --------: | -----------: | --------: |
| (Intercept)   | \-9.5115475 | 0.5535974 | \-17.1813455 | 0.0000000 |
| age           |   0.0513880 | 0.0049764 |   10.3263273 | 0.0000000 |
| mdbpr         |   0.0115341 | 0.0055746 |    2.0690408 | 0.0385423 |
| fbs           |   0.5997708 | 0.0293506 |   20.4346895 | 0.0000000 |
| gendercatmale |   0.1249275 | 0.1284237 |    0.9727761 | 0.3306646 |

#### Interaction 1 - age:dbp

Interaction 1 - age:dbp

  - covariate: age, dbp, fbs, gender,
age:dbp

<!-- end list -->

``` r
Model41_Int01 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + age:mdbpr,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int01)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + age:mdbpr, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3546  -0.4128  -0.2684  -0.1461   3.2630  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -1.244e+01  2.040e+00  -6.096 1.09e-09 ***
    ## age            1.032e-01  3.492e-02   2.955  0.00313 ** 
    ## mdbpr          4.895e-02  2.554e-02   1.917  0.05526 .  
    ## fbs            5.972e-01  2.937e-02  20.330  < 2e-16 ***
    ## gendercatmale  1.276e-01  1.283e-01   0.994  0.32005    
    ## age:mdbpr     -6.584e-04  4.381e-04  -1.503  0.13290    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1836.7  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1848.7
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int01))
```

| term          |     estimate | std.error |   statistic |   p.value |
| :------------ | -----------: | --------: | ----------: | --------: |
| (Intercept)   | \-12.4373230 | 2.0403511 | \-6.0956778 | 0.0000000 |
| age           |    0.1031804 | 0.0349176 |   2.9549650 | 0.0031270 |
| mdbpr         |    0.0489506 | 0.0255372 |   1.9168340 | 0.0552590 |
| fbs           |    0.5971793 | 0.0293737 |  20.3304355 | 0.0000000 |
| gendercatmale |    0.1276150 | 0.1283404 |   0.9943474 | 0.3200537 |
| age:mdbpr     |  \-0.0006584 | 0.0004381 | \-1.5027573 | 0.1329016 |

interaction term not significant

#### Interaction 2 - age:fbs

  - covariate: age, dbp, fbs, gender, age:fbs

<!-- end list -->

``` r
Model41_Int02 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + age:fbs,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int02)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + age:fbs, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5360  -0.4041  -0.2701  -0.1645   3.1098  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -8.163191   0.945998  -8.629  < 2e-16 ***
    ## age            0.024719   0.016232   1.523  0.12780    
    ## mdbpr          0.011818   0.005607   2.108  0.03505 *  
    ## fbs            0.384076   0.127715   3.007  0.00264 ** 
    ## gendercatmale  0.126805   0.128989   0.983  0.32557    
    ## age:fbs        0.004212   0.002454   1.716  0.08610 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785  on 3816  degrees of freedom
    ## Residual deviance: 1836  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1848
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int02))
```

| term          |    estimate | std.error |   statistic |   p.value |
| :------------ | ----------: | --------: | ----------: | --------: |
| (Intercept)   | \-8.1631911 | 0.9459984 | \-8.6291806 | 0.0000000 |
| age           |   0.0247191 | 0.0162323 |   1.5228274 | 0.1278019 |
| mdbpr         |   0.0118180 | 0.0056069 |   2.1077419 | 0.0350533 |
| fbs           |   0.3840762 | 0.1277148 |   3.0072953 | 0.0026358 |
| gendercatmale |   0.1268052 | 0.1289889 |   0.9830704 | 0.3255728 |
| age:fbs       |   0.0042125 | 0.0024543 |   1.7163494 | 0.0860981 |

interaction term not significant

#### Interaction 3 - age:gender

  - covariate: age, dbp, fbs, gender,
age:gender

<!-- end list -->

``` r
Model41_Int03 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + age:gendercat,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int03)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + age:gendercat, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3304  -0.4095  -0.2691  -0.1553   3.1646  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -9.275552   0.590353 -15.712  < 2e-16 ***
    ## age                0.047064   0.006329   7.436 1.03e-13 ***
    ## mdbpr              0.011530   0.005575   2.068   0.0386 *  
    ## fbs                0.600403   0.029394  20.426  < 2e-16 ***
    ## gendercatmale     -0.503082   0.593447  -0.848   0.3966    
    ## age:gendercatmale  0.011055   0.010172   1.087   0.2771    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1837.8  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1849.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int03))
```

| term              |    estimate | std.error |    statistic |   p.value |
| :---------------- | ----------: | --------: | -----------: | --------: |
| (Intercept)       | \-9.2755522 | 0.5903529 | \-15.7118782 | 0.0000000 |
| age               |   0.0470645 | 0.0063289 |    7.4364281 | 0.0000000 |
| mdbpr             |   0.0115304 | 0.0055748 |    2.0683059 | 0.0386113 |
| fbs               |   0.6004027 | 0.0293936 |   20.4263148 | 0.0000000 |
| gendercatmale     | \-0.5030816 | 0.5934467 |  \-0.8477284 | 0.3965893 |
| age:gendercatmale |   0.0110547 | 0.0101721 |    1.0867617 | 0.2771421 |

interaction term not significant

#### Interaction 4 - dbp:fbs

  - covariate: age, dbp, fbs, gender,
dbp:fbs

<!-- end list -->

``` r
Model41_Int04 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + mdbpr:fbs,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int04)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + mdbpr:fbs, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3896  -0.4112  -0.2647  -0.1479   3.2540  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -12.368721   1.477816  -8.370  < 2e-16 ***
    ## age             0.050971   0.005005  10.184  < 2e-16 ***
    ## mdbpr           0.046868   0.017622   2.660  0.00782 ** 
    ## fbs             1.042960   0.212973   4.897 9.72e-07 ***
    ## gendercatmale   0.124860   0.128408   0.972  0.33086    
    ## mdbpr:fbs      -0.005422   0.002555  -2.122  0.03385 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1834.7  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1846.7
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int04))
```

| term          |     estimate | std.error |   statistic |   p.value |
| :------------ | -----------: | --------: | ----------: | --------: |
| (Intercept)   | \-12.3687211 | 1.4778161 | \-8.3695942 | 0.0000000 |
| age           |    0.0509707 | 0.0050051 |  10.1837891 | 0.0000000 |
| mdbpr         |    0.0468675 | 0.0176223 |   2.6595618 | 0.0078242 |
| fbs           |    1.0429603 | 0.2129725 |   4.8971584 | 0.0000010 |
| gendercatmale |    0.1248603 | 0.1284075 |   0.9723747 | 0.3308642 |
| mdbpr:fbs     |  \-0.0054223 | 0.0025554 | \-2.1218869 | 0.0338472 |

interaction term is significant

#### Interaction 5 - dbp:gender

  - covariate: age, dbp, fbs, gender,
dbp:gender

<!-- end list -->

``` r
Model41_Int05 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + mdbpr:gendercat,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int05)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + mdbpr:gendercat, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3494  -0.4102  -0.2685  -0.1522   3.1988  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -9.699005   0.673180 -14.408   <2e-16 ***
    ## age                  0.051396   0.004976  10.328   <2e-16 ***
    ## mdbpr                0.013884   0.007328   1.895   0.0582 .  
    ## fbs                  0.599665   0.029369  20.418   <2e-16 ***
    ## gendercatmale        0.575005   0.919842   0.625   0.5319    
    ## mdbpr:gendercatmale -0.005547   0.011230  -0.494   0.6214    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1838.8  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1850.8
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int05))
```

| term                |    estimate | std.error |    statistic |   p.value |
| :------------------ | ----------: | --------: | -----------: | --------: |
| (Intercept)         | \-9.6990046 | 0.6731799 | \-14.4077452 | 0.0000000 |
| age                 |   0.0513963 | 0.0049762 |   10.3283229 | 0.0000000 |
| mdbpr               |   0.0138838 | 0.0073284 |    1.8945160 | 0.0581566 |
| fbs                 |   0.5996649 | 0.0293695 |   20.4179662 | 0.0000000 |
| gendercatmale       |   0.5750053 | 0.9198417 |    0.6251133 | 0.5318967 |
| mdbpr:gendercatmale | \-0.0055468 | 0.0112297 |  \-0.4939372 | 0.6213505 |

interaction is not significant

#### Interaction 6 - fbs:gender

  - covariate: age, dbp, fbs, gender,
fbs:gender

<!-- end list -->

``` r
Model41_Int06 <- glm(dmdxcat ~ age + mdbpr + fbs + gendercat + fbs:gendercat,
                     family = binomial(link = "logit"),
                     data = LogDS)
summary(Model41_Int06)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + fbs:gendercat, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3996  -0.4085  -0.2693  -0.1539   3.1604  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -9.298615   0.566502 -16.414   <2e-16 ***
    ## age                0.051583   0.004995  10.326   <2e-16 ***
    ## mdbpr              0.011561   0.005595   2.066   0.0388 *  
    ## fbs                0.565803   0.034819  16.250   <2e-16 ***
    ## gendercatmale     -0.562929   0.436238  -1.290   0.1969    
    ## fbs:gendercatmale  0.104452   0.063241   1.652   0.0986 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1836.2  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1848.2
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(Model41_Int06))
```

| term              |    estimate | std.error |   statistic |   p.value |
| :---------------- | ----------: | --------: | ----------: | --------: |
| (Intercept)       | \-9.2986148 | 0.5665017 | \-16.414098 | 0.0000000 |
| age               |   0.0515833 | 0.0049954 |   10.326244 | 0.0000000 |
| mdbpr             |   0.0115613 | 0.0055951 |    2.066315 | 0.0387987 |
| fbs               |   0.5658035 | 0.0348186 |   16.250025 | 0.0000000 |
| gendercatmale     | \-0.5629290 | 0.4362385 |  \-1.290416 | 0.1969064 |
| fbs:gendercatmale |   0.1044516 | 0.0632407 |    1.651651 | 0.0986057 |

no interaction

### Prelim Final Model

Preliminary Final Model

  - outcome: dm
  - predictor: age, dbp, fbs, gender, dbp:fbs

<!-- end list -->

``` r
PrelimFInalModel <- Model41_Int04
summary(PrelimFInalModel)
```

    ## 
    ## Call:
    ## glm(formula = dmdxcat ~ age + mdbpr + fbs + gendercat + mdbpr:fbs, 
    ##     family = binomial(link = "logit"), data = LogDS)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3896  -0.4112  -0.2647  -0.1479   3.2540  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -12.368721   1.477816  -8.370  < 2e-16 ***
    ## age             0.050971   0.005005  10.184  < 2e-16 ***
    ## mdbpr           0.046868   0.017622   2.660  0.00782 ** 
    ## fbs             1.042960   0.212973   4.897 9.72e-07 ***
    ## gendercatmale   0.124860   0.128408   0.972  0.33086    
    ## mdbpr:fbs      -0.005422   0.002555  -2.122  0.03385 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2785.0  on 3816  degrees of freedom
    ## Residual deviance: 1834.7  on 3811  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 1846.7
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
kable(tidy(PrelimFInalModel))
```

| term          |     estimate | std.error |   statistic |   p.value |
| :------------ | -----------: | --------: | ----------: | --------: |
| (Intercept)   | \-12.3687211 | 1.4778161 | \-8.3695942 | 0.0000000 |
| age           |    0.0509707 | 0.0050051 |  10.1837891 | 0.0000000 |
| mdbpr         |    0.0468675 | 0.0176223 |   2.6595618 | 0.0078242 |
| fbs           |    1.0429603 | 0.2129725 |   4.8971584 | 0.0000010 |
| gendercatmale |    0.1248603 | 0.1284075 |   0.9723747 | 0.3308642 |
| mdbpr:fbs     |  \-0.0054223 | 0.0025554 | \-2.1218869 | 0.0338472 |

## Model Checking

### Hosmer Lemeshow Test

``` r
#logitgof(LogDS$dmdxcat, fitted(PrelimFInalModel), g = 10)
#hoslem.test(LogDS$dmdxcat, fitted(PrelimFInalModel), g = 10)
HLT_PrelimModel <- dx(PrelimFInalModel, byCov = T)
kable(tail(HLT_PrelimModel))
```

| (Intercept) | age | mdbpr |   fbs | gendercatmale | mdbpr:fbs | y |         P | n |      yhat |         Pr |         dr |         h |        sPr |        sdr |     dChisq |      dDev |     dBhat |
| ----------: | --: | ----: | ----: | ------------: | --------: | -: | --------: | -: | --------: | ---------: | ---------: | --------: | ---------: | ---------: | ---------: | --------: | --------: |
|           1 |  27 |  66.0 |  3.81 |             0 |   251.460 | 1 | 0.0050211 | 1 | 0.0050211 |  14.076979 |   3.253956 | 0.0002635 |  14.078834 |   3.254385 | 198.213573 | 10.591021 | 0.0522447 |
|           1 |  38 |  63.0 |  3.40 |             0 |   214.200 | 1 | 0.0060925 | 1 | 0.0060925 |  12.772469 |   3.193961 | 0.0003349 |  12.774608 |   3.194497 | 163.190607 | 10.204808 | 0.0546692 |
|           1 |  51 |  75.0 | 13.59 |             0 |  1019.250 | 0 | 0.9162880 | 1 | 0.9162880 | \-3.308431 | \-2.227273 | 0.0051632 | \-3.317006 | \-2.233045 |  11.002526 |  4.986492 | 0.0571034 |
|           1 |  43 |  87.5 | 14.77 |             0 |  1292.375 | 0 | 0.9105803 | 1 | 0.9105803 | \-3.191116 | \-2.197460 | 0.0058937 | \-3.200561 | \-2.203964 |  10.243593 |  4.857457 | 0.0607308 |
|           1 |  35 |  67.5 | 12.91 |             0 |   871.425 | 0 | 0.7888916 | 1 | 0.7888916 | \-1.933107 | \-1.763737 | 0.0190576 | \-1.951795 | \-1.780787 |   3.809503 |  3.171203 | 0.0740105 |
|           1 |  39 |  95.0 | 14.92 |             0 |  1417.400 | 0 | 0.8751060 | 1 | 0.8751060 | \-2.647035 | \-2.039750 | 0.0151282 | \-2.667287 | \-2.055356 |   7.114420 |  4.224489 | 0.1092814 |

### Diagnostic Plot

``` r
plot(PrelimFInalModel)
```

### ROC Curve

``` r
#gof_PrelimModel <- gof(PrelimFInalModel, plotROC = T)
#gof_PrelimModel
#gof_PrelimModel$auc
```

## Linearity in Logit
