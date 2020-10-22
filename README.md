# wizirt <img src="man/figures/logo.png" align="right" height = "100"/>

Generally, missing data are handled using full 
information during estimation. Additional functions for item and person fit tend to use 
list wise deletion or pairwise information when possible.  Currently only supports 
unidimensional, parametric, dichotomous models.

The wizirt package (currently named wizirt) is up and running. To
install it, use

    devtools::install_github('Pflegermeister/wizirt')

The wizirt package represents the work of my dissertation. As such I
have agreed to do the following for the first round of development. I
will walk through how to do each using the wizirt2 package. Data for
testing the package can be loaded using

    a <- capture.output(library(wizirt2)) # weird messages when just installed or loaded. xxxx
    data('responses')

-   Two levels of accessibility
    -   Comprehensive report for beginners I didn’t run this here
        because it creates a new Rmarkdown.

<!-- -->

    irt_summarize(responses[,-1], 
                  title = 'My report', 
                  author = 'Pflegermeister', 
                  note = 'This is a note', 
                  item_type = '2PL')

-   Felxible functions for advanced users

The advanced user interface follows the <code>parsnip</code> package
closely.

    my_model <- irt(item_type = '2PL') %>%  
      set_engine('mirt') %>%
      fit_wizirt(data = responses[,-1])
    # need to add:
    # - ltm: 2PL and 3PL
    # - eRm: Rasch
    # - mirt: 1PL, also check if 3PL works
    # xxxx

A lot of people have commented that they would like to see a wrapper
function for this so they don’t have to specify this on multiple lines.
That will come in the future.

-   Provide general model information including:
    -   Model call
    -   Description of model run
    -   Software and version
    -   Estimator
    -   Convergence status, criteria, values
    -   Estimation issues (not included? Maybe in console
        warnings, xxxx)

<!-- -->

    # Did error get fixed? Does it run now? #reinstall should fix
    # Ok, so adding usemethod('print' did not do what I wanted it to do.)
    wizirt2:::print.irt(my_model, type = 'tech')

-   Response data described (sample size, demographics)
-   Descriptions of items and test

<!-- -->

    wizirt2:::print.irt(my_model, type = 'desc')

    ## $spec
    ## $args
    ## $args$item_type
    ## <quosure>
    ## expr: ^"2PL"
    ## env:  empty
    ## 
    ## $args$irt_pars
    ## <quosure>
    ## expr: ^TRUE
    ## env:  empty
    ## 
    ## $args$rownames
    ## <quosure>
    ## expr: ^NULL
    ## env:  empty
    ## 
    ## $args$tol
    ## <quosure>
    ## expr: ^1e-05
    ## env:  empty
    ## 
    ## 
    ## $eng_args
    ## <list_of<quosure>>
    ## 
    ## named list()
    ## 
    ## $mode
    ## [1] "regression"
    ## 
    ## $method
    ## $method$libs
    ## [1] "mirt"
    ## 
    ## $method$fit
    ## $method$fit$interface
    ## [1] "matrix"
    ## 
    ## $method$fit$protect
    ## [1] ""
    ## 
    ## $method$fit$func
    ##        fun 
    ## "irt_mirt" 
    ## 
    ## $method$fit$defaults
    ## list()
    ## 
    ## $method$fit$args
    ## $method$fit$args[[1]]
    ## missing_arg()
    ## 
    ## $method$fit$args$item_type
    ## <quosure>
    ## expr: ^"2PL"
    ## env:  empty
    ## 
    ## $method$fit$args$tol
    ## <quosure>
    ## expr: ^1e-05
    ## env:  empty
    ## 
    ## 
    ## 
    ## $method$pred
    ## named list()
    ## 
    ## 
    ## $engine
    ## [1] "mirt"
    ## 
    ## attr(,"class")
    ## [1] "irt"        "model_spec"
    ## 
    ## $fit
    ## $fit$data
    ## # A tibble: 75 x 25
    ##        A     B     C     D     E     F     G     H     I     J     K     L     M
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1     1     0     1     1     0     1     1     1     1     1     0     1     1
    ##  2     1     1     1     1     1     1     1     0     1     1     1     1     1
    ##  3     0     1     0     1     0     1     1     1     1     0     1     1     1
    ##  4     0     0     0     0     0     0     1     0     0     0     0     1     1
    ##  5     0     1     0     0     0     1     1     1     1     1     1     1     1
    ##  6     0     1     1     1     1     1     1     1     1     1     1     1     1
    ##  7     1     1     1     1     0     1     1     1     1     1     1     1     1
    ##  8     1     1     1     1     1     1     1     0     1     0     0     1     1
    ##  9     0     1     1     0     1     0     1     1     0     1     1     1     1
    ## 10     0     0     0     1     1     1     0     1     1     1     0     1     1
    ## # ... with 65 more rows, and 12 more variables: N <int>, O <int>, P <int>,
    ## #   Q <int>, R <int>, S <int>, T <int>, U <int>, V <int>, W <int>, X <int>,
    ## #   Y <int>
    ## 
    ## $fit$model
    ## $fit$model$engine
    ## $fit$model$engine$pkg
    ## [1] "mirt"
    ## 
    ## $fit$model$engine$ver
    ## [1] '1.32.7'
    ## 
    ## $fit$model$engine$func
    ## [1] "mirt"
    ## 
    ## $fit$model$engine$call
    ## mirt::mirt(data = data, model = 1, itemtype = item_type, SE = T, 
    ##     TOL = tol, verbose = F)
    ## 
    ## 
    ## $fit$model$n_factors
    ## [1] 1
    ## 
    ## $fit$model$item_type
    ## [1] "2PL"
    ## 
    ## 
    ## $fit$estimation
    ## $fit$estimation$convergence
    ## [1] TRUE
    ## 
    ## $fit$estimation$method
    ## [1] "EM"
    ## 
    ## $fit$estimation$criteria
    ## [1] 1e-05
    ## 
    ## $fit$estimation$iterations
    ## [1] 76
    ## 
    ## $fit$estimation$log_lik
    ## [1] -1009.376
    ## 
    ## 
    ## $fit$parameters
    ## $fit$parameters$coefficients
    ## # A tibble: 25 x 4
    ##    item  difficulty discrimination guessing
    ##    <chr>      <dbl>          <dbl>    <dbl>
    ##  1 A         -0.606         1.75          0
    ##  2 B         -1.87          0.580         0
    ##  3 C         -2.26          0.624         0
    ##  4 D         -2.32          0.363         0
    ##  5 E         10.5          -0.0904        0
    ##  6 F         -5.38          0.177         0
    ##  7 G         -1.71          0.952         0
    ##  8 H         -0.996         1.22          0
    ##  9 I         -2.63          0.459         0
    ## 10 J         -2.70          0.416         0
    ## # ... with 15 more rows
    ## 
    ## $fit$parameters$persons
    ## # A tibble: 75 x 3
    ##    ability std_err   ids
    ##      <dbl>   <dbl> <int>
    ##  1 -0.0216   0.533     1
    ##  2  0.662    0.640     2
    ##  3 -0.201    0.510     3
    ##  4 -1.93     0.556     4
    ##  5 -0.735    0.472     5
    ##  6 -0.503    0.482     6
    ##  7  0.874    0.674     7
    ##  8 -0.746    0.472     8
    ##  9 -0.172    0.514     9
    ## 10 -1.05     0.476    10
    ## # ... with 65 more rows
    ## 
    ## 
    ## $fit$original_object
    ## 
    ## Call:
    ## mirt::mirt(data = data, model = 1, itemtype = item_type, SE = T, 
    ##     TOL = tol, verbose = F)
    ## 
    ## Full-information item factor analysis with 1 factor(s).
    ## Converged within 1e-05 tolerance after 76 EM iterations.
    ## mirt version: 1.32.7 
    ## M-step optimizer: BFGS 
    ## EM acceleration: Ramsay 
    ## Number of rectangular quadrature: 61
    ## Latent density type: Gaussian 
    ## 
    ## Information matrix estimated with method: Oakes
    ## Second-order test: model is a possible local maximum
    ## Condition number of information matrix =  32.08619
    ## 
    ## Log-likelihood = -1009.376
    ## Estimated parameters: 50 
    ## AIC = 2118.753; AICc = 2331.253
    ## BIC = 2234.627; SABIC = 2077.04
    ## G2 (33554381) = 1371.13, p = 1
    ## RMSEA = 0, CFI = NaN, TLI = NaN
    ## 
    ## $preproc
    ## $preproc$y_var
    ## character(0)
    ## 
    ## 
    ## $elapsed
    ##    user  system elapsed 
    ##    0.39    0.03    0.44 
    ## 
    ## attr(,"class")
    ## [1] "_list"      "wizirt_fit" "_model_fit"

-   Missing data summary

<!-- -->

    # not yet implemented xxxx
    wizirt2:::print.irt(my_model, type = 'na')

-   Description of missing data handling (not yet implemented, xxxx)

-   Provide model fit information

<!-- -->

    # Needs a verbose argument xxxx
    assumptions <- irt_assume(my_model)

    ## Using pkgs:

    ##   - sirt 3.9.4 for DETECT, ASSI, RATIO and LD covariance

    ##   - ltm 1.1.1 for LD

    ##   - mirt 1.32.7 for relative fit

    ## -----------------------------------------------------------
    ## Confirmatory DETECT Analysis 
    ## Conditioning on 1 Score
    ## Bandwidth Scale: 1.1 
    ## -----------------------------------------------------------
    ##           unweighted weighted
    ## DETECT        -0.222   -0.222
    ## ASSI          -0.167   -0.167
    ## RATIO         -0.140   -0.140
    ## MADCOV100      1.588    1.588
    ## MCOV100       -0.222   -0.222

-   Unidimensionality (DETECT)

<!-- -->

    assumptions$unidim

    ##    DETECT      ASSI     RATIO MADCOV100   MCOV100 
    ##     -0.22     -0.17     -0.14      1.59     -0.22

-   Absolute fit

<!-- -->

    # not yet implemented xxxx
    assumptions$abs_fit

    ## NULL

-   Relative fit

<!-- -->

    # Missing some important ones right now (chi-square based) xxxx
    assumptions$rel_fit

    ## # A tibble: 8 x 2
    ##   stats   values
    ##   <chr>    <dbl>
    ## 1 log_lik -1009.
    ## 2 N          75 
    ## 3 n_pars     25 
    ## 4 AIC      2069.
    ## 5 AICc     2095.
    ## 6 BIC      2127.
    ## 7 SABIC    2048.
    ## 8 HQ       2092.

-   Anova method for comparing models

<!-- -->

    # Not yet implemented xxxx

-   Provide item information
    -   Parameter estimates

<!-- -->

    # Should make this a print option xxxx
    my_model$fit$parameters$coefficients

    ## # A tibble: 25 x 4
    ##    item  difficulty discrimination guessing
    ##    <chr>      <dbl>          <dbl>    <dbl>
    ##  1 A         -0.606         1.75          0
    ##  2 B         -1.87          0.580         0
    ##  3 C         -2.26          0.624         0
    ##  4 D         -2.32          0.363         0
    ##  5 E         10.5          -0.0904        0
    ##  6 F         -5.38          0.177         0
    ##  7 G         -1.71          0.952         0
    ##  8 H         -0.996         1.22          0
    ##  9 I         -2.63          0.459         0
    ## 10 J         -2.70          0.416         0
    ## # ... with 15 more rows

-   Summary statistics

<!-- -->

    # xxxx
    # does the combined one work? Let's give it a good name xxxx
    plot.irt(my_model, type = 'test_info') # I need a plot of the item locations too. xxxx
    # I feel like I creat more information than is showcased here. Make it more obvious. xxxx

-   Provide item-fit information
    -   Conditional dependence

<!-- -->

    # assumptions <- irt_assume(my_model) # not run xxxx
    assumptions$ld

    ## # A tibble: 300 x 5
    ##    item_1 item_2    LD    pvals    ccov
    ##    <chr>  <chr>  <dbl>    <dbl>   <dbl>
    ##  1 A      W      0.405 0.000312 -0.0269
    ##  2 J      P      0.399 0.000398  0.0584
    ##  3 K      W      0.363 0.00139  -0.0150
    ##  4 A      S      0.331 0.00371   0.0393
    ##  5 A      K      0.315 0.00592  -0.0154
    ##  6 O      T      0.309 0.00695   0.0472
    ##  7 D      I      0.303 0.00816   0.0461
    ##  8 H      W      0.295 0.0101   -0.0151
    ##  9 M      Q      0.285 0.0132    0.0495
    ## 10 H      P      0.282 0.0142    0.0239
    ## # ... with 290 more rows

-   Fit statistics

<!-- -->

    ifa <- irt_item_fit(my_model, stats = c('Zh', 'X2', 'G2', 'infit'))

    ## Warning in sqrt(colSums((pf$C/pf$W^2)/N^2) - 1/N): NaNs produced

    ## Warning in sqrt(colSums(pf$C - pf$W^2)/colSums(pf$W)^2): NaNs produced

    wizirt2:::print.ifa(ifa) # export it so wizirt2::: is not needed.

    ## # A tibble: 25 x 17
    ##    item  difficulty discrimination guessing       Zh  outfit z.outfit   infit
    ##    <chr>      <dbl>          <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 A         -0.606         1.75          0 -1.36e+1 5.21e+0     9.15 2.42e+0
    ##  2 B         -1.87          0.580         0 -2.30e+1 7.32e+0    10.5  6.20e+0
    ##  3 C         -2.26          0.624         0 -2.78e+1 1.10e+1    10.6  8.89e+0
    ##  4 D         -2.32          0.363         0 -2.79e+1 1.06e+1    10.7  9.86e+0
    ##  5 E         10.5          -0.0904        0  4.62e-2 2.58e-7   -18.9  2.58e-7
    ##  6 F         -5.38          0.177         0 -1.27e+2 2.19e+2    11.1  2.15e+2
    ##  7 G         -1.71          0.952         0 -2.29e+1 7.60e+0    10.5  5.02e+0
    ##  8 H         -0.996         1.22          0 -1.68e+1 4.55e+0     9.74 2.81e+0
    ##  9 I         -2.63          0.459         0 -3.26e+1 1.49e+1    10.7  1.32e+1
    ## 10 J         -2.70          0.416         0 -3.37e+1 1.58e+1    10.7  1.43e+1
    ## # ... with 15 more rows, and 9 more variables: z.infit <dbl>, X2 <dbl>,
    ## #   df.X2 <dbl>, RMSEA.X2 <dbl>, p.X2 <dbl>, G2 <dbl>, df.G2 <dbl>,
    ## #   RMSEA.G2 <dbl>, p.G2 <dbl>

-   predicted vs observed item response functions

<!-- -->

    plot.irt(my_model, type = 'resid', items = 1:3, facets = T)
    plot.irt(my_model, type = 'stand', items = 1) # Is this plot correct? It looks goofy. xxxx 
    plot.irt(my_model, type = 'resid_trace', items = 1) # The dots aren't on the same side of the line in stand as they are in this plot xxxx

-   Functional form

<!-- -->

    # I thought this was model-data fit? What do I do for this different from what I have done? xxxx

-   misfitting items flagged

<!-- -->

    # not yet implemented xxxx

-   Provide person information
    -   Person location estimates (including SE)

<!-- -->

    my_model$fit$parameters$persons
    plot.irt(my_model, type = 'theta') # Maybe add SE to plot as a line, we have SE xxxx

-   Provide person-fit information

<!-- -->

    pfa <- irt_person_fit(my_model, stats = c('Ht', 'lzstar', 'U3'))

-   Global detection (Ht, lz\*, infit and outfit, with cutoffs where
    applicable)

<!-- -->

    wizirt2:::print.pfa(pfa) # export it so wizirt2::: is not needed.

    ## # A tibble: 75 x 9
    ##    ability std_err   ids      Ht  Ht_cut   lzstar lzstar_cut    U3 U3_cut
    ##      <dbl>   <dbl> <int>   <dbl>   <dbl>    <dbl>      <dbl> <dbl>  <dbl>
    ##  1 -0.0216   0.533     1  0.0044 -0.0433   -0.133      -2.10 0.374  0.697
    ##  2  0.662    0.640     2  0.0174 -0.0433  -16.9        -2.10 0.386  0.697
    ##  3 -0.201    0.510     3  0.004  -0.0433   -6.89       -2.10 0.396  0.697
    ##  4 -1.93     0.556     4  0.0657 -0.0433 -511.         -2.10 0.208  0.697
    ##  5 -0.735    0.472     5  0.004  -0.0433  -72.1        -2.10 0.383  0.697
    ##  6 -0.503    0.482     6  0.0021 -0.0433  -29.9        -2.10 0.427  0.697
    ##  7  0.874    0.674     7  0.0125 -0.0433  NaN          -2.10 0.373  0.697
    ##  8 -0.746    0.472     8 -0.0159 -0.0433  -81.2        -2.10 0.486  0.697
    ##  9 -0.172    0.514     9  0.0274 -0.0433   -3.13       -2.10 0.286  0.697
    ## 10 -1.05     0.476    10  0.027  -0.0433 -129.         -2.10 0.285  0.697
    ## # ... with 65 more rows

-   Local detection (ICI or other)

<!-- -->

    # not yet implemented xxxx

-   Tabulation or presentation of response patterns (winsteps tables as
    guides)

<!-- -->

    wizirt2:::print.pfa(pfa, patterns = T) # get rid of '_' in patterns xxxx

    ## # A tibble: 75 x 10
    ##    ability std_err   ids      Ht  Ht_cut   lzstar lzstar_cut    U3 U3_cut
    ##      <dbl>   <dbl> <int>   <dbl>   <dbl>    <dbl>      <dbl> <dbl>  <dbl>
    ##  1 -0.0216   0.533     1  0.0044 -0.0433   -0.133      -2.10 0.374  0.697
    ##  2  0.662    0.640     2  0.0174 -0.0433  -16.9        -2.10 0.386  0.697
    ##  3 -0.201    0.510     3  0.004  -0.0433   -6.89       -2.10 0.396  0.697
    ##  4 -1.93     0.556     4  0.0657 -0.0433 -511.         -2.10 0.208  0.697
    ##  5 -0.735    0.472     5  0.004  -0.0433  -72.1        -2.10 0.383  0.697
    ##  6 -0.503    0.482     6  0.0021 -0.0433  -29.9        -2.10 0.427  0.697
    ##  7  0.874    0.674     7  0.0125 -0.0433  NaN          -2.10 0.373  0.697
    ##  8 -0.746    0.472     8 -0.0159 -0.0433  -81.2        -2.10 0.486  0.697
    ##  9 -0.172    0.514     9  0.0274 -0.0433   -3.13       -2.10 0.286  0.697
    ## 10 -1.05     0.476    10  0.027  -0.0433 -129.         -2.10 0.285  0.697
    ## # ... with 65 more rows, and 1 more variable: pattern <chr>

    # more using winsteps as guide xxxx
    # export it so wizirt2::: is not needed.

-   PRF (nonparametric, parametric may follow later)

<!-- -->

    plot.irt(my_model, type = 'np_prf') # Add parametric PRF? xxxx lux
    # Object Y not found... xxxx

-   MLM (Reise, Conijn)

<!-- -->

    mlm_out <- pfa_mlm(pfa_object = pfa, wizirt_fit = my_model) # mlm_data not found xxxx
    # add predictors xxxx
    # Add some summarization methods

    # Does wizirt do anything else?

-   Learning Resources
    -   [Github website](https://github.com/Pflegermeister/wizirt2)
        <!--Everything needs to be moved to a new repository, then this one can be deleted. -->
    -   CRAN-style reference page
        <!-- I think this is made automatically, but I don't know where or how? xxxx--><!-- All documentation needs to be cleaned and improved. xxxx xxxx-->
    -   [tidyverse-style cheat sheet]()
        <!-- needs link, and update xxxx-->
    -   Karabatsos’s simulation replication article
        <!-- needs to be finished, cleaned and written up xxxx-->
-   Quality Assurance
    -   Accuracy check
        -   Match IRTPRO, Winsteps
            <!-- Figure out how to add control values for ltm and mirt to match IRTPRO without changing IRTPRO defaults, Winsteps xxxx-->
        -   Replicate Karabatsos (Decided to replicate Sinharay because
            Karabatsos had problems.)
    -   Informative check
        <!-- (meet all the checks above 'Learning Resources' xxxx) -->
    -   Usefulness check
        -   Usability testing <!-- Round two is Friday! -->
        -   Call it from python on separate laptop <!-- Do this. xxxx-->
    -   Aesthetics check
        -   Repetition
        -   Contrast
        -   Alignment
        -   Proximity

My goal is to complete all of this by October 24. That will give me a
week to write it all up by October 30 and give it to my chair. Then I
will have a week to improve it according to my chair’s recommendations
so I can send it to my committee by November 6.
