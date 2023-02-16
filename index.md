index
================

``` r
# The research question I explored in this RMD file is “What is the association between poverty and educational level attainment in The State of Washington?” I used The 2021 American Community Survey (ACS) dataset for Washington State Residents. I cleaned the Data before I ran the regression. I limited the data to individuals who are 18 years old or older and are employed. The dependent variable in these regression analyses is the poverty level ( Which “expresses each family's total income for the previous year as a percentage of the poverty thresholds”). The independent variable is the educational attainment level for the individual with subsequent regressions controlling for family size, hours worked per week, race, Whether the individual speaks English and the annual gas and water bill burden of for households. The regression showed there is a positive association between having a high school diploma or less and being under the poverty threshold controlling for family size, hours worked per week, race, whether a person speaks English and bills burden. The results are statistically significant at the 0.001 and 0.05 levels. R2 started at 0.107974 and reached 0.2443194 after using all the controls in the regression. Despite the fact that R2 was not very high, the ANOVA test showed that the results of the regression kept getting stronger as we added all the controls.  
```

``` r
# clean memory
rm(list = ls())

#link To file
link='https://github.com/PUBPOL-542/R/raw/main/index.Rmd'

myFile=url(link)

# reading in data:
library(readxl)
multidata = read_excel("poverty21.xlsx")


# reset indexes to R format:
row.names(multidata)=NULL
```

``` r
# hypothesis 1: Poverty is associated with educational attainment level
hypo1=formula(poverty ~ educ)

# hypothesis 2: Poverty is associated with educational attainment level, family size 

hypo2=formula(poverty ~ educ + famsize)

# hypothesis 3: Poverty is associated with educational attainment level, family size, Hours worked per week

hypo3=formula(poverty ~ educ + famsize + uhrswork)

# hypothesis 4: Poverty is associated with educational attainment level, family size, Hours worked per week and Race

hypo4=formula(poverty ~ educ + famsize + uhrswork + race)

# hypothesis 5: Poverty is associated with educational attainment level, family size, Hours worked per week, Race and fluency in English

hypo5=formula(poverty ~ educ + famsize + uhrswork + race + speakeng)

# hypothesis 6: Poverty is associated with educational attainment level, family size, Hours worked per week, Race, fluency in English and bill burden

hypo6=formula(poverty ~ educ + famsize + uhrswork + race + speakeng + billburden)
```

``` r
# results
gauss1=glm(hypo1,
           data = multidata,
           family = 'gaussian')

gauss2=glm(hypo2,
           data = multidata,
           family = 'gaussian')

gauss3=glm(hypo3,
           data = multidata,
           family = 'gaussian')
           
gauss4=glm(hypo4,
           data = multidata,
           family = 'gaussian')
          
gauss5=glm(hypo5,
           data = multidata,
           family = 'gaussian')
           
gauss6=glm(hypo6,
           data = multidata,
           family = 'gaussian')
```

``` r
summary(gauss1)
```

    ## 
    ## Call:
    ## glm(formula = hypo1, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -461.60   -76.05    38.40    83.37   253.46  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    383.435      1.857 206.510  < 2e-16 ***
    ## educ2 years of college          11.196      2.777   4.031 5.57e-05 ***
    ## educ4 years of college          52.396      2.277  23.009  < 2e-16 ***
    ## educ5+ years of college         79.166      2.475  31.981  < 2e-16 ***
    ## educgrade 10                   -74.490      9.572  -7.782 7.35e-15 ***
    ## educgrade 11                   -38.798      7.116  -5.452 5.00e-08 ***
    ## educgrade 12                   -24.385      2.253 -10.825  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8       -116.118      7.530 -15.420  < 2e-16 ***
    ## educgrade 9                   -115.254     10.748 -10.724  < 2e-16 ***
    ## educn/a or no schooling        -72.667      6.711 -10.828  < 2e-16 ***
    ## educnursery school to grade 4 -135.897     14.324  -9.488  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 16137.66)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 566706062  on 35117  degrees of freedom
    ## AIC: 440054
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
summary(gauss2)
```

    ## 
    ## Call:
    ## glm(formula = hypo2, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -486.45   -69.22    38.43    92.74   299.14  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      332.223      2.311 143.770  < 2e-16 ***
    ## educ2 years of college            10.678      2.690   3.969 7.24e-05 ***
    ## educ4 years of college            50.160      2.207  22.730  < 2e-16 ***
    ## educ5+ years of college           75.960      2.400  31.654  < 2e-16 ***
    ## educgrade 10                     -65.500      9.280  -7.058 1.72e-12 ***
    ## educgrade 11                     -35.365      6.899  -5.126 2.97e-07 ***
    ## educgrade 12                     -22.961      2.184 -10.513  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8         -106.365      7.312 -14.546  < 2e-16 ***
    ## educgrade 9                     -104.164     10.422  -9.994  < 2e-16 ***
    ## educn/a or no schooling          -62.486      6.513  -9.594  < 2e-16 ***
    ## educnursery school to grade 4   -126.934     13.886  -9.141  < 2e-16 ***
    ## famsize10                        -11.912     16.994  -0.701  0.48333    
    ## famsize11                        -21.877     23.316  -0.938  0.34810    
    ## famsize12                        -56.111     32.923  -1.704  0.08833 .  
    ## famsize16                         15.737     71.056   0.221  0.82472    
    ## famsize2 family members present   72.185      1.972  36.597  < 2e-16 ***
    ## famsize3                          79.272      2.194  36.128  < 2e-16 ***
    ## famsize4                          61.694      2.254  27.368  < 2e-16 ***
    ## famsize5                          22.700      2.885   7.869 3.67e-15 ***
    ## famsize6                           7.438      4.073   1.826  0.06785 .  
    ## famsize7                         -24.002      5.944  -4.038 5.41e-05 ***
    ## famsize8                         -23.945      9.104  -2.630  0.00854 ** 
    ## famsize9                         -28.299     12.367  -2.288  0.02213 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 15135.49)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 531331429  on 35105  degrees of freedom
    ## AIC: 437814
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
summary(gauss3)
```

    ## 
    ## Call:
    ## glm(formula = hypo3, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -475.69   -66.79    28.50    85.46   332.36  
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      251.63322   22.63997  11.115  < 2e-16 ***
    ## educ2 years of college            10.61627    2.61418   4.061 4.90e-05 ***
    ## educ4 years of college            44.46009    2.14811  20.697  < 2e-16 ***
    ## educ5+ years of college           68.04334    2.33931  29.087  < 2e-16 ***
    ## educgrade 10                     -59.81057    9.00722  -6.640 3.18e-11 ***
    ## educgrade 11                     -21.01193    6.71072  -3.131 0.001743 ** 
    ## educgrade 12                     -22.81060    2.12211 -10.749  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8         -107.02884    7.11022 -15.053  < 2e-16 ***
    ## educgrade 9                     -104.92612   10.11258 -10.376  < 2e-16 ***
    ## educn/a or no schooling          -63.89392    6.32213 -10.106  < 2e-16 ***
    ## educnursery school to grade 4   -133.22239   13.48507  -9.879  < 2e-16 ***
    ## famsize10                         -9.09235   16.48821  -0.551 0.581332    
    ## famsize11                        -26.06013   22.61192  -1.152 0.249125    
    ## famsize12                        -67.82426   31.92187  -2.125 0.033619 *  
    ## famsize16                          0.31106   68.87372   0.005 0.996397    
    ## famsize2 family members present   71.51280    1.91530  37.338  < 2e-16 ***
    ## famsize3                          78.92482    2.13113  37.034  < 2e-16 ***
    ## famsize4                          61.32728    2.18853  28.022  < 2e-16 ***
    ## famsize5                          22.83926    2.80066   8.155 3.61e-16 ***
    ## famsize6                           7.22256    3.95419   1.827 0.067775 .  
    ## famsize7                         -23.52858    5.76721  -4.080 4.52e-05 ***
    ## famsize8                         -20.55429    8.83264  -2.327 0.019967 *  
    ## famsize9                         -24.95996   11.99817  -2.080 0.037504 *  
    ## uhrswork10                        29.93730   23.35415   1.282 0.199892    
    ## uhrswork11                        -9.56247   40.04032  -0.239 0.811246    
    ## uhrswork12                        26.66141   23.95877   1.113 0.265800    
    ## uhrswork13                       -18.42010   32.49376  -0.567 0.570798    
    ## uhrswork14                       -31.40736   28.71155  -1.094 0.274009    
    ## uhrswork15                        20.27026   23.31813   0.869 0.384694    
    ## uhrswork16                        15.59638   24.11561   0.647 0.517809    
    ## uhrswork17                       -39.11243   29.70678  -1.317 0.187976    
    ## uhrswork18                        17.82635   25.50598   0.699 0.484614    
    ## uhrswork19                        40.53394   39.04995   1.038 0.299276    
    ## uhrswork2                         42.38929   27.16007   1.561 0.118599    
    ## uhrswork20                        24.03515   22.79510   1.054 0.291707    
    ## uhrswork21                        57.83591   31.09988   1.860 0.062939 .  
    ## uhrswork22                        10.15585   26.62022   0.382 0.702828    
    ## uhrswork23                        -9.47011   33.56871  -0.282 0.777860    
    ## uhrswork24                        45.88734   23.39418   1.961 0.049830 *  
    ## uhrswork25                        29.25092   22.98295   1.273 0.203125    
    ## uhrswork26                         0.09329   27.37722   0.003 0.997281    
    ## uhrswork27                         7.29489   29.54797   0.247 0.805000    
    ## uhrswork28                        32.86870   24.59943   1.336 0.181507    
    ## uhrswork29                        26.64592   31.89031   0.836 0.403414    
    ## uhrswork3                         23.83075   26.27658   0.907 0.364455    
    ## uhrswork30                        43.88701   22.76102   1.928 0.053843 .  
    ## uhrswork31                        62.41936   32.82279   1.902 0.057218 .  
    ## uhrswork32                        76.61116   23.00071   3.331 0.000867 ***
    ## uhrswork33                        57.60984   29.24661   1.970 0.048870 *  
    ## uhrswork34                        46.83774   26.03425   1.799 0.072014 .  
    ## uhrswork35                        38.83534   22.86589   1.698 0.089442 .  
    ## uhrswork36                        78.75447   23.17802   3.398 0.000680 ***
    ## uhrswork37                        43.77221   24.71078   1.771 0.076506 .  
    ## uhrswork38                        39.66874   23.43686   1.693 0.090545 .  
    ## uhrswork39                        30.81204   26.84597   1.148 0.251086    
    ## uhrswork4                         30.69988   25.56117   1.201 0.229745    
    ## uhrswork40                        95.86632   22.56051   4.249 2.15e-05 ***
    ## uhrswork41                        72.85716   29.10122   2.504 0.012299 *  
    ## uhrswork42                        89.24705   23.68314   3.768 0.000165 ***
    ## uhrswork43                        98.14911   25.87572   3.793 0.000149 ***
    ## uhrswork44                       108.39772   25.68117   4.221 2.44e-05 ***
    ## uhrswork45                       113.22320   22.71835   4.984 6.27e-07 ***
    ## uhrswork46                       101.15691   27.22959   3.715 0.000204 ***
    ## uhrswork47                       121.79710   32.16876   3.786 0.000153 ***
    ## uhrswork48                       120.56851   23.58523   5.112 3.20e-07 ***
    ## uhrswork49                        96.41106   36.67365   2.629 0.008570 ** 
    ## uhrswork5                          0.52711   24.88524   0.021 0.983101    
    ## uhrswork50                       115.07999   22.65556   5.080 3.80e-07 ***
    ## uhrswork51                       147.72470   53.70824   2.751 0.005953 ** 
    ## uhrswork52                        91.95265   26.19573   3.510 0.000448 ***
    ## uhrswork53                       130.12930   45.70159   2.847 0.004411 ** 
    ## uhrswork54                       100.22989   35.47109   2.826 0.004721 ** 
    ## uhrswork55                       118.07628   23.20612   5.088 3.63e-07 ***
    ## uhrswork56                        76.45181   29.54907   2.587 0.009678 ** 
    ## uhrswork57                       122.04686   53.65215   2.275 0.022925 *  
    ## uhrswork58                       137.68915   34.92080   3.943 8.07e-05 ***
    ## uhrswork59                       123.94674   87.29774   1.420 0.155670    
    ## uhrswork6                         36.68219   25.74400   1.425 0.154200    
    ## uhrswork60                       112.65761   22.82381   4.936 8.01e-07 ***
    ## uhrswork62                       102.60883   43.95232   2.335 0.019573 *  
    ## uhrswork63                        64.27142   87.30887   0.736 0.461651    
    ## uhrswork64                        65.80533   42.44942   1.550 0.121101    
    ## uhrswork65                       122.02098   24.66490   4.947 7.57e-07 ***
    ## uhrswork66                        25.36678   72.47354   0.350 0.726330    
    ## uhrswork67                       187.67432   87.28611   2.150 0.031554 *  
    ## uhrswork68                        92.03950  121.37471   0.758 0.448271    
    ## uhrswork69                        70.77034   72.44785   0.977 0.328652    
    ## uhrswork7                        -15.09100   28.59630  -0.528 0.597693    
    ## uhrswork70                        81.78635   24.13845   3.388 0.000704 ***
    ## uhrswork72                        72.06213   30.65206   2.351 0.018730 *  
    ## uhrswork74                       -68.47076   87.28501  -0.784 0.432781    
    ## uhrswork75                       113.93601   30.24279   3.767 0.000165 ***
    ## uhrswork76                       -21.74744  121.36876  -0.179 0.857794    
    ## uhrswork77                        80.45649   72.45150   1.110 0.266797    
    ## uhrswork78                       -24.49847   87.28968  -0.281 0.778975    
    ## uhrswork79                       107.52752  121.38947   0.886 0.375728    
    ## uhrswork8                         25.39772   23.69861   1.072 0.283864    
    ## uhrswork80                        85.89128   24.33150   3.530 0.000416 ***
    ## uhrswork81                       200.66458  121.36443   1.653 0.098257 .  
    ## uhrswork82                        85.56531   72.44949   1.181 0.237597    
    ## uhrswork83                       114.25469   87.29313   1.309 0.190590    
    ## uhrswork84                       105.17388   31.10053   3.382 0.000721 ***
    ## uhrswork85                        -2.73624   40.03380  -0.068 0.945509    
    ## uhrswork88                       -10.90176   63.75080  -0.171 0.864220    
    ## uhrswork9                         11.58838   28.36661   0.409 0.682893    
    ## uhrswork90                        61.16740   33.99004   1.800 0.071937 .  
    ## uhrswork91                       -24.60611  121.36116  -0.203 0.839331    
    ## uhrswork95                        94.22406   72.47285   1.300 0.193565    
    ## uhrswork96                       114.80934   53.65237   2.140 0.032372 *  
    ## uhrswork98                       127.70949   50.39829   2.534 0.011281 *  
    ## uhrswork99 (topcode)             108.81232   28.47718   3.821 0.000133 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 14219)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 497906550  on 35017  degrees of freedom
    ## AIC: 435707
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
summary(gauss4)
```

    ## 
    ## Call:
    ## glm(formula = hypo4, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -478.41   -65.97    25.70    82.21   315.57  
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          203.9821    22.9575   8.885  < 2e-16 ***
    ## educ2 years of college                 9.3158     2.5941   3.591 0.000330 ***
    ## educ4 years of college                41.7964     2.1380  19.549  < 2e-16 ***
    ## educ5+ years of college               64.4575     2.3444  27.495  < 2e-16 ***
    ## educgrade 10                         -56.5345     8.9369  -6.326 2.55e-10 ***
    ## educgrade 11                         -15.6071     6.6617  -2.343 0.019144 *  
    ## educgrade 12                         -21.0251     2.1069  -9.979  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8              -80.4744     7.1665 -11.229  < 2e-16 ***
    ## educgrade 9                          -84.6860    10.0732  -8.407  < 2e-16 ***
    ## educn/a or no schooling              -50.7092     6.3200  -8.024 1.06e-15 ***
    ## educnursery school to grade 4       -101.5521    13.4604  -7.544 4.65e-14 ***
    ## famsize10                             -4.6605    16.3569  -0.285 0.775704    
    ## famsize11                            -13.1471    22.4418  -0.586 0.557995    
    ## famsize12                            -74.9236    31.6856  -2.365 0.018055 *  
    ## famsize16                             -9.6464    68.3532  -0.141 0.887772    
    ## famsize2 family members present       70.9029     1.9021  37.277  < 2e-16 ***
    ## famsize3                              79.1687     2.1157  37.419  < 2e-16 ***
    ## famsize4                              62.2642     2.1743  28.636  < 2e-16 ***
    ## famsize5                              25.9333     2.7854   9.310  < 2e-16 ***
    ## famsize6                              12.1813     3.9341   3.096 0.001961 ** 
    ## famsize7                             -18.5635     5.7374  -3.236 0.001215 ** 
    ## famsize8                             -15.8095     8.7671  -1.803 0.071352 .  
    ## famsize9                             -20.0405    11.9101  -1.683 0.092452 .  
    ## uhrswork10                            29.4527    23.1702   1.271 0.203686    
    ## uhrswork11                           -12.2661    39.7211  -0.309 0.757470    
    ## uhrswork12                            24.8589    23.7696   1.046 0.295648    
    ## uhrswork13                           -12.7396    32.2414  -0.395 0.692749    
    ## uhrswork14                           -35.2381    28.4874  -1.237 0.216106    
    ## uhrswork15                            19.2714    23.1337   0.833 0.404825    
    ## uhrswork16                            16.1151    23.9247   0.674 0.500585    
    ## uhrswork17                           -37.5628    29.4724  -1.275 0.202492    
    ## uhrswork18                            17.5984    25.3036   0.695 0.486753    
    ## uhrswork19                            47.0380    38.7379   1.214 0.224656    
    ## uhrswork2                             41.2498    26.9430   1.531 0.125779    
    ## uhrswork20                            24.4372    22.6149   1.081 0.279891    
    ## uhrswork21                            53.0197    30.8532   1.718 0.085723 .  
    ## uhrswork22                             9.5402    26.4097   0.361 0.717924    
    ## uhrswork23                           -11.7429    33.3007  -0.353 0.724366    
    ## uhrswork24                            47.5916    23.2089   2.051 0.040316 *  
    ## uhrswork25                            29.7840    22.8008   1.306 0.191469    
    ## uhrswork26                             0.5220    27.1589   0.019 0.984666    
    ## uhrswork27                             8.9340    29.3150   0.305 0.760551    
    ## uhrswork28                            33.3294    24.4049   1.366 0.172047    
    ## uhrswork29                            24.6875    31.6360   0.780 0.435184    
    ## uhrswork3                             22.7572    26.0701   0.873 0.382709    
    ## uhrswork30                            44.5596    22.5815   1.973 0.048472 *  
    ## uhrswork31                            58.9546    32.5604   1.811 0.070207 .  
    ## uhrswork32                            77.5735    22.8182   3.400 0.000675 ***
    ## uhrswork33                            56.4535    29.0143   1.946 0.051698 .  
    ## uhrswork34                            48.4630    25.8277   1.876 0.060608 .  
    ## uhrswork35                            41.0652    22.6856   1.810 0.070275 .  
    ## uhrswork36                            79.6188    22.9943   3.463 0.000536 ***
    ## uhrswork37                            43.5103    24.5153   1.775 0.075935 .  
    ## uhrswork38                            42.8715    23.2523   1.844 0.065227 .  
    ## uhrswork39                            28.0227    26.6327   1.052 0.292718    
    ## uhrswork4                             29.5341    25.3569   1.165 0.244134    
    ## uhrswork40                            97.0844    22.3819   4.338 1.44e-05 ***
    ## uhrswork41                            72.0487    28.8694   2.496 0.012576 *  
    ## uhrswork42                            89.4399    23.4943   3.807 0.000141 ***
    ## uhrswork43                            98.8865    25.6702   3.852 0.000117 ***
    ## uhrswork44                           110.0100    25.4792   4.318 1.58e-05 ***
    ## uhrswork45                           113.5659    22.5393   5.039 4.71e-07 ***
    ## uhrswork46                           103.1178    27.0136   3.817 0.000135 ***
    ## uhrswork47                           118.6599    31.9123   3.718 0.000201 ***
    ## uhrswork48                           121.1477    23.3977   5.178 2.26e-07 ***
    ## uhrswork49                            93.0109    36.3802   2.557 0.010573 *  
    ## uhrswork5                             -0.2219    24.6872  -0.009 0.992828    
    ## uhrswork50                           115.4618    22.4770   5.137 2.81e-07 ***
    ## uhrswork51                           152.7987    53.2816   2.868 0.004136 ** 
    ## uhrswork52                            97.9004    25.9888   3.767 0.000165 ***
    ## uhrswork53                           133.2531    45.3344   2.939 0.003291 ** 
    ## uhrswork54                           105.8769    35.1890   3.009 0.002625 ** 
    ## uhrswork55                           117.9938    23.0229   5.125 2.99e-07 ***
    ## uhrswork56                            74.6087    29.3161   2.545 0.010933 *  
    ## uhrswork57                           136.0836    53.2390   2.556 0.010590 *  
    ## uhrswork58                           135.1230    34.6443   3.900 9.63e-05 ***
    ## uhrswork59                           118.1658    86.5946   1.365 0.172392    
    ## uhrswork6                             37.1568    25.5387   1.455 0.145701    
    ## uhrswork60                           113.1416    22.6432   4.997 5.86e-07 ***
    ## uhrswork62                           120.2834    43.6130   2.758 0.005819 ** 
    ## uhrswork63                            57.1773    86.6076   0.660 0.509137    
    ## uhrswork64                            64.4282    42.1085   1.530 0.126012    
    ## uhrswork65                           120.3962    24.4705   4.920 8.69e-07 ***
    ## uhrswork66                            25.9760    71.9528   0.361 0.718091    
    ## uhrswork67                           183.2667    86.5832   2.117 0.034296 *  
    ## uhrswork68                            85.4851   120.3959   0.710 0.477688    
    ## uhrswork69                            65.1757    71.8652   0.907 0.364457    
    ## uhrswork7                            -13.9194    28.3690  -0.491 0.623673    
    ## uhrswork70                            81.2906    23.9478   3.394 0.000688 ***
    ## uhrswork72                            80.3508    30.4099   2.642 0.008239 ** 
    ## uhrswork74                           -49.2561    86.6069  -0.569 0.569542    
    ## uhrswork75                           118.5380    30.0033   3.951 7.80e-05 ***
    ## uhrswork76                            32.6191   120.4258   0.271 0.786497    
    ## uhrswork77                            72.4837    71.8818   1.008 0.313282    
    ## uhrswork78                           -16.9817    86.6460  -0.196 0.844620    
    ## uhrswork79                            98.8160   120.4109   0.821 0.411847    
    ## uhrswork8                             26.1527    23.5103   1.112 0.265976    
    ## uhrswork80                            88.4735    24.1389   3.665 0.000248 ***
    ## uhrswork81                           193.8715   120.3852   1.610 0.107314    
    ## uhrswork82                            94.1373    71.8726   1.310 0.190279    
    ## uhrswork83                           108.3001    86.5907   1.251 0.211048    
    ## uhrswork84                           108.9001    30.8547   3.529 0.000417 ***
    ## uhrswork85                            -4.7764    39.7139  -0.120 0.904269    
    ## uhrswork88                           -16.9390    63.2468  -0.268 0.788836    
    ## uhrswork9                             12.3576    28.1442   0.439 0.660605    
    ## uhrswork90                            61.0833    33.7220   1.811 0.070091 .  
    ## uhrswork91                           -26.9500   120.3820  -0.224 0.822859    
    ## uhrswork95                            89.2011    71.8905   1.241 0.214691    
    ## uhrswork96                           108.1566    53.2217   2.032 0.042142 *  
    ## uhrswork98                           137.9713    50.0026   2.759 0.005796 ** 
    ## uhrswork99 (topcode)                 109.4439    28.2496   3.874 0.000107 ***
    ## raceblack/african american            28.1622     6.1084   4.610 4.03e-06 ***
    ## racechinese                           54.2794     6.1781   8.786  < 2e-16 ***
    ## racejapanese                          64.2599    10.5006   6.120 9.48e-10 ***
    ## raceother asian or pacific islander   54.6050     5.2601  10.381  < 2e-16 ***
    ## raceother race, nec                   -8.7449     5.6253  -1.555 0.120059    
    ## racethree or more major races         26.2711     8.0020   3.283 0.001028 ** 
    ## racetwo major races                   30.8088     5.1686   5.961 2.54e-09 ***
    ## racewhite                             53.2686     4.8237  11.043  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 13990.11)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 489779731  on 35009  degrees of freedom
    ## AIC: 435145
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
summary(gauss5)
```

    ## 
    ## Call:
    ## glm(formula = hypo5, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -478.12   -65.47    24.81    82.37   325.07  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          135.932     24.761   5.490 4.05e-08 ***
    ## educ2 years of college                 9.271      2.587   3.583 0.000340 ***
    ## educ4 years of college                41.551      2.133  19.480  < 2e-16 ***
    ## educ5+ years of college               64.716      2.343  27.617  < 2e-16 ***
    ## educgrade 10                         -51.078      8.925  -5.723 1.06e-08 ***
    ## educgrade 11                         -15.235      6.645  -2.293 0.021869 *  
    ## educgrade 12                         -20.134      2.103  -9.573  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8              -53.510      7.545  -7.093 1.34e-12 ***
    ## educgrade 9                          -68.165     10.152  -6.714 1.92e-11 ***
    ## educn/a or no schooling              -34.231      6.493  -5.272 1.36e-07 ***
    ## educnursery school to grade 4        -72.446     13.834  -5.237 1.64e-07 ***
    ## famsize10                              4.339     16.334   0.266 0.790517    
    ## famsize11                             -5.628     22.396  -0.251 0.801594    
    ## famsize12                            -70.558     31.606  -2.232 0.025593 *  
    ## famsize16                             -4.761     68.182  -0.070 0.944327    
    ## famsize2 family members present       71.056      1.897  37.453  < 2e-16 ***
    ## famsize3                              80.085      2.112  37.922  < 2e-16 ***
    ## famsize4                              63.565      2.172  29.271  < 2e-16 ***
    ## famsize5                              28.538      2.786  10.245  < 2e-16 ***
    ## famsize6                              15.976      3.935   4.060 4.91e-05 ***
    ## famsize7                             -13.836      5.735  -2.412 0.015851 *  
    ## famsize8                              -9.838      8.761  -1.123 0.261440    
    ## famsize9                             -14.045     11.896  -1.181 0.237748    
    ## uhrswork10                            27.993     23.112   1.211 0.225835    
    ## uhrswork11                           -18.378     39.623  -0.464 0.642777    
    ## uhrswork12                            22.508     23.711   0.949 0.342488    
    ## uhrswork13                           -12.780     32.162  -0.397 0.691100    
    ## uhrswork14                           -37.462     28.415  -1.318 0.187376    
    ## uhrswork15                            16.856     23.077   0.730 0.465132    
    ## uhrswork16                            13.800     23.865   0.578 0.563081    
    ## uhrswork17                           -39.254     29.398  -1.335 0.181793    
    ## uhrswork18                            15.106     25.240   0.598 0.549525    
    ## uhrswork19                            44.093     38.640   1.141 0.253835    
    ## uhrswork2                             39.538     26.875   1.471 0.141248    
    ## uhrswork20                            22.286     22.558   0.988 0.323197    
    ## uhrswork21                            49.469     30.776   1.607 0.107982    
    ## uhrswork22                             7.140     26.344   0.271 0.786366    
    ## uhrswork23                           -14.087     33.217  -0.424 0.671515    
    ## uhrswork24                            45.268     23.151   1.955 0.050548 .  
    ## uhrswork25                            27.144     22.744   1.193 0.232692    
    ## uhrswork26                            -1.051     27.090  -0.039 0.969045    
    ## uhrswork27                             8.935     29.241   0.306 0.759943    
    ## uhrswork28                            30.400     24.344   1.249 0.211749    
    ## uhrswork29                            22.385     31.557   0.709 0.478107    
    ## uhrswork3                             19.221     26.006   0.739 0.459840    
    ## uhrswork30                            42.601     22.525   1.891 0.058599 .  
    ## uhrswork31                            57.366     32.478   1.766 0.077358 .  
    ## uhrswork32                            74.944     22.761   3.293 0.000994 ***
    ## uhrswork33                            54.155     28.942   1.871 0.061331 .  
    ## uhrswork34                            46.286     25.762   1.797 0.072396 .  
    ## uhrswork35                            38.822     22.629   1.716 0.086243 .  
    ## uhrswork36                            77.051     22.937   3.359 0.000782 ***
    ## uhrswork37                            41.248     24.454   1.687 0.091661 .  
    ## uhrswork38                            39.878     23.194   1.719 0.085568 .  
    ## uhrswork39                            26.995     26.565   1.016 0.309552    
    ## uhrswork4                             27.698     25.293   1.095 0.273486    
    ## uhrswork40                            94.830     22.326   4.248 2.17e-05 ***
    ## uhrswork41                            68.325     28.797   2.373 0.017667 *  
    ## uhrswork42                            87.228     23.435   3.722 0.000198 ***
    ## uhrswork43                            98.011     25.606   3.828 0.000130 ***
    ## uhrswork44                           108.993     25.415   4.289 1.80e-05 ***
    ## uhrswork45                           110.834     22.483   4.930 8.28e-07 ***
    ## uhrswork46                           100.553     26.946   3.732 0.000191 ***
    ## uhrswork47                           115.503     31.832   3.629 0.000285 ***
    ## uhrswork48                           119.127     23.339   5.104 3.34e-07 ***
    ## uhrswork49                            87.161     36.290   2.402 0.016320 *  
    ## uhrswork5                             -1.880     24.625  -0.076 0.939130    
    ## uhrswork50                           112.790     22.421   5.030 4.92e-07 ***
    ## uhrswork51                           147.493     53.148   2.775 0.005521 ** 
    ## uhrswork52                            97.005     25.924   3.742 0.000183 ***
    ## uhrswork53                           134.512     45.218   2.975 0.002934 ** 
    ## uhrswork54                           103.253     35.102   2.942 0.003268 ** 
    ## uhrswork55                           114.185     22.967   4.972 6.67e-07 ***
    ## uhrswork56                            77.099     29.242   2.637 0.008379 ** 
    ## uhrswork57                           134.242     53.105   2.528 0.011481 *  
    ## uhrswork58                           134.665     34.556   3.897 9.76e-05 ***
    ## uhrswork59                           114.970     86.372   1.331 0.183162    
    ## uhrswork6                             33.515     25.475   1.316 0.188317    
    ## uhrswork60                           110.518     22.587   4.893 9.97e-07 ***
    ## uhrswork62                           123.670     43.506   2.843 0.004477 ** 
    ## uhrswork63                            45.047     86.391   0.521 0.602068    
    ## uhrswork64                            61.146     42.001   1.456 0.145448    
    ## uhrswork65                           117.510     24.410   4.814 1.48e-06 ***
    ## uhrswork66                            49.993     71.855   0.696 0.486588    
    ## uhrswork67                           211.910     86.396   2.453 0.014180 *  
    ## uhrswork68                            81.716    120.085   0.680 0.496206    
    ## uhrswork69                            67.252     71.682   0.938 0.348147    
    ## uhrswork7                            -16.286     28.297  -0.576 0.564937    
    ## uhrswork70                            80.293     23.887   3.361 0.000776 ***
    ## uhrswork72                            79.564     30.335   2.623 0.008723 ** 
    ## uhrswork74                           -56.317     86.386  -0.652 0.514451    
    ## uhrswork75                           116.848     29.928   3.904 9.47e-05 ***
    ## uhrswork76                            30.991    120.121   0.258 0.796409    
    ## uhrswork77                            81.857     71.722   1.141 0.253748    
    ## uhrswork78                           -21.512     86.423  -0.249 0.803426    
    ## uhrswork79                            93.743    120.101   0.781 0.435081    
    ## uhrswork8                             23.399     23.452   0.998 0.318407    
    ## uhrswork80                            86.181     24.080   3.579 0.000345 ***
    ## uhrswork81                           190.358    120.075   1.585 0.112900    
    ## uhrswork82                            87.899     71.689   1.226 0.220166    
    ## uhrswork83                           118.316     86.383   1.370 0.170797    
    ## uhrswork84                           106.865     30.778   3.472 0.000517 ***
    ## uhrswork85                            -8.779     39.614  -0.222 0.824611    
    ## uhrswork88                           -17.134     63.086  -0.272 0.785935    
    ## uhrswork9                              9.094     28.076   0.324 0.746017    
    ## uhrswork90                            58.415     33.637   1.737 0.082461 .  
    ## uhrswork91                           -29.326    120.071  -0.244 0.807046    
    ## uhrswork95                            85.605     71.706   1.194 0.232550    
    ## uhrswork96                           104.465     53.085   1.968 0.049091 *  
    ## uhrswork98                           135.661     49.877   2.720 0.006533 ** 
    ## uhrswork99 (topcode)                 109.328     28.181   3.879 0.000105 ***
    ## raceblack/african american            29.873      6.096   4.900 9.62e-07 ***
    ## racechinese                           68.790      6.290  10.937  < 2e-16 ***
    ## racejapanese                          71.088     10.494   6.774 1.27e-11 ***
    ## raceother asian or pacific islander   67.047      5.362  12.505  < 2e-16 ***
    ## raceother race, nec                    3.089      5.691   0.543 0.587270    
    ## racethree or more major races         26.721      7.982   3.348 0.000816 ***
    ## racetwo major races                   33.507      5.159   6.495 8.43e-11 ***
    ## racewhite                             51.357      4.817  10.661  < 2e-16 ***
    ## speakengyes, but not well             25.931      9.788   2.649 0.008072 ** 
    ## speakengyes, speaks only english      72.431      9.374   7.727 1.13e-14 ***
    ## speakengyes, speaks very well         56.036      9.427   5.944 2.80e-09 ***
    ## speakengyes, speaks well              44.186      9.651   4.578 4.71e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 13917.89)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 487195653  on 35005  degrees of freedom
    ## AIC: 434968
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
summary(gauss6)
```

    ## 
    ## Call:
    ## glm(formula = hypo6, family = "gaussian", data = multidata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -468.28   -65.10    24.98    80.96  2123.58  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         146.1011    24.5437   5.953 2.66e-09 ***
    ## educ2 years of college                9.4083     2.5644   3.669 0.000244 ***
    ## educ4 years of college               41.3736     2.1140  19.572  < 2e-16 ***
    ## educ5+ years of college              63.9150     2.3227  27.518  < 2e-16 ***
    ## educgrade 10                        -50.9214     8.8457  -5.757 8.65e-09 ***
    ## educgrade 11                        -15.4257     6.5858  -2.342 0.019172 *  
    ## educgrade 12                        -19.9010     2.0844  -9.547  < 2e-16 ***
    ## educgrade 5, 6, 7, or 8             -52.7190     7.4774  -7.050 1.82e-12 ***
    ## educgrade 9                         -63.5417    10.0635  -6.314 2.75e-10 ***
    ## educn/a or no schooling             -33.4662     6.4354  -5.200 2.00e-07 ***
    ## educnursery school to grade 4       -70.6459    13.7108  -5.153 2.58e-07 ***
    ## famsize10                             0.7527    16.1894   0.046 0.962918    
    ## famsize11                            -8.5630    22.1963  -0.386 0.699659    
    ## famsize12                           -72.9231    31.3244  -2.328 0.019918 *  
    ## famsize16                            -8.0771    67.5745  -0.120 0.904858    
    ## famsize2 family members present      68.6837     1.8827  36.482  < 2e-16 ***
    ## famsize3                             77.4368     2.0957  36.951  < 2e-16 ***
    ## famsize4                             60.7222     2.1552  28.174  < 2e-16 ***
    ## famsize5                             25.5752     2.7633   9.255  < 2e-16 ***
    ## famsize6                             12.8140     3.9017   3.284 0.001024 ** 
    ## famsize7                            -17.0675     5.6856  -3.002 0.002685 ** 
    ## famsize8                            -13.1390     8.6835  -1.513 0.130261    
    ## famsize9                            -17.7451    11.7905  -1.505 0.132325    
    ## uhrswork10                           26.9122    22.9062   1.175 0.240047    
    ## uhrswork11                            9.8927    39.2859   0.252 0.801188    
    ## uhrswork12                           22.3403    23.4995   0.951 0.341777    
    ## uhrswork13                           -0.8030    31.8793  -0.025 0.979904    
    ## uhrswork14                          -40.5277    28.1620  -1.439 0.150133    
    ## uhrswork15                           13.7459    22.8717   0.601 0.547844    
    ## uhrswork16                           11.8068    23.6524   0.499 0.617656    
    ## uhrswork17                          -40.3415    29.1357  -1.385 0.166181    
    ## uhrswork18                           11.3461    25.0156   0.454 0.650146    
    ## uhrswork19                           40.6679    38.2965   1.062 0.288278    
    ## uhrswork2                            34.9017    26.6358   1.310 0.190094    
    ## uhrswork20                           19.9380    22.3575   0.892 0.372515    
    ## uhrswork21                           44.5838    30.5028   1.462 0.143851    
    ## uhrswork22                            2.1633    26.1098   0.083 0.933969    
    ## uhrswork23                          -17.9697    32.9219  -0.546 0.585187    
    ## uhrswork24                           40.8714    22.9450   1.781 0.074876 .  
    ## uhrswork25                           23.6485    22.5418   1.049 0.294141    
    ## uhrswork26                           -5.6052    26.8489  -0.209 0.834630    
    ## uhrswork27                            4.4772    28.9814   0.154 0.877228    
    ## uhrswork28                           25.6619    24.1276   1.064 0.287522    
    ## uhrswork29                           17.2070    31.2766   0.550 0.582216    
    ## uhrswork3                            18.1208    25.7740   0.703 0.482019    
    ## uhrswork30                           38.3591    22.3252   1.718 0.085769 .  
    ## uhrswork31                           52.2481    32.1898   1.623 0.104571    
    ## uhrswork32                           69.9425    22.5595   3.100 0.001934 ** 
    ## uhrswork33                           49.4643    28.6849   1.724 0.084644 .  
    ## uhrswork34                           41.3154    25.5333   1.618 0.105650    
    ## uhrswork35                           34.1335    22.4282   1.522 0.128043    
    ## uhrswork36                           72.3888    22.7337   3.184 0.001453 ** 
    ## uhrswork37                           36.2724    24.2373   1.497 0.134519    
    ## uhrswork38                           35.0873    22.9885   1.526 0.126945    
    ## uhrswork39                           22.1119    26.3291   0.840 0.401012    
    ## uhrswork4                            23.9438    25.0685   0.955 0.339517    
    ## uhrswork40                           89.6340    22.1282   4.051 5.12e-05 ***
    ## uhrswork41                           62.7363    28.5416   2.198 0.027951 *  
    ## uhrswork42                           82.1643    23.2271   3.537 0.000405 ***
    ## uhrswork43                           92.7578    25.3784   3.655 0.000258 ***
    ## uhrswork44                          103.4332    25.1896   4.106 4.03e-05 ***
    ## uhrswork45                          105.3706    22.2842   4.728 2.27e-06 ***
    ## uhrswork46                           95.1743    26.7072   3.564 0.000366 ***
    ## uhrswork47                          110.2708    31.5488   3.495 0.000474 ***
    ## uhrswork48                          113.6203    23.1322   4.912 9.07e-07 ***
    ## uhrswork49                           81.7971    35.9674   2.274 0.022960 *  
    ## uhrswork5                            -5.1596    24.4063  -0.211 0.832574    
    ## uhrswork50                          107.4337    22.2226   4.834 1.34e-06 ***
    ## uhrswork51                          142.4891    52.6749   2.705 0.006832 ** 
    ## uhrswork52                           91.9129    25.6935   3.577 0.000348 ***
    ## uhrswork53                          129.4575    44.8155   2.889 0.003871 ** 
    ## uhrswork54                           97.6152    34.7900   2.806 0.005021 ** 
    ## uhrswork55                          108.7129    22.7631   4.776 1.80e-06 ***
    ## uhrswork56                           71.4502    28.9826   2.465 0.013695 *  
    ## uhrswork57                          128.8272    52.6325   2.448 0.014383 *  
    ## uhrswork58                          129.1155    34.2490   3.770 0.000164 ***
    ## uhrswork59                          109.4552    85.6025   1.279 0.201030    
    ## uhrswork6                            30.0422    25.2487   1.190 0.234112    
    ## uhrswork60                          105.1944    22.3867   4.699 2.62e-06 ***
    ## uhrswork62                          118.4709    43.1192   2.748 0.006008 ** 
    ## uhrswork63                           39.9070    85.6219   0.466 0.641159    
    ## uhrswork64                           55.8169    41.6270   1.341 0.179967    
    ## uhrswork65                          112.0873    24.1932   4.633 3.62e-06 ***
    ## uhrswork66                           46.4263    71.2150   0.652 0.514459    
    ## uhrswork67                          205.2682    85.6263   2.397 0.016524 *  
    ## uhrswork68                           78.1278   119.0158   0.656 0.511540    
    ## uhrswork69                           62.9921    71.0432   0.887 0.375261    
    ## uhrswork7                           -14.0047    28.0450  -0.499 0.617526    
    ## uhrswork70                           75.7962    23.6751   3.202 0.001368 ** 
    ## uhrswork72                           74.8097    30.0650   2.488 0.012842 *  
    ## uhrswork74                          -59.8197    85.6164  -0.699 0.484747    
    ## uhrswork75                          111.0644    29.6625   3.744 0.000181 ***
    ## uhrswork76                           26.1483   119.0513   0.220 0.826154    
    ## uhrswork77                           76.9605    71.0834   1.083 0.278958    
    ## uhrswork78                          -26.7015    85.6538  -0.312 0.755242    
    ## uhrswork79                           89.6767   119.0310   0.753 0.451221    
    ## uhrswork8                            20.8631    23.2429   0.898 0.369398    
    ## uhrswork80                           81.1725    23.8660   3.401 0.000672 ***
    ## uhrswork81                          184.5668   119.0053   1.551 0.120932    
    ## uhrswork82                           81.9138    71.0510   1.153 0.248965    
    ## uhrswork83                          112.5639    85.6134   1.315 0.188588    
    ## uhrswork84                          101.0661    30.5043   3.313 0.000923 ***
    ## uhrswork85                          -11.3636    39.2614  -0.289 0.772251    
    ## uhrswork88                          -20.7545    62.5242  -0.332 0.739934    
    ## uhrswork9                            12.1201    27.8259   0.436 0.663154    
    ## uhrswork90                           56.8546    33.3374   1.705 0.088123 .  
    ## uhrswork91                          -33.7392   119.0019  -0.284 0.776782    
    ## uhrswork95                           80.1361    71.0674   1.128 0.259493    
    ## uhrswork96                           99.1907    52.6130   1.885 0.059399 .  
    ## uhrswork98                          130.7044    49.4330   2.644 0.008195 ** 
    ## uhrswork99 (topcode)                103.4046    27.9309   3.702 0.000214 ***
    ## raceblack/african american           29.7907     6.0420   4.931 8.23e-07 ***
    ## racechinese                          68.6872     6.2336  11.019  < 2e-16 ***
    ## racejapanese                         69.6685    10.4011   6.698 2.14e-11 ***
    ## raceother asian or pacific islander  66.0354     5.3140  12.427  < 2e-16 ***
    ## raceother race, nec                   2.8352     5.6403   0.503 0.615198    
    ## racethree or more major races        26.1771     7.9107   3.309 0.000937 ***
    ## racetwo major races                  33.1294     5.1132   6.479 9.35e-11 ***
    ## racewhite                            50.7554     4.7746  10.630  < 2e-16 ***
    ## speakengyes, but not well            25.2998     9.7009   2.608 0.009111 ** 
    ## speakengyes, speaks only english     71.6111     9.2904   7.708 1.31e-14 ***
    ## speakengyes, speaks very well        55.3484     9.3426   5.924 3.17e-09 ***
    ## speakengyes, speaks well             43.7736     9.5653   4.576 4.75e-06 ***
    ## billburden                          -10.6689     0.4240 -25.161  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 13671.03)
    ## 
    ##     Null deviance: 635483188  on 35127  degrees of freedom
    ## Residual deviance: 478540798  on 35004  degrees of freedom
    ## AIC: 434340
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
library(rsq)
rsq(gauss1,adj=T); rsq(gauss2,adj=T); rsq(gauss3,adj=T); rsq(gauss4,adj=T); rsq(gauss5,adj=T); rsq(gauss6,adj=T)
```

    ## [1] 0.1079741

    ## [1] 0.1633698

    ## [1] 0.2140301

    ## [1] 0.226682

    ## [1] 0.2306742

    ## [1] 0.2443194

``` r
anova(gauss1,gauss2,gauss3,gauss4,gauss5,gauss6,test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: poverty ~ educ
    ## Model 2: poverty ~ educ + famsize
    ## Model 3: poverty ~ educ + famsize + uhrswork
    ## Model 4: poverty ~ educ + famsize + uhrswork + race
    ## Model 5: poverty ~ educ + famsize + uhrswork + race + speakeng
    ## Model 6: poverty ~ educ + famsize + uhrswork + race + speakeng + billburden
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1     35117  566706062                          
    ## 2     35105  531331429 12 35374633 < 2.2e-16 ***
    ## 3     35017  497906550 88 33424880 < 2.2e-16 ***
    ## 4     35009  489779731  8  8126818 < 2.2e-16 ***
    ## 5     35005  487195653  4  2584078 < 2.2e-16 ***
    ## 6     35004  478540798  1  8654854 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
