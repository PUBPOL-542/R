
```{r}
# The research question I explored in this RMD file is “What is the association between poverty and educational level attainment in The State of Washington?” I used The 2021 American Community Survey (ACS) dataset for Washington State Residents. I cleaned the Data before I ran the regression. I limited the data to individuals who are 18 years old or older and are employed. The dependent variable in these regression analyses is the poverty level ( Which “expresses each family's total income for the previous year as a percentage of the poverty thresholds”). The independent variable is the educational attainment level for the individual with subsequent regressions controlling for family size, hours worked per week, race, Whether the individual speaks English and the annual gas and water bill burden of for households. The regression showed there is a positive association between having a high school diploma or less and being under the poverty threshold controlling for family size, hours worked per week, race, whether a person speaks English and bills burden. The results are statistically significant at the 0.001 and 0.05 levels. R2 started at 0.107974 and reached 0.2443194 after using all the controls in the regression. Despite the fact that R2 was not very high, the ANOVA test showed that the results of the regression kept getting stronger as we added all the controls.  
```


```{r}
 
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



```{r}
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




```{r}
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



```{r}
summary(gauss1)
summary(gauss2)
summary(gauss3)
summary(gauss4)
summary(gauss5)
summary(gauss6)

```
           


```{r}
library(rsq)
rsq(gauss1,adj=T); rsq(gauss2,adj=T); rsq(gauss3,adj=T); rsq(gauss4,adj=T); rsq(gauss5,adj=T); rsq(gauss6,adj=T)
```


           
```{r}
anova(gauss1,gauss2,gauss3,gauss4,gauss5,gauss6,test="Chisq")
```
           
