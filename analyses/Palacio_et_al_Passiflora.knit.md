---
title: "On the adequacy of fruit removal as a proxy for fitness in studies of animal-mediated phenotypic selection"
subtitle: "Supplementary information"
author: 'Code author: Facundo X. Palacio'
date: "2022-03-04"
output: 
  tufte::tufte_html:
    toc: true
---

\ 

This document presents an overview of the analyses in the main text. It highlights the essential R code used to conduct these analyses, but does not display code related to table and plot aesthetics. The code for this document is provided in **Palacio_et_al_Passiflora.Rmd** and is available at https://github.com/facuxpalacio/Passiflora_fitness. 





```r
# Load required libraries.
library(lme4) # Fitting generalized linear mixed models
library(lmerTest) # P-values for GLMMs
library(MuMIn) # Computing R2
library(MVN) # Assessing multivariate normality
library(car) # Computing variance inflation factors
library(boot) # Bootstrapping
library(ggplot2) # Plotting
library(ggdist) # Plotting
library(dplyr) # Managing data
library(reshape2) # Managing data
library(relaimpo) # Assessing relative importance of predictors
library(mgcv) # Fitting generalized additive models
```

## Predict the number of seeds removed from fruit diameter


```r
data <- read.table("../data/Passiflora_fruits.txt", head = TRUE)
intact_fruits <- data %>% filter(aspect == "intact")
m1 <- glmer.nb(seedn_obs ~ diameter_mm + (1|idpopulation/idplant), data = intact_fruits)
r.squaredGLMM(m1)
```

```
##                 R2m       R2c
## delta     0.4302785 0.8391902
## lognormal 0.4335489 0.8455685
## trigamma  0.4267324 0.8322741
```

```r
pecked_fruits <- data %>% filter(aspect == "slightly_pecked"| aspect == "highly_pecked")
# Check names (with pecked fruits and without intact fruits)
pecked2 <- pecked_fruits[pecked_fruits$idplant %in% intact_fruits$idplant, ]

# Predictions from the GLMM
pred.pecked <- data.frame(idfruit_visit = pecked2$idfruit_visit, 
                          y = predict(m1, newdata = pecked2, type = "response"))
```

\

## Data summary


```r
data %>% filter(aspect == "intact") %>%
group_by(idpopulation) %>% 
summarise(xseedn = round(mean(seedn_obs, na.rm = TRUE), 2),
          xdiam = round(mean(diameter_mm, na.rm = TRUE), 2),
          xmass = round(mean(mass_g, na.rm = TRUE), 2),
          cvdiam = round(100*sd(diameter_mm, na.rm = TRUE)/mean(diameter_mm, na.rm = TRUE), 2),
          cvmass = round(100*sd(mass_g, na.rm = TRUE)/mean(mass_g, na.rm = TRUE), 2),
          cvseedn = round(100*sd(seedn_obs, na.rm = TRUE)/mean(seedn_obs, na.rm = TRUE), 2),
          sddiam = round(sd(diameter_mm, na.rm = TRUE), 2),
          sdmass = round(sd(mass_g, na.rm = TRUE), 2),
          sdseedn = round(sd(seedn_obs, na.rm = TRUE), 2),
          n = n()) %>% knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> idpopulation </th>
   <th style="text-align:right;"> xseedn </th>
   <th style="text-align:right;"> xdiam </th>
   <th style="text-align:right;"> xmass </th>
   <th style="text-align:right;"> cvdiam </th>
   <th style="text-align:right;"> cvmass </th>
   <th style="text-align:right;"> cvseedn </th>
   <th style="text-align:right;"> sddiam </th>
   <th style="text-align:right;"> sdmass </th>
   <th style="text-align:right;"> sdseedn </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> La_Plata </td>
   <td style="text-align:right;"> 152.72 </td>
   <td style="text-align:right;"> 32.16 </td>
   <td style="text-align:right;"> 12.07 </td>
   <td style="text-align:right;"> 18.56 </td>
   <td style="text-align:right;"> 48.48 </td>
   <td style="text-align:right;"> 52.20 </td>
   <td style="text-align:right;"> 5.97 </td>
   <td style="text-align:right;"> 5.85 </td>
   <td style="text-align:right;"> 79.73 </td>
   <td style="text-align:right;"> 114 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa_Fe </td>
   <td style="text-align:right;"> 197.06 </td>
   <td style="text-align:right;"> 38.29 </td>
   <td style="text-align:right;"> 19.07 </td>
   <td style="text-align:right;"> 11.12 </td>
   <td style="text-align:right;"> 30.46 </td>
   <td style="text-align:right;"> 33.77 </td>
   <td style="text-align:right;"> 4.26 </td>
   <td style="text-align:right;"> 5.81 </td>
   <td style="text-align:right;"> 66.55 </td>
   <td style="text-align:right;"> 143 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tucumßn </td>
   <td style="text-align:right;"> 104.94 </td>
   <td style="text-align:right;"> 32.47 </td>
   <td style="text-align:right;"> 10.27 </td>
   <td style="text-align:right;"> 21.46 </td>
   <td style="text-align:right;"> 63.22 </td>
   <td style="text-align:right;"> 69.63 </td>
   <td style="text-align:right;"> 6.97 </td>
   <td style="text-align:right;"> 6.50 </td>
   <td style="text-align:right;"> 73.07 </td>
   <td style="text-align:right;"> 141 </td>
  </tr>
</tbody>
</table>

Number of seeds removed  


```r
data %>% filter(aspect == "slightly_pecked"| aspect == "highly_pecked") %>%
group_by(idpopulation) %>%
summarise(xseedn_rem = round(mean(seedn_rem, na.rm = TRUE), 2),
          sdseedn_rem = round(sd(seedn_rem, na.rm = TRUE), 2)) %>% 
knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> idpopulation </th>
   <th style="text-align:right;"> xseedn_rem </th>
   <th style="text-align:right;"> sdseedn_rem </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> La_Plata </td>
   <td style="text-align:right;"> 71.44 </td>
   <td style="text-align:right;"> 64.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa_Fe </td>
   <td style="text-align:right;"> 168.35 </td>
   <td style="text-align:right;"> 183.24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tucumßn </td>
   <td style="text-align:right;"> 59.48 </td>
   <td style="text-align:right;"> 50.38 </td>
  </tr>
</tbody>
</table>

Fruit diameter comparison between intact and pecked fruits


```r
data$aspect2 <- ifelse(data$aspect == "slightly_pecked"|data$aspect == "highly_pecked",
                        "Pecked", "Intact")

ggplot(data = data, aes(x = aspect2, y = diameter_mm, col = aspect2)) + 
  stat_halfeye(aes(fill = data$aspect2),adjust = 0.5, width = 0.7, .width = 0,
               justification = -0.2, alpha = 0.5) + 
  geom_boxplot(width = 0.2, outlier.shape = NA) + 
  geom_jitter(width = 0.05, alpha = 0.3) + facet_wrap(~idpopulation) + 
  theme_bw() + xlab("Fruit aspect") + ylab("Diameter (mm)")
```

<img src="Palacio_et_al_Passiflora_files/figure-html/raincloud plot-1.png" width="672"  />

## Fruit number-seed number trade-off


```r
data <- read.table("../data/Passiflora_selection_analysis.txt", header = TRUE)
m2 <- lmer(xseedn ~ log(fruit.crop) + (1|idpopulation), data = data)
summary(m2)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: xseedn ~ log(fruit.crop) + (1 | idpopulation)
##    Data: data
## 
## REML criterion at convergence: 783.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.16775 -0.60151  0.03794  0.58546  2.67658 
## 
## Random effects:
##  Groups       Name        Variance Std.Dev.
##  idpopulation (Intercept) 1980     44.50   
##  Residual                 2526     50.26   
## Number of obs: 74, groups:  idpopulation, 3
## 
## Fixed effects:
##                 Estimate Std. Error      df t value Pr(>|t|)   
## (Intercept)      141.724     32.164   4.337   4.406  0.00969 **
## log(fruit.crop)    2.754      5.288  71.060   0.521  0.60421   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## lg(frt.crp) -0.573
```

```r
r.squaredGLMM(m2) %>% round(., 3) %>% 
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> R2m </th>
   <th style="text-align:right;"> R2c </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.441 </td>
  </tr>
</tbody>
</table>

\

## Fruit crop size-fruit removal correlation


```r
passi <- read.table("../data/Passiflora_plant_monitoring.txt", head = TRUE)
m.corr1 <- glmer.nb(npecked_fruits ~ log(fruit_crop + 1) + (1|idpopulation/idplant) + (1|visit_number), data = passi)
summary(m.corr1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(2.7426)  ( log )
## Formula: npecked_fruits ~ log(fruit_crop + 1) + (1 | idpopulation/idplant) +  
##     (1 | visit_number)
##    Data: passi
## 
##      AIC      BIC   logLik deviance df.resid 
##   1118.7   1138.8   -553.4   1106.7      204 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3989 -0.6586 -0.2419  0.3607  3.5409 
## 
## Random effects:
##  Groups               Name        Variance Std.Dev.
##  idplant:idpopulation (Intercept) 0.2645   0.5143  
##  idpopulation         (Intercept) 0.4235   0.6508  
##  visit_number         (Intercept) 0.0805   0.2837  
## Number of obs: 210, groups:  
## idplant:idpopulation, 74; idpopulation, 3; visit_number, 3
## 
## Fixed effects:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -0.05452    0.44407  -0.123    0.902    
## log(fruit_crop + 1)  0.72055    0.07084  10.172   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## lg(frt_c+1) -0.323
```

```r
r.squaredGLMM(m.corr1) %>% round(., 3) %>%
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> R2m </th>
   <th style="text-align:right;"> R2c </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> delta </td>
   <td style="text-align:right;"> 0.404 </td>
   <td style="text-align:right;"> 0.750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lognormal </td>
   <td style="text-align:right;"> 0.426 </td>
   <td style="text-align:right;"> 0.791 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trigamma </td>
   <td style="text-align:right;"> 0.374 </td>
   <td style="text-align:right;"> 0.694 </td>
  </tr>
</tbody>
</table>

\

## Fruit removal-seed removal correlation


```r
passi$visit_number <- as.factor(passi$visit_number)
visits23 <- passi %>% filter(visit_number == c(2, 3))
m.corr2 <- glmer.nb(seed_rem ~ log(npecked_fruits + 1) + visit_number + (1|idpopulation/idplant), data = visits23)
summary(m.corr2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(0.1709)  ( log )
## Formula: 
## seed_rem ~ log(npecked_fruits + 1) + visit_number + (1 | idpopulation/idplant)
##    Data: visits23
## 
##      AIC      BIC   logLik deviance df.resid 
##    734.9    748.4   -361.5    722.9       64 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -0.41342 -0.41150 -0.33461  0.05576  2.60613 
## 
## Random effects:
##  Groups               Name        Variance  Std.Dev. 
##  idplant:idpopulation (Intercept) 1.264e-13 3.555e-07
##  idpopulation         (Intercept) 1.853e-13 4.304e-07
## Number of obs: 70, groups:  idplant:idpopulation, 38; idpopulation, 3
## 
## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.8924     0.8626   2.194   0.0282 *  
## log(npecked_fruits + 1)   1.9921     0.4134   4.818 1.45e-06 ***
## visit_number3             0.3400     0.6440   0.528   0.5975    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) l(_+1)
## lg(npck_+1) -0.890       
## visit_nmbr3 -0.662  0.431
## convergence code: 0
## boundary (singular) fit: see ?isSingular
```

```r
r.squaredGLMM(m.corr2) %>% round(., 3) %>%
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> R2m </th>
   <th style="text-align:right;"> R2c </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> delta </td>
   <td style="text-align:right;"> 0.429 </td>
   <td style="text-align:right;"> 0.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lognormal </td>
   <td style="text-align:right;"> 0.695 </td>
   <td style="text-align:right;"> 0.695 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trigamma </td>
   <td style="text-align:right;"> 0.110 </td>
   <td style="text-align:right;"> 0.110 </td>
  </tr>
</tbody>
</table>

\

## Natural selection analyses
The workflow presented here is modified from [Palacio et al. (2019)](https://brill.com/view/journals/ijee/65/3-4/article-p130_130.xml).

Load the dataset


```r
data <- read.table("../data/Passiflora_selection_analysis.txt", header = TRUE)
lp <- subset(data, idpopulation == "La_Plata")
sf <- subset(data, idpopulation == "Santa_Fe")
tu <- subset(data, idpopulation == "Tucuman")
```

To perform a selection analysis for a given population, just replace the corresponding population object name with the new object 'data'.


```r
data <- lp
```

\

## Step 1. Relative fitness computation and phenotypic trait standardization


```r
# Fitness component 1: number of fruits pecked
data$W1 <- data$fruits.removed
data$wrel1 <- data$W1/mean(data$W1)

# Fitness component 2: mean number of seeds removed
data$W2 <- data$xseeds.removed
data$wrel2 <- data$W2/mean(data$W2)

# Total fitness: number of fruits pecked x mean number of seeds removed
data$W3 <- data$fruits.removed*data$xseeds.removed
data$wrel3 <- data$W3/mean(data$W3)

data$x1 <- data$fruit.crop
data$x2 <- data$xdiam
data$x3 <- data$xseedn

data$z1 <- (data$x1 - mean(data$x1))/sd(data$x1)
data$z2 <- (data$x2 - mean(data$x2))/sd(data$x2)
data$z3 <- (data$x3 - mean(data$x3))/sd(data$x3)
```

\

## Relative importance of fitness components
The contribution of each fitness component to total fitness was assessed by fitting a linear model between (log) total fitness and both (log) fitness components and computing the average $R^2$  for each predictor over orderings among predictors (LMG metric).


```r
total.fitness.model <- lm(log(W3 + 1) ~ log(W1 + 1) + log(W2 + 1), data = data)
calc.relimp(total.fitness.model, b = 1000, type = "lmg")@lmg %>%
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> x </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> log(W1 + 1) </td>
   <td style="text-align:right;"> 0.6832822 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> log(W2 + 1) </td>
   <td style="text-align:right;"> 0.2234077 </td>
  </tr>
</tbody>
</table>

\

## Step 2. Testing normality of phenotypic traits


```r
# Trait histograms
ggplot(data = data, aes(x = z1)) + xlim(-2, 4) +
       geom_histogram(aes(y = ..density..), col = "black", fill = "gray", binwidth = 0.5, alpha = 0.5) +  
       geom_density(col = "red", fill = "red", alpha = 0.3) + theme_classic()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-1.png" width="672"  />

```r
ggplot(data = data, aes(x = z2)) + xlim(-4, 4) +
       geom_histogram(aes(y = ..density..), col = "black", fill = "gray", binwidth = 0.5, alpha = 0.5) +  
       geom_density(col = "red", fill = "red", alpha = 0.3) + theme_classic()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-2.png" width="672"  />

```r
ggplot(data = data, aes(x = z3)) + xlim(-4, 4) +
       geom_histogram(aes(y = ..density..), col = "black", fill = "gray", binwidth = 0.5, alpha = 0.5) +  
       geom_density(col = "red", fill = "red", alpha = 0.3) + theme_classic()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-3.png" width="672"  />

```r
# Multivariate normality tests
MVN::mvn(data.frame(data$z1, data$z2), mvnTest = "hz")$multivariateNormality
```

```
##            Test       HZ   p value MVN
## 1 Henze-Zirkler 0.670611 0.1005536 YES
```

```r
MVN::mvn(data.frame(data$z1, data$z2), mvnTest = "royston")$multivariateNormality
```

```
##      Test        H      p value MVN
## 1 Royston 14.37972 0.0007534534  NO
```

```r
# Q-Q plots
qqnorm(data$z1, cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(data$z1, col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-4.png" width="672"  />

```r
qqnorm(data$z2, cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(data$z2, col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-5.png" width="672"  />

```r
qqnorm(data$z3, cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(data$z3, col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/multivariate normality-6.png" width="672"  />

\

## Step 3. Assessing collinearity of phenotypic traits


```r
# Pearson correlation between traits
data %>% dplyr::select(z1, z2, z3) %>% cor() %>% round(., 2) %>%
knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> z1 </th>
   <th style="text-align:right;"> z2 </th>
   <th style="text-align:right;"> z3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> z1 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> -0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z2 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z3 </td>
   <td style="text-align:right;"> -0.12 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
</tbody>
</table>

```r
# Variance inflation factor on (linear) Lande and Arnold's model
lin.grad1 <- lm(wrel1 ~ z1 + z2 + z3, data = data)
vif(lin.grad1) %>% round(., 2) %>%
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> x </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> z1 </td>
   <td style="text-align:right;"> 1.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z2 </td>
   <td style="text-align:right;"> 1.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z3 </td>
   <td style="text-align:right;"> 1.65 </td>
  </tr>
</tbody>
</table>

```r
summary(lin.grad1)$coeff %>% round(., 3) %>%
  knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Std. Error </th>
   <th style="text-align:right;"> t value </th>
   <th style="text-align:right;"> Pr(&gt;|t|) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.192 </td>
   <td style="text-align:right;"> 5.205 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z1 </td>
   <td style="text-align:right;"> 1.156 </td>
   <td style="text-align:right;"> 0.200 </td>
   <td style="text-align:right;"> 5.791 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z2 </td>
   <td style="text-align:right;"> -0.149 </td>
   <td style="text-align:right;"> 0.254 </td>
   <td style="text-align:right;"> -0.585 </td>
   <td style="text-align:right;"> 0.565 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z3 </td>
   <td style="text-align:right;"> 0.151 </td>
   <td style="text-align:right;"> 0.252 </td>
   <td style="text-align:right;"> 0.597 </td>
   <td style="text-align:right;"> 0.557 </td>
  </tr>
</tbody>
</table>

```r
lin.grad2 <- lm(wrel2 ~ z1 + z2 + z3, data = data)
lin.grad3 <- lm(wrel3 ~ z1 + z2 + z3, data = data)
```

\

## Step 4. Checking model residuals


```r
# Shapiro-Wilk test
shapiro.test(resid(lin.grad1))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  resid(lin.grad1)
## W = 0.9182, p-value = 0.04662
```

```r
shapiro.test(resid(lin.grad2))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  resid(lin.grad2)
## W = 0.96387, p-value = 0.4967
```

```r
shapiro.test(resid(lin.grad3))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  resid(lin.grad3)
## W = 0.97134, p-value = 0.6791
```

```r
# Q-Q plots
qqnorm(resid(lin.grad1), cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(resid(lin.grad1), col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/model residuals-1.png" width="672"  />

```r
qqnorm(resid(lin.grad2), cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(resid(lin.grad2), col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/model residuals-2.png" width="672"  />

```r
qqnorm(resid(lin.grad3), cex = 1.5, pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), main = "")
qqline(resid(lin.grad3), col = "red", lwd = 2)
```

<img src="Palacio_et_al_Passiflora_files/figure-html/model residuals-3.png" width="672"  />

\

## Step 5. Standard error and confidence interval estimation
For the function *grad*, the data must have the relative fitness in the first column and standardized variables in the remaining columns. The function returns a vector with linear, quadratic and correlational gradients, and represents the input for the function *boot*.


```r
grad <- function(data, original = c(1:nrow(data))){
  data <- data[original, ]
  vars  <- colnames(data)[-1]
  colnames(data)[1] <- "Wrel"
  model.lin <- as.formula(paste("Wrel", paste(vars, collapse=" + "), sep=" ~ "))
  m1 <- lm(formula = model.lin, data = data)
  
  part1 <- paste("(", paste(vars, collapse=" + "), ")^2", sep = "")
  part2 <- paste("I(0.5*(", vars, "^2))", sep = "", collapse = " + ")
  model.qua <- as.formula <- paste("Wrel", paste(part1, part2, sep = " + "), sep = " ~ ")
  m2 <- lm(formula = model.qua, data = data)
  
  sel.grad<-c(m1$coefficients[-1], m2$coefficients[-c(1:ncol(data))])
  return(sel.grad)
}

newdata <- data.frame(data[, c("wrel1", "z1", "z2", "z3")])
selection.gradients <- grad(data = newdata)
nsamples <- 1000
boot.grad <- boot(data = newdata, statistic = grad, R = nsamples)

# Create a list with 95% bias-corrected bootstrap confidence intervals for each gradient and p-values.
CI <- list()
for(i in 1:length(boot.grad$t0)){
  CI[[i]] <- boot.ci(boot.grad, conf = 0.95, type = "bca", index = i)$bca[4:5]
}
names(CI) <- names(boot.grad$t0)

data.frame(grad = selection.gradients,
           se = apply(boot.grad$t, 2, sd, na.rm = TRUE),
           low.CI = t(as.data.frame(CI)[1, ]),
           upp.CI = t(as.data.frame(CI)[2, ]),
           p = apply(boot.grad$t, 2, function(x) length(x[x<0])/nsamples)) %>%
  round(., 2) %>% knitr::kable(.)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> grad </th>
   <th style="text-align:right;"> se </th>
   <th style="text-align:right;"> X1 </th>
   <th style="text-align:right;"> X2 </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> z1 </td>
   <td style="text-align:right;"> 1.16 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 1.80 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z2 </td>
   <td style="text-align:right;"> -0.15 </td>
   <td style="text-align:right;"> 0.42 </td>
   <td style="text-align:right;"> -1.46 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z3 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> -0.37 </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(0.5 * (z1^2)) </td>
   <td style="text-align:right;"> -0.58 </td>
   <td style="text-align:right;"> 1.23 </td>
   <td style="text-align:right;"> -2.39 </td>
   <td style="text-align:right;"> 9.20 </td>
   <td style="text-align:right;"> 0.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(0.5 * (z2^2)) </td>
   <td style="text-align:right;"> 1.48 </td>
   <td style="text-align:right;"> 1.13 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 6.26 </td>
   <td style="text-align:right;"> 0.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(0.5 * (z3^2)) </td>
   <td style="text-align:right;"> 1.55 </td>
   <td style="text-align:right;"> 1.13 </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> 4.92 </td>
   <td style="text-align:right;"> 0.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z1:z2 </td>
   <td style="text-align:right;"> -0.68 </td>
   <td style="text-align:right;"> 1.08 </td>
   <td style="text-align:right;"> -2.65 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z1:z3 </td>
   <td style="text-align:right;"> 0.66 </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> -0.68 </td>
   <td style="text-align:right;"> 3.54 </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z2:z3 </td>
   <td style="text-align:right;"> -1.61 </td>
   <td style="text-align:right;"> 1.01 </td>
   <td style="text-align:right;"> -6.80 </td>
   <td style="text-align:right;"> -0.27 </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
</tbody>
</table>

\

## Step 6. Plotting Lande & Arnold's model results

```r
# Linear selection + bootstrap samples
# Example: fruit crop size vs number of fruits pecked
new.z1 <- seq(-1, 4, length = 100)
pred.z1 <- predict(lin.grad1, 
                   newdata = data.frame(z1 = new.z1, z2 = 0, z3 = 0))
new <- data.frame(z1 = new.z1, wrel = pred.z1)

boot.y <- as.data.frame(matrix(NA, nrow = nrow(boot.grad$t), ncol = 100))
for(i in 1:nrow(boot.grad$t)){
boot.y[i,] <- 1 + boot.grad$t[i, 1]*new.z1
}
boot.y <- as.data.frame(t(boot.y))
boot.y$z1 <- new.z1
boot.melted <- melt(boot.y, id.vars = "z1")

ggplot() + 
geom_point(data = data, aes(x = z1, y = wrel1), size = 5) +
geom_line(data = boot.melted, aes(x = z1, y = value, group = variable), col = "gray", alpha = 0.1) +
xlab("Standardized fruit crop size") + 
geom_line(data = new, aes(x = z1, y = wrel), col = "red", size = 2) +
ylab("Relative fitness (number of pecked fruits)") +
theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/L&A plots-1.png" width="672"  />

\

## Simulations of the effect of the fruit number-seed number trade-off on seed dispersal

Set simulation parameters


```r
set.seed(1001)
# Number of simulations
nsim <- 10000
# Number of plants per population
nplants <- 500
```

Simulate fruit crop size and different fruit number-seed number trade-offs. Extract model coefficients.

```r
# Fruit crop size
crop <- seq(1, 500, length = nplants)
# Assumption 1: fruit removal correlates positively with fruit crop size
rem <- rbinom(n = nplants, size = crop, prob = 0.7)

b <- seq(-0.5, 0, length = nsim) # fruit number-seed number trade-offs
l <- c()
q <- c()
x <- c()
cv <- c()

for(i in 1:nsim){
# Assumption 2: negative relationship between fruit crop size and seed number (seed size-number trade-off)
  xseeds <- rpois(n = nplants, lambda = 100*exp(b[i]*rem)) # Mean number of seeds dispersed
  fitness <- rem*xseeds # Total fitness
  m <- glm(fitness ~ rem + I(rem^2), family = poisson)
  l[i] <- coef(m)[2] # Linear effect 
  q[i] <- coef(m)[3] # Non-linear effect
  x[i] <- mean(xseeds) # Mean number of seeds dispersed
  cv[i] <- 100*sd(xseeds)/mean(xseeds) # Coefficient of variation in seed removal
}

df <- data.frame(b, l, q, x, cv) 
```

Seed size-number trade-off vs fruit removal-fitness quadratic term


```r
m1 <- gam(q ~ s(b), data = df)
df$pred1 <- predict(m1, newdata = df)
ggplot() +
  geom_point(data = df, aes(x = b, y = q), size = 3, alpha = 0.1, col = "gray") +
  geom_line(data = df, aes(x = b, y = pred1), size = 1.5, col = "darkorchid") +
  xlab(expression(paste(beta, " (seeds ~ crop)"))) + 
  ylab("Quadratic coefficient (fitness ~ fruit removal)") +
  theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/tradeoff effect-1.png" width="672"  />

Coefficient of variation in seed number vs fruit removal-fitness correlation


```r
m2 <- gam(q ~ s(cv), data = df)
df$pred2 <- predict(m2, newdata = df)
ggplot() + 
  geom_point(data = df, aes(x = cv, y = q), size = 3, alpha = 0.1, col = "gray") +
  geom_line(data = df, aes(x = cv, y = pred2), size = 1.5, col = "darkorchid") +
  xlab("CV seed number (%)") + 
  ylab("Quadratic coefficient (fitness ~ fruit removal)") +
  theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/cv-1.png" width="672"  />

Examples of the relationship between fruit removal and total fitness for different trade-off values ($\beta_i$).

$\beta$ = -0.2


```r
seeds <- rpois(n = nplants, lambda = 100*exp(-0.2*rem))
fitness <- rem*seeds
ggplot() + 
  geom_point(data = data.frame(rem, fitness), aes(x = rem, y = fitness), 
             size = 3, alpha = 0.3, col = "orange") +
  xlab("Number of fruits removed") + 
  ylab("Total fitness") + theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/beta1-1.png" width="672"  />

$\beta$ = -0.1


```r
seeds <- rpois(n = nplants, lambda = 100*exp(-0.1*rem))
fitness <- rem*seeds
ggplot() + 
  geom_point(data = data.frame(rem, fitness), aes(x = rem, y = fitness), 
             size = 3, alpha = 0.3, col = "orange") +
  xlab("Number of fruits removed") + 
  ylab("Total fitness") + theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/beta2-1.png" width="672"  />

$\beta$ = -0.01


```r
seeds <- rpois(n = nplants, lambda = 100*exp(-0.01*rem))
fitness <- rem*seeds
ggplot() + 
  geom_point(data = data.frame(rem, fitness), aes(x = rem, y = fitness), 
             size = 3, alpha = 0.3, col = "orange") +
   xlab("Number of fruits removed") + 
   ylab("Total fitness") + theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/beta3-1.png" width="672"  />

$\beta$ = -0.001


```r
seeds <- rpois(n = nplants, lambda = 100*exp(-0.001*rem))
fitness <- rem*seeds
ggplot() + 
  geom_point(data = data.frame(rem, fitness), aes(x = rem, y = fitness), 
             size = 3, alpha = 0.3, col = "orange")+
  xlab("Number of fruits removed") + 
  ylab("Total fitness") + theme_bw()
```

<img src="Palacio_et_al_Passiflora_files/figure-html/beta4-1.png" width="672"  />


