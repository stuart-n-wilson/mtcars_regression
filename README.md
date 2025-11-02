# Exploring different linear regression models using the mtcars dataset

## Introduction

The [mtcars dataset](https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars) in R contains information about 32 cars with such things as their horsepower (hp), weight (wt) and miles per gallon (mpg) amongst others. My aim was to create a linear regression model to predict the mpg for a given car, and to compare how different models would compare in accuracy.

## The models

### Prep work

As is the case in all model building, the data had to be imported and here I chose to set a random seed, so that this method is fully reproducible. Additionally, I chose an 80-20 training-test data split as is standard.

```R
library(tidyverse)
data(mtcars)

df <- mtcars

# REGRESSION MODEL TO PREDICT MPG

# split data into test train

# set seed for reproducability
set.seed(100)

# 80:20 split, use floor function to ensure integer result
train_indices = sample(1 : nrow(df), size = floor(0.8 * nrow(df)))

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]
```

### All variable model

This first model used all variables in the dataframe to predict mpg. 

```R
# create model using all variables
mpg_model <- lm(
  mpg ~ .,
  data = train_df
)
```
Calling the summary() function in R on this model provides insights into the model and produces the following:
```R
  > summary(mpg_model)
  
  Call:
  lm(formula = mpg ~ ., data = train_df)
  
  Residuals:
      Min      1Q  Median      3Q     Max 
  -2.8364 -1.7153 -0.0079  1.1297  4.7578 
  
  Coefficients:
              Estimate Std. Error t value Pr(>|t|)
  (Intercept) -2.90575   33.12883  -0.088    0.931
  cyl         -0.43502    1.03851  -0.419    0.682
  disp         0.01303    0.02084   0.626    0.542
  hp          -0.02034    0.02541  -0.800    0.437
  drat         1.92591    1.80583   1.066    0.304
  wt          -2.74100    2.38984  -1.147    0.271
  qsec         1.48482    1.50257   0.988    0.340
  vs          -1.45215    2.88138  -0.504    0.622
  am           3.20406    2.26356   1.415    0.179
  gear        -0.01280    1.68003  -0.008    0.994
  carb         0.16705    0.90571   0.184    0.856
  
  Residual standard error: 2.454 on 14 degrees of freedom
  Multiple R-squared:  0.8813,	Adjusted R-squared:  0.7965 
  F-statistic: 10.39 on 10 and 14 DF,  p-value: 7.076e-05
```
On inspection of the coefficients table, it was clear that many of the coefficients were very close to zero. This suggests that they may be having a neglible effect on the model (with the caveat of some collinearity) and so I thought to see how a step regression approach would work for this model.

### Step regression model
