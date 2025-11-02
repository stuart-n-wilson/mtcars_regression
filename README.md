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

To create the step regression, I began with an intercept online model, and then used a forward regression to add in variables. 

```R
# create intercept only model
mpg_intercept_only <- lm(mpg ~ 1, data = train_df)

# perform forward step-wise regression
forward_mpg <- step(mpg_intercept_only,
                direction = "forward",
                scope = formula(mpg_model),
                trace = 0)
```

To visualise this, we can look at how the AIC (Akaike Information Criterion) decreases with different variables being introduced. This measures the models improved but penalises complexity so is ideal for step regression analysis.

```R
> forward_mpg$anova
   Step Df  Deviance Resid. Df Resid. Dev      AIC
1       NA        NA        24  710.43040 85.67488
2 + cyl -1 548.46720        23  161.96320 50.71233
3  + wt -1  36.86974        22  125.09346 46.25463
4  + hp -1  10.42416        21  114.66930 46.07941
5  + am -1  16.65914        20   98.01016 44.15488
```
We see that the impact of introducing the cylinder variable is highly signifcant followed by the weight variable. This is the reason I decided to add cylinder only and weight only regression models to my comparison.

### Cylinder only and weight only models

```R
# model just using wt (weight) variable
wt_mpg <- lm(mpg ~ wt, data = train_df)
summary(wt_mpg)

# model just using cyl (cylinder) variable
cyl_mpg <- lm(mpg ~ cyl, data = train_df)
summary(cyl_mpg)

```

## Model comparison

### Data wrangling

Here I created a new comparison dataframe, and used my test data to add a prediction column and a residue column for each model. The residue is simply calculated as the true value subtract the prediction.
```R
# create df to compare the models, turn row names into a col
compare_df <- test_df |>
  mutate(
    forward_predict = predict(forward_mpg, newdata = test_df),
    forward_resid = mpg - forward_predict,
    all_predict = predict(mpg_model, newdata = test_df),
    all_resid = mpg - all_predict,
    wt_predict = predict(wt_mpg, newdata = test_df),
    wt_resid = mpg - wt_predict,
    cyl_predict = predict(cyl_mpg, newdata = test_df),
    cyl_resid = mpg - cyl_predict
  ) |> 
  rownames_to_column("car")
```


