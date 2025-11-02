# Exploring and comparing different linear regression models using the mtcars dataset

## Introduction

The [mtcars dataset](https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars) in R contains information about 32 cars with such things as their horsepower (hp), weight (wt) and miles per gallon (mpg) amongst others. My aim was to create a linear regression model to predict the mpg for a given car, and to compare these different models.

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

To visualise this, we can look at how the AIC (Akaike Information Criterion) decreases with different variables being introduced. This measures the models improvement but penalises complexity so is ideal for step regression analysis.

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
I then furthered this by creating a second, tidier dataframe where each row had a prediction and residue, along with the model used for this prediction. This made the following plots considerably easier to create.

```R
# create tidier df for plotting by combining predictions and residuals
compare_df_tidy <- compare_df |> 
  pivot_longer(
    cols = c("forward_predict", "all_predict", "wt_predict", "cyl_predict"),
    names_to = "model_prediction",
    values_to = "prediction"
  ) |> 
  pivot_longer(
    cols = c("forward_resid", "all_resid", "wt_resid", "cyl_resid"),
    names_to = "model_residue",
    values_to = "residue"
  )

# sort df so that models on x axis are in increasing mpg order (purely for
# aesthetics)

# Ensure factor levels are unique car names ordered by actual mpg
compare_df_tidy$car <- factor(
  compare_df_tidy$car,
  levels = compare_df$car[order(compare_df$mpg)]
)
```

### The plots

For the first comparison, I wanted to show each models prediction compared with the true value, side by side. I decided that a facetted appraoch suited this, with line segments between the two values to emphasis the residual.

```R
# plot model comparisons as facet
facet_labels <- c(
  "forward_predict" = "Forward Step model",
  "all_predict" = "All Variables Model",
  "wt_predict" = "Weight Only Model",
  "cyl_predict" = "Cylinders Only Model"
)

ggplot(
  compare_df_tidy,
  aes(x = car, y = mpg)
) +
  
  # custom colour palette
  scale_colour_manual(
    # map old factor levels to new names
    values = c(
      "forward_predict" = "#003f5c",
      "all_predict" = "#7a5195",
      "wt_predict" = "#ef5675",
      "cyl_predict" = "#ffa600"
      ),
    labels = c(
      "forward_predict" = "Forward Step Model",
      "all_predict" = "All Variables Model",
      "wt_predict" = "Weight Only",
      "cyl_predict" = "Cylinders Only"
    )) +
    
  # vertical line segments between true and predicted values
  geom_segment( 
    aes(xend = car, yend = prediction),
    alpha = 0.9,
    colour = "grey"
  ) +
  
  # true values
  geom_point(size = 5, shape = 18) +
  
  # predicted values
  geom_point(
    aes(x = car, y = prediction, colour = model_prediction),
    size = 3,
    show.legend = FALSE
  ) +
  
  # theme adjustments
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # labels
  labs(
    title = "mpg predictions vs true value using different linear regression models",
    caption = "Data Source: mtcars dataset | Plot by Stuart Wilson ",
    x = "Car model",
    y = "Miles per Gallon (mpg)"
  ) +
  
  # faceting
  facet_wrap(~ model_prediction,
             labeller = labeller(model_prediction = facet_labels)) 
```

This produces this set of scatterplots.

<img width="1091" height="759" alt="image" src="https://github.com/user-attachments/assets/50cde8e4-db22-4dd3-8819-a9b50154cda4" />

The second comparison I wanted to visualise, was between the residues. 

```R
# plot boxplot to show residual distribution
ggplot(
  compare_df_tidy,
  aes(x = model_residue, y = residue, colour = model_residue)
) +
  
  # add whisker ends
  stat_boxplot(lwd = 1, 
               geom = "errorbar", 
               width = 0.2,
               show.legend = FALSE) +  
  
  
  geom_boxplot(lwd = 1,
               outlier.shape = 16,
               show.legend = FALSE) +
  
  # centre 0 on y axis
  ylim(-8, 8) +

  # dashed line y = 0
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey",
             alpha = 0.9,
             show.legend = FALSE) +
  
  # custom colour palette
  scale_colour_manual(
    values = c(
      "forward_resid" = "#003f5c",
      "all_resid" = "#7a5195",
      "wt_resid" = "#ef5675",
      "cyl_resid" = "#ffa600"
    )) +
  
  theme_minimal() + 
  
  # labels
  labs(
    title = "Residual comparisons between linear regression models",
    caption = "Data Source: mtcars dataset | Plot by Stuart Wilson ",
    x = "Model",
    y = "Residue"
  ) +
  
  # rename the x labels
  scale_x_discrete(
    labels = c(
      "forward_resid" = "Forward Step",
      "all_resid" = "All",
      "wt_resid" = "Weight",
      "cyl_resid" = "Cylinder"
    ))
```
<img width="1091" height="759" alt="image" src="https://github.com/user-attachments/assets/5c80bca1-1aa0-488f-b9e5-81073f1f59b1" />

This comparison shows that the residuals for the forward step model and weight only model are minimised, with their median close to zero. The median residue of the cylinder only model is also close to zero but ths spread of the residues is much greater. Interestingly, the worst performer in terms of residues is the all variables model, with the median residue far from zero. 

We can see how this corresponds to the R-squared value for each model.

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

```R
> summary(forward_mpg)

Call:
lm(formula = mpg ~ cyl + wt + hp + am, data = train_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.3218 -1.6508 -0.2954  1.1237  5.3159 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 32.30903    3.35044   9.643 5.81e-09 ***
cyl         -0.83830    0.62779  -1.335   0.1968    
wt          -1.19055    0.99458  -1.197   0.2453    
hp          -0.03035    0.01352  -2.245   0.0362 *  
am           2.75050    1.49178   1.844   0.0801 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.214 on 20 degrees of freedom
Multiple R-squared:  0.862,	Adjusted R-squared:  0.8344 
F-statistic: 31.24 on 4 and 20 DF,  p-value: 2.403e-08
```
```R
> summary(cyl_mpg)

Call:
lm(formula = mpg ~ cyl, data = train_df)

Residuals:
   Min     1Q Median     3Q    Max 
-4.556 -1.596  0.044  0.924  6.404 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  37.0360     2.0707  17.886 5.44e-15 ***
cyl          -2.7600     0.3127  -8.825 7.64e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.654 on 23 degrees of freedom
Multiple R-squared:  0.772,	Adjusted R-squared:  0.7621 
F-statistic: 77.89 on 1 and 23 DF,  p-value: 7.644e-09
```
```R
> summary(wt_mpg)

Call:
lm(formula = mpg ~ wt, data = train_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.1908 -2.2781 -0.4501  1.6074  7.5901 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  35.7399     2.3473  15.226 1.67e-13 ***
wt           -4.9681     0.6876  -7.225 2.35e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.074 on 23 degrees of freedom
Multiple R-squared:  0.6942,	Adjusted R-squared:  0.6809 
F-statistic: 52.21 on 1 and 23 DF,  p-value: 2.351e-07
```

We see that the forward step model produces the greatest adjusted R-squared value of the models but all of the models have a reasonable R-squared value.

## Conclusion

Through building and analysing these models, the personal learnings I take away from this is that a more complex model is not necessarily a better one. As we have seen, a model can be created using just one factor to predict some outcome with reasonable accuracy, and this model is far more understandable and reproducible from a non-technical viewpoint. This is something I intend to keep in the back of my mind when creating new models going forwards.

*********************
Thank you for reading.
