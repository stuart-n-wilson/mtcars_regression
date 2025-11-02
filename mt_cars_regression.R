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

# create model using all variables
mpg_model <- lm(
  mpg ~ .,
  data = train_df
)


# use model on test data, show 95% confidence intervals
predict(
  mpg_model,
  newdata = test_df,
  interval = "confidence"
)


# summary shows strong R-squared value, and low overall p value,
# but the individuals predictors do not have significant p values.
# some coeffs are also close to 0, so step-wise regression is good idea.

# create intercept only model
mpg_intercept_only <- lm(mpg ~ 1, data = train_df)

# perform forward step-wise regression
forward_mpg <- step(mpg_intercept_only,
                direction = "forward",
                scope = formula(mpg_model),
                trace = 0)

# look at AIC decrease my introducing new variables
forward_mpg$anova

# predict with forward_mpg model
predict(
  forward_mpg,
  newdata = test_df,
)

# model just using wt (weight) variable
wt_mpg <- lm(mpg ~ wt, data = train_df)
summary(wt_mpg)

# model just using cyl (cylinder) variable
cyl_mpg <- lm(mpg ~ cyl, data = train_df)
summary(cyl_mpg)


# create df to compare the two models, turn row names into a col, sort by mpg
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
    caption = "Data Source: mpg dataset | Plot by Stuart Wilson ",
    x = "Car model",
    y = "Miles per Gallon (mpg)"
  ) +
  
  # faceting
  facet_wrap(~ model_prediction,
             labeller = labeller(model_prediction = facet_labels)) 



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
    caption = "Data Source: mpg dataset | Plot by Stuart Wilson ",
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



