library(sparklyr)
options(rsparkling.sparklingwater.version = "1.6.8")

library(rsparkling)
library(h2o)
library(tidyverse)

#
sc <- spark_connect("local", version = "1.6.2")

### PCA ----------------
# all_df %>% 
#   select(starts_with("V", ig = F)) %>% 
#   prcomp(center = T, scale. = T) -> tmp
# 
# tmp$x %>% as_tibble() %>% 
#   bind_cols(all_df, .) -> all_df


all_df <- read_rds("objects/all_df.rds")
all_df = left_join(tmp %>% select(num_retweet, diff_point), all_df, by = "num_retweet")


all_tbl <- copy_to(sc,
                   all_df,
                   "all_df",
                   overwrite = T)

# transform our data set, and then partition into 'training', 'test'
partitions <- all_tbl %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

training <- as_h2o_frame(sc, partitions$training, strict_version_check = FALSE)
test <- as_h2o_frame(sc, partitions$test, strict_version_check = FALSE)
all_h2o = as_h2o_frame(sc, all_tbl, strict_version_check = F)

# fit a linear model to the training dataset
xvar <- c(paste0("V", 1:75), "diff_point")

glm_model <- h2o.glm(x = xvar, 
                     y = "num_retweet", 
                     training_frame = training,
                     lambda_search = TRUE,
                     family = "poisson",
                     validation_frame = test,
                     nfolds = 2
                     )
print(glm_model)

dp_model <- h2o.deeplearning(x = xvar[-1], 
                 y = "diff_point", 
                 training_frame = training,
                 validation_frame = training,
                 nfolds = 2,
                 activation = "Maxout",
                 hidden = c(400, 400),
                 
                 )
print(dp_model)

gbm_model <- h2o.gbm(x = xvar,
  y = "num_retweet", 
  training_frame = training,
  validation_frame = training)
print(gbm_model)


# compute predicted values on our test dataset
pred <- h2o.predict(dp_model, newdata = test)
# convert from H2O Frame to Spark DataFrame
predicted <- as_spark_dataframe(sc, pred, strict_version_check = FALSE)

# extract the true 'mpg' values from our test dataset
actual <- partitions$test %>%
  select(num_retweet) %>%
  collect() %>%
  `[[`("num_retweet")

# produce a data.frame housing our predicted + actual 'mpg' values
data <- data.frame(
  predicted = predicted,
  actual    = actual
)
# a bug in data.frame does not set colnames properly; reset here 
names(data) <- c("predicted", "actual")

# plot predicted vs. actual values
ggplot(data, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Actual number of retweet",
    y = "Predicted number of retweet",
    title = "bs ponta retweet"
  ) + 
  coord_flip()

ggplot(all_df) + geom_line(aes(as.integer(e_day), num_retweet))

##
y_hat = with(data, mean(actual))
 with(data, mean(predicted))

tss = with(data, sum((actual - y_hat)^2))
ess = with(data, sum((predicted - y_hat)^2))
ess/tss
with(data, actual - predicted) %>% sd


##
## 試合前、後の画像が交互に投稿されるので、
## ２階の自己相関が強い
acf(all_df$num_retweet)
lm(num_retweet ~ lag(num_retweet, 1), all_df) %>% summary
lm(num_retweet ~ lag(num_retweet, 2), all_df) %>% summary



