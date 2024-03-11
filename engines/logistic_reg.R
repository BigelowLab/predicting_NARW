# tuned
# model_spec <- logistic_reg(penalty = .0002, mixture = 1) |>
#   set_engine("glmnet")

model_spec <- logistic_reg(mode = "classification") |>
  set_engine("glm", family = stats::binomial(link = "logit"))