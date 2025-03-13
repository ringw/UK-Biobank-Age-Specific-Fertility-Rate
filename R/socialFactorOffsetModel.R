library(lme4)
library(mgcv)

compute_offset <- function(data, age_mm) {
  mygam <- gam(
    Y ~ 0 + age_mm + s(job_distance, bs="bs", by=job_data),
    poisson("identity"),
    data
  )
  fixed_mm <- model.matrix(mygam)
  fixed_coefs <- grep(
    "Intercept|age|job_datapresent",
    colnames(fixed_mm),
    val=T
  )
  fixed_mm <- fixed_mm[, fixed_coefs] %>%
    cbind(
      job_datadecline = data$job_data == "decline",
      job_datadonotknow = data$job_data == "donotknow"
    )
  
  myglm <- glm(
    Y ~ 0 + fixed_mm,
    poisson("identity"),
    data
  )
  
  mylmm <- lme4::lmer(
    Y ~ 0 + fixed_mm + (1 | centre),
    data,
    start = list(fixef = myglm$coefficients)
  )
  
  myglmm <- lme4::glmer(
    Y ~ 0 + fixed_mm + (1 | centre),
    data,
    poisson("identity"),
    mustart = mylmm@resp$mu,
    verbose = 2L
  )
}
