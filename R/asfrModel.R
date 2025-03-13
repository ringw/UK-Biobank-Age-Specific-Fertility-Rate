library(dplyr)
library(forcats)
library(reshape2)
library(stringr)
library(tibble)
library(tidyr)

read_cohort <- function() as_tibble(read.csv("data.csv"))
months <- c(
  January = 1,
  February = 2,
  March = 3,
  April = 4,
  May = 5,
  June = 6,
  July = 7,
  August = 8,
  September = 9,
  October = 10,
  November = 11,
  December = 12
)
centre_coding <- c(
  "11012"="Barts",
  "11021"="Birmingham",
  "11011"="Bristol",
  "11008"="Bury",
  "11003"="Cardiff",
  "11024"="Cheadle.revisit",
  "11020"="Croydon",
  "11005"="Edinburgh",
  "11004"="Glasgow",
  "11018"="Hounslow",
  "11010"="Leeds",
  "11016"="Liverpool",
  "11001"="Manchester",
  "11017"="Middlesborough",
  "11009"="Newcastle",
  "11013"="Nottingham",
  "11002"="Oxford",
  "11007"="Reading",
  "11014"="Sheffield",
  "10003"="Stockport.pilot",
  "11006"="Stoke",
  "11022"="Swansea",
  "11023"="Wrexham",
  "11025"="Cheadle.imaging",
  "11026"="Reading.imaging",
  "11027"="Newcastle.imaging",
  "11028"="Bristol.imaging"
)
process_cohort <- function(ukb) {
  Y <- ukb[str_glue("participant.p2405_i{0:3}")] %>% unlist(use.names = F)
  birth <- with(
    ukb,
    str_glue("{participant.p34}-{months[participant.p52]}-15")
  ) %>%
    as.Date()
  assessment <- unlist(
    ukb[str_glue("participant.p53_i{0:3}")],
    use.names = F
  ) %>%
    as.Date()
  centre <- unlist(
    ukb[str_glue("participant.p54_i{0:3}")],
    use.names = F
  )
  centre <- centre_coding[as.character(centre)] %>%
    replace(is.na(.), "NA")
  centre <- factor(centre) %>%
    fct_relevel("NA", after = length(levels(.)))
  job <- unlist(
    ukb[str_glue("participant.p796_i{0:3}")],
    use.names = F
  )
  job_data <- is.na(job) %>%
    ifelse(
      "missing",
      ifelse(is.na(job), F, job == -3) %>%
        ifelse(
          "decline",
          ifelse(is.na(job), F, job == -1) %>%
            ifelse(
              "donotknow",
              "present"
            )
        )
    ) %>%
    factor(c("missing", "donotknow", "decline", "present"))
  job_distance <- ifelse(
    (job > 0) %>% replace(is.na(job), FALSE),
    job,
    0
  ) %>%
    pmin(200)
  data <- tibble(
    eid = rep(ukb$participant.eid, 4),
    birth = rep(birth, 4),
    age = as.numeric(assessment - birth) / 365.25,
    Y = as.numeric(Y),
    centre,
    job_data,
    job_distance,
    ethnicity = rep(ukb$participant.p22006, 4) %>% factor(),
  ) %>%
    subset(!is.na(Y) & Y >= 0)
}

age_to_x <- \(age) setNames(
  if (age < 42)
    # f(42) = a - 3b + 3^2*c - 3^3*d
    # f'(42) = b - 2*3*c + 3*3^2*d
    # f(x) ~= f(42) + (x-42)*f'(42)
    c(1, -3, 9, 27) + c(0, 1, -6, 27) * (age - 42)
  else if (age > 78)
    # f(78) = a + 33b + 33^2*c + 33^3*d
    # f'(78) = b + 2*33*c + 3*33^2*d
    c(1, 33, 33^2, 33^3) + c(0, 1, 2*33, 3*33^2) * (age - 78)
  else
    c(1, (age - 45), (age - 45)^2, (age - 45)^3),
  poly_names
)
poly_names <- c("Intercept", "x", "x^2", "x^3")
poly_change_of_basis <- matrix(
  c(
    # f(45) = Intercept
    1,0,0,0,
    # f(x) ~= a + b(x-45), f'(45) = b
    0,1,0,0,
    # f(x) := a + b(x-45) + c(x-45)^2 + d(x-45)^3
    # f'(55) = b + 2*10*c + 3*10^2*d
    0,1,20,300,
    # f'(65) = b + 2*20*c + 3*20^2*d
    0,1,40,1200
  ),
  nrow=4,
  byrow=TRUE,
  dimnames=list(
    c("Intercept", "ASFR45", "ASFR55", "ASFR65"),
    poly_names
  )
)
model_asfr <- function(age) {
  poly_names <- c("Intercept", "x", "x^2", "x^3")

  mm = t(sapply(age, age_to_x))
  mm2 <- t(solve(t(poly_change_of_basis), t(mm)))
  mm2 <- mm2 %*% diag(c(1, 1e-3, 1e-3, 1e-3))
  colnames(mm2) <- rownames(poly_change_of_basis)
  mm2 <- mm2
}
