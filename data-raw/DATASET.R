## code to prepare `DATASET` dataset goes here

set.seed(1234)

Cont_Var_1 <- rnorm(100)
Cont_var_2 <- rnorm(100, mean=5, sd=12)

n <- 100
Cat_var_1 <- sample(c(0,1), replace = TRUE, size=n)

set.seed(4321)
Cat_var_2 <- sample(c(0,1), replace = TRUE, size=n)

sample_data <- data_frame(Cont_Var_1, Cont_var_2, Cat_var_1, Cat_var_2)

usethis::use_data(sample_data, overwrite = TRUE)

