# everything in body of function is something we create or user gives us

new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x)/n
	return(mean_val)
}

x <- c(10, 15, 20, 25, 30)

new_mean(x = c(34, 222, 90))
mean(x = c(34, 222, 90))

# everything in function(___) is something we need the user to give to us
prop <- function(x, multiplier) {
	n <- length(x)
	proportion_val <- sum(x)/n
	multiplied_val <- multiplier*proportion_val
	return(multiplied_val)
}

prop(c(1, 1, 1, 0, 0), multiplier = 1)
prop(c(1, 1, 1, 0, 0), multiplier = 100)


# start out with a number to test
y <- 3
# you'll want your function to return this number
y^2

square <- function(y) {
	sq <- y*y
	return(sq)
}

# test it out
square(y)
square(y=53)

53^2 # does this match?

square(y=389)
389^2



raise <- function(x, power) {
	answer <- x^power
	return(answer)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4


# test
raise(x = 5, power = 2)
# should give you
5^2

raise(3, 4)
3^4



library(tidyverse)
library(gtsummary)
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial())

poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson())

logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log"))

tbl_regression <- tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)
tbl_regression

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		)
	)
}


new_table_function <- function(model, tidy_fun = broom.helpers::tidy_with_broom_or_parameters) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		),
		tidy_fun = tidy_fun
	)
}




