# install.packages("readxl")
library("readxl")
data <- read_excel("Capstone Data (Autosaved).xlsx", sheet = "Toyota")
auction_info <- read_excel("Auction Locations.xlsx")

# Removes rows with NA values (excluding Transmission since it won't be used
# in these models anyway - they're all automatic)

data$region <- auction_info$region[match(data$auction_code, auction_info$auction_code)]
data$city <- auction_info$city[match(data$auction_code, auction_info$auction_code)]

data_nona <- data[complete.cases(data[, c(1:8, 10:17)]),]


# install.packages("data.table")
library(data.table)
prius <- data_nona[data_nona$model %like% "PRIUS",]
avalon <- data_nona[data_nona$model %like% "AVALON",]

# Prius Cleaning

prius$body <- as.factor(prius$body)
#All Priuses seem to be hatchbacks (either 4 or 5 door)
prius$body <- as.character(prius$body)


#Split up body types into either 4 door or 5 door hatchbacks
#Going to go back later to remove rows that have incomplete or missing data
for (i in 1:length(prius$body)) { 
    if (is.na(prius$body[i])) {
      prius$body[i] <- NA
  } else if (prius$body[i] == "5DHB") {
    prius$body[i] <- "5 DOOR"
  } else if (prius$body[i] == "NONE") {
    prius$body[i] <- NA
  } else {
    prius$body[i] <- "4 DOOR"
  }
}

prius$subseries <- as.factor(prius$subseries)
# install.packages("forcats")
library(forcats)
prius$subseries <- fct_collapse(prius$subseries, ONE = c("1", "BASE", "ONE", "I"))
prius$subseries <- fct_collapse(prius$subseries, TWO = c("2", "TWO", "II", "TWO ECO"))
prius$subseries <- fct_collapse(prius$subseries, THREE = c("3", "III", "THREE"))
prius$subseries <- fct_collapse(prius$subseries, FOUR = c("4", "IV", "FOUR"))
prius$subseries <- fct_collapse(prius$subseries, FIVE = c("5", "V", "FIVE"))
prius$subseries <- fct_collapse(prius$subseries, TOURING = c("TOURING",
                                  "3 TOURING", "4 TOURING"))
prius$subseries <- fct_collapse(prius$subseries, SPECIAL_EDIT = c("ADVANCED", 
                  "PERSONA", "PLUS", "PREMIUM", "SPECIAL ED"))
prius$subseries <- fct_collapse(prius$subseries, DROPPED = c("(NATL)", "C",
                                                  "HY", "HYBRID", "NONE"))

# Drop the rows that have missing or insufficient information
for (i in 1:length(prius$subseries)) {
  if (prius$subseries[i] == "DROPPED") {
    prius$subseries[i] <- NA
  }
}

prius <- prius[complete.cases(prius[, 6]),]
prius$subseries <- droplevels(prius$subseries)

prius$condition_grade <- as.factor(prius$condition_grade)
summary(prius$condition_grade)

for (i in 1:length(prius$condition_grade)) {
  if (prius$condition_grade[i] == "AV") {
    prius$condition_grade[i] <- 30
  }
  else if (prius$condition_grade[i] == "CL") {
    prius$condition_grade[i] <- 40
  }
  else if (prius$condition_grade[i] == "PR") {
    prius$condition_grade[i] <- 10
  }
  else if (prius$condition_grade[i] == "RG") {
    prius$condition_grade[i] <- 20
  }
  else if (prius$condition_grade[i] == "SL") {
    prius$condition_grade[i] <- 0
  }
}

prius$condition_grade <- droplevels(prius$condition_grade)
prius$condition_grade <- as.numeric(prius$condition_grade)

prius$car_year <- as.factor(prius$car_year)

# Test Model

prius_model <- lm(sale_price ~ car_year  + subseries + body + mileage +
                    condition_grade + times_run + region, data = prius)
summary(prius_model)

#Findings: All US regions have lower expected sales price than Canada Region (maybe value of dollar?)
# color is insignificant to sales price
# haven't tried using date of sale as a IV yet, might could see if there's an effect of price over time


# Avalon Cleaning 

avalon$subseries <- as.factor(avalon$subseries)
summary(avalon$subseries)
#Not big enough sample size for hybrid ltd, stx, xle touring, or xse

avalon$subseries <- fct_collapse(avalon$subseries, DROPPED = c("HYBRID LTD", "NONE",
                      "STX", "XLE TOURIN", "XSE"))

for (i in 1:length(avalon$subseries)) {
  if (avalon$subseries[i] == "DROPPED") {
    avalon$subseries[i] <- NA
  }
}

avalon <- avalon[complete.cases(avalon[, 6]),]
avalon$subseries <- droplevels(avalon$subseries)


avalon$engine <- as.factor(avalon$engine)
summary(avalon$engine)
#Two Engine Types (4H and 6G)


avalon$condition_grade <- as.factor(avalon$condition_grade)
summary(avalon$condition_grade)

for (i in 1:length(avalon$condition_grade)) {
  if (avalon$condition_grade[i] == "AV") {
    avalon$condition_grade[i] <- 30
  }
  else if (avalon$condition_grade[i] == "CL") {
    avalon$condition_grade[i] <- 40
  }
  else if (avalon$condition_grade[i] == "PR") {
    avalon$condition_grade[i] <- 10
  }
  else if (avalon$condition_grade[i] == "RG") {
    avalon$condition_grade[i] <- 20
  }
  else if (avalon$condition_grade[i] == "SL") {
    avalon$condition_grade[i] <- 0
  }
}

avalon$condition_grade <- droplevels(avalon$condition_grade)
avalon$condition_grade <- as.numeric(avalon$condition_grade)

avalon$car_year <- as.factor(avalon$car_year)


avalon_model <- lm(sale_price ~ car_year + subseries + engine + mileage +
                    condition_grade + times_run + region, data = avalon)
summary(avalon_model)


#Findings: High R^2 score, but coefficients seem a bit funky and lots of high p-scores
# need to investigate and/or try new models (want to give ML models a chance)





