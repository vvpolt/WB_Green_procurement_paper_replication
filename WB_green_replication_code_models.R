library(rio)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggforce)
library(data.table)
library(lubridate)
library(stringr)
library(foreign)
library(stargazer)
library(haven)
library(data.table)
#install.packages('margins')
library(margins)

#Read data
rm(list=ls())
setwd("/Users/file/repository")
data <-  data.table::fread("bg_cri_processed_green_pp20220426.csv", encoding = "UTF-8")

##1) Control variable cleaning
#1.1) General

data <- filter(data, filter_ok != 0)
data <- filter(data, framework_filter != 0)
data <- filter(data, opentender != "f")

#1.2) Buyer location

data$buyer_nuts <- word(data$buyer_nuts, 1)
data$buyer_nuts <- ifelse(is.na(data$buyer_nuts), data$buyer_city, data$buyer_nuts)
data$buyer_nuts[data$buyer_nuts ==""] <- "NA"
data$buyer_nuts[is.na(data$buyer_nuts)] <- "NA"
data$anb_location <- as.factor(data$buyer_nuts) 

table(data$anb_location)

#1.3) Bidder location
data$bidder_nuts[data$bidder_nuts == ""] <- "NA"
data$bidder_nuts[is.na(data$bidder_nuts)] <- "NA"
data$bidder_nuts <- word(data$bidder_nuts, 1)
data$w_location <- as.factor(data$bidder_nuts)

table(data$w_location)

#1.4) Contract type
data$tender_supplytype[data$tender_supplytype ==""] <- "NA"
data$tender_supplytype[is.na(data$tender_supplytype)] <- "NA"
data$ca_type <- as.factor(data$tender_supplytype)

table(data$ca_type, useNA = "always")

#1.5) Tender cpvs
data$tender_cpvs[(is.na(data$tender_cpvs)) & (data$tender_supplytype =="SUPPLIES")] <- "99100000"
data$tender_cpvs[(is.na(data$tender_cpvs)) & (data$tender_supplytype =="SERVICES")] <- "99100000"
data$tender_cpvs[(is.na(data$tender_cpvs)) & (data$tender_supplytype =="WORKS")] <- "99100000"
data$tender_cpvs[(is.na(data$tender_cpvs)) & (data$tender_supplytype =="NA")] <- "99100000"
data$tender_cpvs[(is.na(data$tender_cpvs)) & (is.na(data$tender_supplytype))] <- "99100000"
data$tender_cpvs[(is.na(data$tender_cpvs)) | data$tender_cpvs == ""] <- "99100000"
#for fixed effects model
data$market_id = substr(data$tender_cpvs, 1,2)
data$market_id <- as.factor(data$market_id) 

table(data$market_id, useNA = "always")

#1.6) Contract values
data$ca_contract_value10 <- ntile(data$bid_digiwhist_price, 10)
table(data$ca_contract_value10)
#1.7) buyer type
data$buyer_buyertype[data$buyer_buyertype ==""] <- "NA"
data$buyer_buyertype[is.na(data$buyer_buyertype)] <- "NA"
data$anb_type <- as.factor(data$buyer_buyertype)

table(data$anb_type, useNA = "always")

#Local buyer
data$local<-ifelse(data$bidder_nuts==data$buyer_nuts, 1, 0)
#data$winner_share<-ifelse(data$bidder_nuts==data$buyer_nuts, 1, 0)

#Share of contracts won by supplier within same market
data <- data %>%
  group_by(market_id, tender_year) %>%
  mutate(n_cont = n()) %>%
  ungroup() %>%
  group_by(market_id, bidder_id, tender_year) %>%
  mutate(n_bid_cont = n(), share_win = n_bid_cont / n_cont) %>%
  ungroup()


summary(data$share_win)
data$bidder_id=as.character(data$bidder_id)

# Sort the dataset by tender_year in ascending order
#table(data$tender_year)
#data$year<- strptime(as.character(data$tender_year), "%Y")
#data$year=as.numeric(format(data$year, "%Y"))
#table(data$year)

#New company market penetration
###another way
data <- data %>%
  dplyr::arrange(tender_year)

# Initialize previous column with a value of 1
data$previous <- 1

# Iterate over each row
for (i in 2:nrow(data)) {
  previous_years <- data$tender_year[1:(i - 1)]
  current_bidder_name <- data$bidder_name[i]
  
  if (any(data$bidder_name[1:(i - 1)] == current_bidder_name & previous_years < data$tender_year[i])) {
    data$previous[i] <- 0
  }
}
# View the updated dataset
table(data$tender_year)
table(data$bidder_name)
table(data$tender_year, data$previous)

data <- data %>%
  group_by(buyer_id, tender_year) %>%
  mutate(
    size = sum(tender_finalprice, na.rm=TRUE),
    category=ntile(size, 3)
  ) %>%
  ungroup()


data <- data %>%
  group_by(market_id) %>%
  mutate(n_m = n()) %>%
  ungroup() %>%
  group_by(market_id) %>%
  mutate(n_green = sum(green_pp_dummy == 1), share_green=n_green*100/n_m) %>%
  ungroup()

summary(data$share_green)
data$category=as.factor(data$category)
table(data$category)
table(data$previous, data$tender_year)
table(data$tender_year)

###Data extract for Marc
extr=data
extr=extr%>%
  distinct(bidder_bvdid, tender_year, green_pp_dummy, tender_finalprice, buyer_bvdid, tender_contractsignaturedate)
extr <- extr %>%
  group_by(bidder_bvdid, tender_year) %>%
  summarize(
    green_pp_n = sum(green_pp_dummy),
    contract_n = n(),
    contract_sum = sum(tender_finalprice, na.rm = TRUE),
    green_pp_value = sum(ifelse(green_pp_dummy == 1, tender_finalprice, 0), na.rm = TRUE),
    green_pp_share = (green_pp_value * 100) / contract_sum, .groups="keep"
  )

write.csv(x=extr,
          file='green_company.csv',
          row.names = FALSE)

###Making standartisation for the aggregated indicator
table(data$lot_bidscount)
clean<-data%>%
  filter(lot_bidscount<=15)
clean$lot_bidscount_z=15-clean$lot_bidscount
clean$lot_bidscount_z <- (clean$lot_bidscount_z - min(clean$lot_bidscount_z, na.rm=TRUE)) / (max(clean$lot_bidscount_z, na.rm=TRUE) - min(clean$lot_bidscount_z, na.rm=TRUE))
clean$share_win_z <- (clean$share_win - min(clean$share_win, na.rm=TRUE)) / (max(clean$share_win, na.rm=TRUE) - min(clean$share_win, na.rm=TRUE))
# Assuming clean$lot_bidscount_z, clean$share_win_z, clean$local, and clean$previous are columns in your clean frame
clean$local=as.numeric(as.character(clean$local))
clean$previous=as.numeric(as.character(clean$previous))
clean$previous_z=1-clean$previous
clean$competition_indicator <- rowMeans(clean[, c("lot_bidscount_z", "share_win_z", "local", "previous_z")], na.rm = TRUE)
clean$competition_indicator=1-clean$competition_indicator
summary(clean$competition_indicator)
#lot_bidscount higher = better competition
#share_win - higher = worse competition
#local - 1=worse competition
#previous - 1=worse competition
table(clean$market_id)
summary(data$lot_bidscount)
summary(clean$lot_bidscount)

table(is.na(data$anb_location))
table(is.na(data$anb_type))
table(is.na(data$w_location))
table(is.na(data$ca_type))
table(is.na(data$tender_year))
table(is.na(data$market_id))
table(is.na(data$ca_contract_value10))
table(is.na(data$no_cft))
table(is.na(data$corr_proc))
table(is.na(data$singleb))
table(is.na(data$green_pp_dummy))
table(is.na(data$category))
data$category=as.factor(data$category)
filt<-clean%>%
  filter(tender_year>=2011,
         share_green>0)

model10_rob <- lm(cri ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)
                  + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                  + as.factor(ca_contract_value10), data = filt)

summary(model10_rob)
model14_rob <- glm(singleb ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)
                   + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                   + as.factor(ca_contract_value10), data = filt)
summary(model14_rob)
table(is.na(clean$local))
table(is.na(clean$share_win))
table(is.na(clean$local))
table(is.na(clean$local))
filt$local=as.factor(filt$local)
filt$previous=as.factor(filt$previous)
table(filt$local)


##########

model11a_rob <- lm(lot_bidscount ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)
                   + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                   + as.factor(ca_contract_value10), data = filt)
summary(model11a_rob)
model11_rob <- glm(local ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)+
                     + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                   + as.factor(ca_contract_value10), data = filt, family = "binomial")
summary(model11_rob)

model12_rob <- lm(share_win ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)+
                    + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                  + as.factor(ca_contract_value10), data = filt)
summary(model12_rob)
#assigning weights for the new company observations downweighting the most recent observations
#data$weight <- 1 / (data$tender_year - min(data$tender_year) + 1)
filt$previous=as.factor(filt$previous)
table(filt$category)
table(data$ca_type)


model13_rob <- glm(previous ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)+
                     + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                   + as.factor(ca_contract_value10),  data = filt, family = "binomial")

summary(model13_rob)
summary(filt$competition_indicator)
model13a_rob <- lm(competition_indicator ~ as.factor(green_pp_dummy)+category+as.factor(anb_location) + as.factor(anb_type)+
                     + as.factor(w_location) + as.factor(ca_type)+ as.factor(tender_year)*as.factor(market_id) 
                   + as.factor(ca_contract_value10),  data = filt)

summary(model13a_rob)
#filt$local=as.numeric(as.character(filt$local))
#filt$previous=as.numeric(as.character(filt$previous))
#cor(filt$competition_indicator, filt$previous)

#model15_rob <- glm(present_previous_years ~ as.factor(green_pp_dummy) + as.factor(anb_location) + as.factor(anb_type) + as.factor(w_location)+
#as.factor(ca_type)+ as.factor(tender_year)+as.factor(market_id)+
#as.factor(ca_contract_value10), data = data,  family = "binomial")


# To get average marginal effects instead of log odds
library(sandwich) # For robust standard errors
library(lmtest)
# Calculate predicted probabilities
predicted_probabilities <- predict(model13_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model13_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model13_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model13_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model13_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame

# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars_1 <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)


# Calculate predicted probabilities
predicted_probabilities <- predict(model11_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model11_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model11_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model11_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model11_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame
# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars_2 <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)
ame_with_stars_2

###And same for singleb model
# Calculate predicted probabilities
predicted_probabilities <- predict(model14_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model14_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model14_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model14_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model14_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame
# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)

#same for model 
library(stargazer)
stargazer_output <- capture.output(
  stargazer(model11a_rob, model11_rob,model12_rob,model13_rob,model13a_rob,
            type = "html",
            out = "model_green_4_410.html",
            omit = c("category", "anb_location","anb_type","w_location",
                     "ca_type","year","market_id",
                     "ca_contract_value10", "no_cft",
                     "corr_proc"),
            covariate.labels = c("Green procurement"),
            dep.var.labels = c("n of bids", "local", "winning share", "new firm", "competition indicator"),
            notes = c("Included controls not shown are: Buyer location, Buyer type, Supl. location,",
                      "Contract type, Year FE*Market FE, Contract value deciles,"),
            title = "Regression results (years 2011-2019); Year*Market FE",
            notes.align = "l",
            column.labels = c("<em> ols <em>", "<em> logistic <em>", "<em> ols <em>", "<em> logistic <em>", "<em> ols <em>"),  model.names = FALSE)
  
)
cat(paste(stargazer_output[1:20], collapse = "\n"))
pattern <- '</sup></td><td>0.003</td><td>'
replacement <- paste0('</sup></td><td>', ame_with_stars_2, '</td><td>')
stargazer_output <- gsub(pattern, replacement, stargazer_output, perl = TRUE)
pattern <- '</sup></td><td>0.034</td><td>'
replacement <- paste0('</sup></td><td>', ame_with_stars_1, '</td><td>')
stargazer_output <- gsub(pattern, replacement, stargazer_output, perl = TRUE)
cat(paste(stargazer_output[1:20], collapse = "\n"))

writeLines(stargazer_output, "model_green_4_410.html")

stargazer_output <- capture.output(
  stargazer(model10_rob,model14_rob,
            type = "html",
            out = "model_green_ols_410.html",
            omit = c("category","anb_location","anb_type","w_location",
                     "ca_type","year","market_id",
                     "ca_contract_value10",
                     "corr_proc", "share_green", "no_cft"),
            covariate.labels = c("Green procurement"),
            dep.var.labels = c("cri", "singleb"),
            notes = c("Included controls not shown are: Buyer location, Buyer type, Supl. location,",
                      "Contract type, Year FE*Market FE, Contract value deciles"),
            title = "Regression results (years 2011-2019); Year*Market FE",
            notes.align = "l",
            column.labels = c("<em> ols <em>", "<em> logistic <em>"),
            model.names = FALSE)
)

# Insert the AME with significance stars into the HTML output
# Print the first few lines of the stargazer output to inspect
cat(paste(stargazer_output[1:20], collapse = "\n"))
pattern <- '</sup></td><td>-0.006</td></tr>'
replacement <- paste0('</sup></td><td>', ame_with_stars, '</td></tr>')
stargazer_output <- gsub(pattern, replacement, stargazer_output, perl = TRUE)
cat(paste(stargazer_output[1:20], collapse = "\n"))
writeLines(stargazer_output, "model_green_ols_410.html")

#####Matching
install.packages("MatchIt")
install.packages("Rcpp")
library(MatchIt)
library(Rcpp)
table(clean$tender_year)


match.1 <- matchit(green_pp_dummy~anb_type+ca_type+market_id+anb_location+w_location+tender_year, data = clean,
                   method = "exact", replace = FALSE)

#install.packages("cobalt")
#library(cobalt)
#love.plot(match.1, stars = "std")

match_dat <- match.data(match.1)
match_dat
clean_m<-match_dat%>%
  filter(lot_bidscount<=15)
model15_rob <- lm(cri ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = clean_m)
summary(model15_rob)
#model151_rob <- lm(corr_proc ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)
#model152_rob <- lm(corr_subm ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)
#model153_rob <- lm(no_cft ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)
#model154_rob <- lm(corr_decp ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)
#model155_rob <- lm(proa_ycsh ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)
#model156_rob <- lm(w_ycsh ~ as.factor(green_pp_dummy)+ as.factor(ca_contract_value10)+category, data = match_dat)

#stargazer(model15_rob,model151_rob,model152_rob,model153_rob,
#model154_rob,
#type = "html",
#out = "model_green_ols_matched_check.html",
#omit = c("category", "ca_contract_value10"),
#covariate.labels = c("Green procurement"),
#notes = c("Contract type, Buyer size (cat)"),
#title = "Regression results (years 2011-2019), matched samples",
#notes.align = "l",
#model.names = FALSE)
#stargazer(model155_rob, model156_rob,
#type = "html",
#out = "model_green_ols_matched_check.html",
#omit = c("category", "ca_contract_value10"),
#covariate.labels = c("Green procurement"),
#notes = c("Contract type, Buyer size (cat)"),
#title = "Regression results (years 2011-2019), matched samples",
#notes.align = "l",
#model.names = FALSE)
summary(match_dat$lot_bidscount)
model16_rob <- glm(singleb ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m)
summary(model16_rob)
cor(clean_m$singleb, clean_m$lot_bidscount)

# Calculate predicted probabilities
predicted_probabilities <- predict(model16_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model16_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model16_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model16_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model16_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame
# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars_3 <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)
ame_with_stars_3

library(stargazer)
stargazer_output <- capture.output(
  stargazer(model15_rob,model16_rob,
            type = "html",
            out = "model_green_ols_matched_410.html",
            omit = c("category", "ca_contract_value10", "corr_proc"),
            covariate.labels = c("Green procurement"),
            notes = c("Contract type, procedure type, buyer size (cat)"),
            title = "Regression results (years 2011-2019), matched samples",
            notes.align = "l",
            column.labels = c("<em> ols <em>", "<em> logistic <em>"),
            model.names = FALSE)
)

writeLines(stargazer_output, "model_green_ols_matched_410.html")

summary(clean_m$lot_bidscount)
clean_m<-match_dat%>%
  filter(lot_bidscount<=15)
model17_rob <- lm(lot_bidscount ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m)
model18_rob <- glm(local ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m)
model19_rob <- lm(share_win ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m)
#assigning weights for the new company observations downweighting the most recent observations
#match_dat$weight <- 1 / (match_dat$tender_year - min(match_dat$tender_year) + 1)

model20_rob <- glm(previous ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m, family = "binomial")
model21_rob <- lm(competition_indicator ~ as.factor(green_pp_dummy)+as.factor(ca_contract_value10)+category, data = clean_m)


# Calculate predicted probabilities
predicted_probabilities <- predict(model18_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model18_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model18_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model18_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model18_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame
# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars_4 <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)
ame_with_stars_4

# Calculate predicted probabilities
predicted_probabilities <- predict(model20_rob, type = "response")
# Extract coefficient for as.factor(green_pp_dummy)1
coefficient_green_pp_dummy1 <- model20_rob$coefficients["as.factor(green_pp_dummy)1"]
# Calculate marginal effects
marginal_effects_green_pp_dummy1 <- predicted_probabilities * (1 - predicted_probabilities) * coefficient_green_pp_dummy1
# Calculate average marginal effect (AME)
average_marginal_effect_green_pp_dummy1 <- mean(marginal_effects_green_pp_dummy1)
# Print the AME
print(average_marginal_effect_green_pp_dummy1)
# Get the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model20_rob)
# Calculate the gradient (partial derivatives) of the AME with respect to the coefficients
gradient <- predicted_probabilities * (1 - predicted_probabilities)
# Calculate the standard error of the AME
se_ame <- sqrt(sum(gradient^2 * vcov_matrix["as.factor(green_pp_dummy)1", "as.factor(green_pp_dummy)1"]) / length(predicted_probabilities))
# Print the standard error
print(se_ame)
###adding to the model
model20_rob$coefficients["as.factor(green_pp_dummy)1"]=average_marginal_effect_green_pp_dummy1
model20_rob$se["as.factor(green_pp_dummy)1"]=se_ame
# Calculate z-value for the AME
z_value_ame <- average_marginal_effect_green_pp_dummy1 / se_ame
# Calculate p-value for the AME
p_value_ame <- 2 * pnorm(-abs(z_value_ame))

# Determine the number of stars based on the p-value
significance_stars <- ""
if (p_value_ame < 0.001) {
  significance_stars <- "***"
} else if (p_value_ame < 0.01) {
  significance_stars <- "**"
} else if (p_value_ame < 0.05) {
  significance_stars <- "*"
} else if (p_value_ame < 0.1) {
  significance_stars <- "."
}

# Create a custom coefficient with significance stars
ame_with_stars_5 <- paste0(round(average_marginal_effect_green_pp_dummy1, 4), significance_stars)
ame_with_stars_5

stargazer_output <- capture.output(
  stargazer(model17_rob,model18_rob,model19_rob, model20_rob, model21_rob,
            type = "html",
            out = "model_green_matched_410.html",
            omit = c("category","ca_contract_value10", "corr_proc"),
            covariate.labels = c("Green procurement"),
            dep.var.labels = c("n of bids", "local", "winning share", "new firm", "competition indicator"),
            notes = c("Controls added: contract size, buyer size (cat)"),
            title = "Regression results (years 2011-2019), matched samples",
            notes.align = "l",
            column.labels = c("<em> ols <em>", "<em> logistic <em>", "<em> ols <em>", "<em> logistic <em>", "<em> ols <em>"),          model.names = FALSE)
)

cat(paste(stargazer_output[1:20], collapse = "\n"))

writeLines(stargazer_output, "model_green_matched_410.html")
