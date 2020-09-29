### Industry Project Workflow Example
## Step 1. Ingest data
general_payment <- data.table::fread("Stack/OP_DTL_GNRL_PGYR2017_P06302020.csv", showProgress = TRUE)
medicare_charges <- data.table::fread("Stack/MEDICARE_PROVIDER_CHARGE_INPATIENT_DRGALL_FY2017.CSV", showProgress = TRUE)
## Step 2. Transform and merge data
# Step 2a. Create aggregate variables by hospital in general payment
# The following code only creates 2 variables, you will need to come up with a few more on your own
# but you can use the same code syntax as I have below.
# The idea is that you want to create a data frame like this:
# Hospital Name Total_Number_of_Drug_Companies Total_PayMent_From_Third_Party
# ABC           100                             1343.0
# DEF           200                             243243.0
install.packages("dplyr")
library(dplyr)
general_payment <- general_payment[general_payment$Teaching_Hospital_Name != "",]
general_payment_agg <- general_payment %>%
  group_by(Teaching_Hospital_Name) %>%
  summarize(
    total_number_drug_companies = n_distinct(Name_of_Drug_or_Biological_or_Device_or_Medical_Supply_1, na.rm = TRUE),
    total_payment = sum(Total_Amount_of_Payment_USDollars))
View(general_payment_agg)
# Step 2b. Join the two datasets on HOSPITAL_NAME
final_data <- merge(medicare_charges, general_payment_agg,
                    by.x = "Provider Name", by.y = "Teaching_Hospital_Name",
                    all.x = TRUE, all.y = FALSE)
# we want to include all data from medicare but not all from general payment
View(final_data)
# Step 2c. Clean final data, create new Y variable
# There are multiple ways you can approach this as I mentioned before.
# Option 1. Create a new variable that measures deviation in hospital charges from procedure-wide average cost
# Option 2. Create a new variable that measures deviation in percentage in hospital charges from procedure-wide average cost
# The following code showcases option 1
final_data <- final_data %>%
  # you need to sum all charges to get total procedure charges
  mutate(total_charges = `Average Covered Charges` + `Average Medicare Payments` + `Average Total Payments`) %>%
  group_by(`DRG Definition`) %>%
  mutate(average_procedure_cost = mean(total_charges)) %>%
  ungroup() %>%
  mutate(procedure_cost_deviation = total_charges - average_procedure_cost)
View(final_data)
## Step 3. Visualilzation
# The goal of the visualization is to demonstrate potential relationship
# between some predictor variablle and the variable we want to predict
library(ggplot2)
ggplot(final_data,
       aes(x=final_data$total_number_drug_companies,
           y=final_data$procedure_cost_deviation)) +
  geom_point()
ggplot(final_data,
       aes(x=final_data$`Provider State`, y=final_data$procedure_cost_deviation)) +
  geom_point()
## Step 4. Build models
##train_index <- sample(1:nrow(final_data), 0.8 * nrow(final_data))
##final_train_set<-final_data[train_index,]
##final_test_set<-final_data[-train_index

## Cor Matrix

cor_data <- cor(final_data[,c(9,10,11,12,15,16,17)])
View(cor)

colnames(final_data) <- c('Provider_Name','DRG_Definition','Provider_Id','Provider_Street_Address','Provider_City','Provider_State', 'Provider_Zip_Coder', 'Hospital_Referral_Region_Description', 'Total_Discharges', 'Average_Covered_Charges', 'Average_Total_Payments', 'Average_Medicare_Payments', 'total_number_drug_companies', 'total_payment', 'total_charges', 'average_procedure_cost', 'procedure_cost_deviation')


##colnames(df)[colnames(df) == ''] <- 'newName'

plot_DrugAvgMed <- plot(final_data$total_number_drug_companies,final_data$Average_Medicare_Payments)

plot_DevTotal <- plot(final_data$procedure_cost_deviation, final_data$total_charges)

plot_DevPay <- plot(final_data$procedure_cost_deviation, final_data$total_payment)

plot_AvgPTC <- plot(final_data$average_procedure_cost, final_data$total_charges)

plot_AvgMTC <- plot(final_data$Average_Medicare_Payments, final_data$total_charges)

plot_ATP_AVM <- plot(final_data$Average_Total_Payments, final_data$Average_Medicare_Payments)

plot_AVM_ATP <- plot(final_data$Average_Medicare_Payments, final_data$Average_Total_Payments)

plot_dev_avp <- plot(final_data$procedure_cost_deviation, final_data$average_procedure_cost)

plot_avp_AMP <- plot(final_data$average_procedure_cost, final_data$Average_Medicare_Payments)

##Mort_LogNox_Plot <- plot(mortality$lognox,mortality$Mortality)

fit <- lm( final_data$procedure_cost_deviation ~ final_data$total_payment, data = final_data) 
summary(fit)

fit2 <- lm(final_data$total_number_drug_companies ~ final_data$Average_Medicare_Payments)
summary(fit2)

fit3 <- lm(final_data$average_procedure_cost ~ final_data$total_charges)
summary(fit3)

fit4 <- lm(final_data$procedure_cost_deviation ~ final_data$average_procedure_cost)

fit5 <- lm(final_data$Average_Total_Payments ~ final_data$Average_Medicare_Payments)

fit6 <- lm(final_data$Average_Medicare_Payments ~ final_data$Average_Total_Payments)

fit7 <- lm(final_data$Average_Total_Payments ~ final_data$Average_Medicare_Payments)

fit8 <- lm(final_data$Average_Medicare_Payments ~ final_data$average_procedure_cost)

abline(fit8, col= "red")         
summary(fit8)


final_predicted <- final_data$Average_Medicare_Payments*15.099+905.613


## mortality$residual <- resid(fit)

## fitted(fit)

install.packages("writexl")
library(writexl)
write_xlsx(final_data, "Stack/finaldata.xlsx")
 