# Loading of dataset
raw.data = read.csv("D://machine learning/Neelima_mam/Online Retail.csv", header = T)
View(raw.data)
str(raw.data)

data = raw.data

# Removing the invoices with missing ID numbers
length(unique(data$CustomerID))
sum(is.na(data$CustomerID))
data = subset(data, !is.na(data$CustomerID))

table(data$Country)
data = subset(data, Country == "United Kingdom")

length(unique(data$InvoiceNo))
length(unique(data$CustomerID))


# Calculate the recency and frequency table
# Identify the returns
data$item.return = grepl("C", data$InvoiceNo, fixed = TRUE)
data$purchase.invoice = ifelse(data$item.return == 'TRUE', 0, 1)



##############################################################################
####  CREATE CUSTOMER-LEVEL DEATASET ##########

customers = as.data.frame(unique(data$CustomerID))
names(customers) = "CustomerID"

#############
## RECENCY ##
#############
data$recency = as.Date("2011-12-10") - as.Date(data$InvoiceDate)

# Consider only the invoices of the transaction which are not returned
# Obtain no of days since most recent purchase
temp = subset(data, purchase.invoice == 1) 

# Add recency to customer data
recency = aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm= TRUE )
remove(temp)

customers = merge(customers, recency, by = "CustomerID", all = TRUE, sort = TRUE)
remove(recency)

customers$recency = as.numeric(customers$recency)
head(customers)


#################
### Frequency ###
#################

customer.invoices = subset(data, select = c("CustomerID","InvoiceNo","purchase.invoice"))

customer.invoices = customer.invoices[!duplicated(customer.invoices),]
customer.invoices = customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) = NULL

# Number of invoices/year (purchase only)
annual.invoices = aggregate(purchase.invoice ~ CustomerID, data = customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] = "frequency"

# Add no of invoices to customer data
customers = merge(customers, annual.invoices, by = "CustomerID", all = TRUE, sort = TRUE)
remove(customer.invoices, annual.invoices)

range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the pat year 
customers = subset(customers, frequency>0)
customers


################################
## MONETRY VALUE OF CUSTOMERS ##
################################

# Total spent on each item on an invoice
data$Amount = data$Quantity * data$UnitPrice

# Aggregated total sales to customer
annual.sales = aggregate(Amount ~ CustomerID, data = data, FUN = sum, na.rm = TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] = "Monetry"

# Add monetry value to customers dataset
customers = merge(customers, annual.sales, by="CustomerID", aa.x = TRUE, sort= TRUE)
remove(annual.sales)

# Identify customers with negative monetry value numbers , as they were presumably returning purchases
hist(customers$Monetry)
customers$Monetry = ifelse(customers$Monetry < 0, 0, customers$Monetry) # reset negative numbers
hist(customers$Monetry)


# 80/20 rule

customers = customers[order(-customers$Monetry),]

# Apply Pareto Principle(80/20 Rule) -> [80% of revenue generated by top 20 % customers]
pareto.cutoff = 0.8*sum(customers$Monetry)
customers$pareto = ifelse(cumsum(customers$Monetry) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto = factor(customers$pareto, levels = c("Top 20%","Bottom 80%"), ordered = TRUE)
levels(customers$pareto)
round(prop.table(table(customers$pareto)), 2)
remove(pareto.cutoff)

customers = customers[order(customers$CustomerID),]
head(customers)



