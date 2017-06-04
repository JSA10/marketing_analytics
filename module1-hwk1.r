
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 1 - STATISTICAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


# Load text file into local variable called 'data'
data = read.delim(file = 'purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
#install.packages("sqldf")
library(sqldf)
?sqldf

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")

# Explore the data
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


# Jerome’s notes ----------------------------------------------------------
# 2 key takeaways applicable to transformation of the data

# 1: In order to compare different metrics that are on different scales of
# measurement e.g. days, units and £value need to standardise so that they are
# displayed on roughly the same scale
# Standardise by: (data point - mean) / standard deviation
# leaves data on a comparable scale of -2 to 2 roughly

# 2: And this is the new one. Statistically speaking the difference between
# a purchases of £5 and £15 and one of £310 and £320 would be the same. However
# to the business, they are not equivalent - particularly with the skewed right
#  data set, the two customers at £300+ can be grouped together but actually
#  because of the frequency, the ones at £5 and £15 could be very different
#  This is what you use the log transformation for - to transform the
#  distribution so that there is a more even spread and clearer differences can
#  be made when the data is bunched up and conversely data points that are
#  sparse can be brought closer together.

# The transformation code below contains functions to do so
# (it's pretty simple!)
# scale(data)
# log(data)

# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)

# hwk answer 1 ------------------------------------------------------------
new_data$frequency <- log(new_data$frequency)
hist(new_data$amount)
hist(new_data$frequency)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 9 segments
#members = cutree(c, k = 9)
# hwk answer 2 ------------------------------------------------------------
members = cutree(c, k = 5)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)

