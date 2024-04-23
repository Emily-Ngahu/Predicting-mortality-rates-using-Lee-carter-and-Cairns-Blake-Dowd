px_data<-read.csv("C:/Users/ADMIN/Documents/Predicting-mortality-rates-using-Lee-carter-and-Cairns-Blake-Dowd/Longevity risk.csv")
head(px_data)  # Inspect the first few rows to ensure data loaded correctly

# Inspect Data Structure
class(px_data)  # Check the class of px_data
str(px_data)    # Check the structure of px_data

RH_females<-px_data$px.RH.females
length(RH_females)
class(RH_females)
x<-as.vector(RH_females)
class(x)
# Check the class of x
class(x)

# Define a function to represent the values of RH_femal
#generate n random numbers on the interval[0,1]
x<-runif(n,min=0,max=1)
fx <- function(x) {
  return(x)
}

# Perform integration
result <- integrate(fx, lower = 0, upper = Inf)
result
