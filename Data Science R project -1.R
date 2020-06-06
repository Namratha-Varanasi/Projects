#                       **FOUNDATION TO DATA SCIENCE HOMEWORK1 - R MINI PROJECT** 

# 1) DATA LOADING


#Loading the data of train.csv into a data variable data.df
data.df=read.csv("train.csv")
# printing the content of the csv file train.csv
print(data.df)



#2) DATA SUMMARIZATION



#Calculating the number of rows and number of columns and storing them in variables data.df.n_rows and data.df.n_cols
data.df.n_rows=nrow(data.df)
data.df.n_cols=ncol(data.df)
#printing the number of rows and numbe of columns
print(data.df.n_rows)
print(data.df.n_cols)



#3) DATA SUBSETTING



#Creating a data frame data.df.subset to store the subset of the data having columns PassengerId,Age,Fare,Embarked
data.df.subset=data.frame(data.df$PassengerId,data.df$Age,data.df$Fare,data.df$Embarked)
#renaming them from data.df$PassengerId,data.df$Age,data.df$Fare,data.df$Embarked to "PassengerId","Age","Fare","Embarked"
names(data.df.subset)=c("PassengerId","Age","Fare","Embarked")
#printing the data frame and number of rows for checking purpose
print(data.df.subset)
print(nrow(data.df.subset))



#4) DATA CLEANING


# 4a)


#Replacing the NA values in the age column with median by first calclauting the median using median function 
#In median function first setting na.rm=TRUE to ignore NA values during median calcuation
# In left hand side we first find all the rows where we have NA for age using which(is.na(data.df.subset$Age)) 
#then replace the NA value with median as follows
data.df.subset$Age[which(is.na(data.df.subset$Age))]=median(data.df.subset$Age,na.rm = TRUE)
#Printing the dataframe which contains replaced values
print(data.df.subset)

# 4b)

# Mode Reference 
# "Tutorialspoint -R Mean Median and Mode" Internet: https://www.tutorialspoint.com/r/r_mean_median_mode.htm, [Accessed : SEP. 15, 2018]. 



#calculating the mode to replace the missing values in column Embarked with mode
# As the is no special built in function for mode we generate a user defined fucntion for mode as follows

# --> first calculate unique values in Embarked and assign it to mode.result
# x=unique(data.df.subset$Embarked)
# print(x)

# --> Second compare those unique values with all the values in the Embarked using match function
# match(data.df.subset$Embarked,x)

# --> Third use tabulate function to calculate the number of times each value occurs (frequency of each unique value in the column embarked)
# tabulate(match(data.df.subset$Embarked,x))

# --> Fourth use which.max fuction on the values of the tabulate function to obtain the index of the value with highest frequency
# which.max(tabulate(match(data.df.subset$Embarked,x)))

# --> Fifth use mode.result function on the computed result to get the value stored at the index postion which we got in fourth step
# x[which.max(tabulate(match(data.df.subset$Embarked,x)))]



# HERE CREATING A GENENRAL FUNCTION FOR MODE AND LATER USING IT ON THE EMBARKED COLUMN  OF DATA FRAME
 mode <- function(i) {
  mode.result <- unique(i)
  
  mode.result[which.max(tabulate(match(i, mode.result)))]
}

# here the mode fuction is applied on the column Embarked to get the mode value and it is stored in mode.value
mode.value=mode(data.df.subset$Embarked)

#Printing the mode value
print(mode.value)

#Replacing the empty space with mode value in the Embarked column 
data.df.subset$Embarked=sub("^$", mode.value, data.df.subset$Embarked)
#printing the data frame to see it once it is replaced
print(data.df.subset)


# 4c) 

#Checking for missing values in all columns 

#--> 1)  In passengerid column

k=data.df.subset$PassengerId
print(is.na(k))

#--> 2)  In age column

l=data.df.subset$Age
print(is.na(l))


#--> 3)  In Fare column

m=data.df.subset$Fare
print(is.na(m))

#--> 4)  In Embarked column

n=data.df.subset$Embarked
print(is.na(n))

# on checking all columns we find that no column has missing / N/A values
#DATA CLEANED

print(data.df.subset)




# 5) DATA VISUALIZATION


#5a)  HISTOGRAM FOR AGE VARIABLE 

# storing all values of age in age.result
age.result=data.df.subset$Age

# creating a histogram with age variable using hist() function where
# --> title=Histogram for Age
# --> x axis label,xlab=Age
# --> colour of the bars is set to orange and border is set to black

hist(age.result,main="Histogram for Age",xlab = "Age",col="orange",border="black")


#5b) Scatterplot Age Vs Fare

# storing all values of age in age.result and all fare values in fare.result
age.result=data.df.subset$Age
Fare.result=data.df.subset$Fare


# creating a scatterplot with age variable and fare variableusing plot() function where
# --> title=scatterplot Age Vs Fare
# --> x axis label,xlab=Age, y axis label,ylab=Fare
# --> plot 1

plot(age.result,Fare.result,main="Scatterplot Age Vs Fare", xlab="Age", ylab="Fare")

# --> plot 2
# --> in plot 2 addtionally adding ylim=c(0,300) as maximum values are within 300
# plot 2 --->plot(age.result,Fare.result,main="Scatterplot Age Vs Fare", xlab="Age", ylab="Fare",ylim=c(0,300))




#6) ANOMOLY DETECTION



# --> step 1 - Calculate mean
mean.result=mean(data.df.subset$Age)
print(mean.result)

# --> step 2 - calculate standard deviation
stdev.result=sd(data.df.subset$Age)
print(stdev.result)

# --> step 3 - calculate mean+2(stdev) and mean-2(stdev) and storing the values in p and q
p=mean.result+2*(stdev.result)
q=mean.result-2*(stdev.result)
print(p)
print(q)

# --> step 4 -create a subsets of data frame s and add the conditions that age should be more than p and less than q
# Store the corresponding passengerId values in anomalous_indices and print those anomalous_indices
s=subset(data.df.subset,data.df.subset$Age>p|data.df.subset$Age<q)
anomalous_indices=s$PassengerId
print(anomalous_indices)


# 7) DATA SUBSETTING PART 2


#  7a)

#create a subset s1 of data.df.subset having age >=25 and age<=80 
#print the subset s1 and number of rows in the subset s1 to check if the condition is applied  
s1=subset(data.df.subset,data.df.subset$Age>=25 & data.df.subset$Age<=80)
print(s1)
print(nrow(s1))

#7b)

#create a data frame t1 having only 3 columns Age, Fare ,Embarked from data.df.subset
t1=data.frame(s1$Age,s1$Fare,s1$Embarked)
# rename the columns from data.df.subset$Age,data.df.subset$Fare,data.df.subset$Embarked to "Age","Fare","Embarked"
# print the data frame t1 and also number of rows in t1 to check dataframe is created properly 
names(t1)=c("Age","Fare","Embarked")
print(t1)
print(nrow(t1))
# Now the data frames are combined and it is stored in the variable data.df.subset.v2
# Printing the data frame data.df.subset.v2 for checking purpose
data.df.subset.v2=t1
print(data.df.subset.v2)


# 8  DATA TRANSFORMATION 

#Rescaling the fare to a range of (0,100) using rescale function 
# Then storing the rescaled values of fare to the to Fare_Rescaled which is then added as a new column to data.df.subset.v2
# Finally printing the data.df.subset.v2 with the newly added column Fare_Rescaled
# here library(scales) is used to load the installed package scales to use it to support the rescale function
# check the readme.txt file for the corresponding package to be installed
library(scales)
g=rescale(data.df.subset.v2$Fare, to = c(0, 100))
data.df.subset.v2$Fare_Rescaled=g
print(data.df.subset.v2)

#COMPLETED








