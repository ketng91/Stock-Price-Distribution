#Question 1: Mr.R wants to make an investment in the stock market. He wants to know the the prices range and the number of stocks in each price range.
#Solution: We can use histogram graph to illustrate the stock price distribution.

#Step 1:Import R packages and dataset:
install.packages("tidyverse")
library("tidyverse")
library(readxl)
stock <- read_excel("D:/STUDY/DATA SCIENCE/TABLEAU/Daily Stock Tracking.xlsx",
                    sheet = "Stock")
#Step 2: Understanding the Data set
View(stock)
str(stock)
summary(stock$"Close Price") #detect irrelevant data values

#Step 3: Extracting, Transforming and Processing data
substock <- stock %>% 
            select(Stock, "Close Price") %>% #select only relevant data
            rename(Price="Close Price") %>% #Rename column for simple interpretation
            drop_na(Price) %>% #Make sure that only useful columns and rows are kept
            arrange(-Price)
substock2<- substock[-c(1,2),] #remove irrelevant data values
View(substock)
View(substock2) #This is the final clean dataset for analysis

#Step 4: Visualizing the data
substock2 %>%
  ggplot(aes(x=Price)) +
  geom_histogram(binwidth=5, fill="#2CAAF0", color="white", alpha=0.9) +
  theme_classic() +
  labs(x ="Stock Price",
       y = "No. of Stocks",
       title = "The Frequency Distribution of Stock Price"
       ) +
  scale_x_continuous(breaks = seq(0, 250, 10))
#Here we can note that the graph is biased towards the left side,
#and hence this is a sign of distribution, which is right-skewed distribution.
#A large number of data prices occur on the left side and fewer data on the right side.
#That means a greater number of stocks have the price less than 50, especially in the below price 20 group.

#Further details: Count the number of stocks below 50.
total <-substock2 %>%
            summarise(n())

detail<- substock2 %>%
      filter(Price<50) %>%
      summarise(Numberstock50= n()) %>%
      mutate(Total=total) %>%
      mutate(Percentage = round(100*Numberstock50/Total))
View(detail)
#Remark: 88% of stocks have the price below 50.
