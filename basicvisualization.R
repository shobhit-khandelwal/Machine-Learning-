

#multiplot function to divide the plot area

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





#loading data to r dataframe
test <- as.data.table(read.csv("SampleLoans2.csv"))
sapply(test, class)



test

#creating ficogrp , interest_rate and loan_state variable in dataset
test<- test[, ficogrp :=ifelse(fico_range %in% c("660-664","665-669") ,"poor",
                               ifelse(fico_range %in% c("670-674","675-679","680-684","685-689","690-694","695-699"),"average", 
                                      ifelse (fico_range %in% c("699-704","705-709","710-714","715-719","720-724","725-729","735-739") ,"good","very good")))]

test<- test[, interest_rate :=ifelse(int_rate*100< 10 ,"low",
                               ifelse(int_rate*100< 15,"moderate", 
                                      ifelse (int_rate*100< 20,"high","very high")))]


test<- test[, loan_state :=ifelse(loan_status %in% c("Current", "Fully Paid") ,2,
                                     ifelse(loan_status %in% c("In Grace Period", "Late (16-30 days)") ,-2, 
                                            ifelse (loan_status %in% c("Late (31-120 days)") ,-4,-6)))]







#status of loan with respect or fico score

qplot(loan_status, data=test, geom="bar", fill=ficogrp,xlab="status of loan")



#status of loan of different grade and interest rate

ggplot(test,aes(x=loan_status,fill= grade))+geom_bar()+ggtitle("Loan status and Grade")
ggplot(test,aes(x=loan_status,fill= interest_rate))+geom_bar()+ggtitle("Loan status and Interest Rate")



#interest rate for different grade loan
ggplot(test,aes(x=interest_rate,fill= grade))+geom_bar()+ggtitle("Loan status and Grade")




#status of loan for different home_ownership

qplot(loan_status, data=test, geom="bar", fill=home_ownership,xlab="status of loan")

#status of loan with respect to interest rate
qplot(loan_status, data=test, geom="bar", fill=interest_rate,xlab="status of loan")











#mm <- ddply(test, "ficogrp" ,summarise, avg_loan = mean(loan_amount))
#ggplot(mm, aes(x = ficogrp, y = avg_loan),colour="darkgreen") + geom_bar(stat = "identity")

## ############################# SATURDAY #######################################



#for fico score for each loan in each investment grade

tt= as.data.table(sqldf('select grade ,ficogrp ,count(*) as cnt from test  group by grade,ficogrp'))


Data <- ddply(tt, .(grade), transform, pos = cumsum(cnt) - (0.5 * cnt))
p <- ggplot(Data, aes(x = grade, y = cnt)) +
  geom_bar(aes(fill = ficogrp), stat="identity") +
  geom_text(aes(label = cnt, y = pos), size = 3)+ggtitle("Grade and Fico Score of loans")

p

# status of loan with respect to purpose they were taken

pp= as.data.table(sqldf('select purpose,loan_status , count(*) as cnt from test  group by purpose,loan_status'))


Data <- ddply(pp, .(purpose), transform, pos = cumsum(cnt) - (0.5 * cnt))
l <- ggplot(Data, aes(x = purpose, y = cnt)) +
  geom_bar(aes(fill = loan_status), stat="identity") +
  geom_text(aes(label = cnt, y = pos), size = 3)+ggtitle("Loan status and Purpose")
l

#status of loan for each grade
gg= as.data.table(sqldf('select loan_status ,grade, count(*) as cnt from test  group by loan_status,grade'))


Data <- ddply(gg, .(loan_status), transform, pos = cumsum(cnt) - (0.5 * cnt))
l <- ggplot(Data, aes(x = loan_status, y = cnt)) +
  geom_bar(aes(fill = grade), stat="identity") +
  geom_text(aes(label = cnt, y = pos), size = 3)+ggtitle("Loan status and Grade")
l

#number of loans by fico range and home ownership and investmenr grade



qplot(ficogrp, data=test, geom="bar", fill=ficogrp,xlab="fico group")

qplot(grade, data= test, geom="bar", fill=grade, xlab="Grade")

qplot(factor(home_ownership), data=test, geom="bar", fill=factor(home_ownership),xlab="home_ownership")

#status of loan for interest rate level
gg= as.data.table(sqldf('select loan_status ,interest_rate ,count(*) as cnt from test  group by loan_status,interest_rate'))


Data <- ddply(gg, .(loan_status), transform, pos = cumsum(cnt) - (0.5 * cnt))
l <- ggplot(Data, aes(x = loan_status, y = cnt)) +
  geom_bar(aes(fill = interest_rate), stat="identity") +
  geom_text(aes(label = cnt, y = pos), size = 3)+ggtitle("Status of loan with Interest Rate ")
l











#grade of loans and different matrices 
setkey(test,loan_status,grade)
test2 <- test[loan_status %in% c("Current","Fully_Paid","Charged_Off","In Grace Period","Late (16-30 days)","Late (31-120 days)" ),list(average_loan=mean(loan_amount),average_income=mean(annual_income),average_monthly_installment=mean(monthly_installment),average_outstanding_principal=mean(outstanding_principal),average_principal_repaid=mean(principal_repaid),average_interest_received=mean(interest_received),average_total_payment=mean(total_payment),average_service_fee=mean(service_fee)), by=c("grade")]

test2 


p1=ggplot(test2,aes(x=grade,y=average_loan,fill=grade))+geom_bar(stat= "identity")+ggtitle("Average loan size for each Grade")
p2=ggplot(test2,aes(x=grade,y=average_interest_received,fill=grade))+geom_bar(stat= "identity")+ggtitle("Average interest received for each Grade")
p3=ggplot(test2,aes(x=grade,y=average_monthly_installment,fill=grade))+geom_bar(stat= "identity")+ggtitle("Average monthly installment for each Grade")
p4=ggplot(test2,aes(x=grade,y=average_principal_repaid,fill=grade))+geom_bar(stat= "identity")+ggtitle("Average Principal Repaid for each Grade")

  multiplot(p1,p2)
multiplot(p3,p4)

#loan status with respect to loan term
with(test, table(loan_status,term))
ggplot(test,aes(x=loan_status,fill= term))+geom_bar()+ggtitle("Loan status and term")

gg= as.data.table(sqldf('select loan_status ,term ,count(*) as cnt from test  group by loan_status,term'))


Data <- ddply(gg, .(loan_status), transform, pos = cumsum(cnt) - (0.5 * cnt))
l <- ggplot(Data, aes(x = loan_status, y = cnt)) +
  geom_bar(aes(fill = term), stat="identity") +
  geom_text(aes(label = cnt, y = pos), size = 3)+ggtitle("Status of loan with Term ")
l

## relation of grade and purpose to status of loan


##I also wanted to create a heat map showing the relation of grade and purpose to loan status but could not come up with the right data format for that, but I believe that can be really useful 

#gg= as.data.table(sqldf('select grade, purpose,loan_state,count(*) as cnt from test  group by grade,purpose,loan_state'))


#average amount of  loan and average income for each fico group and home ownership
setkey(test,home_ownership)
test2 <- test[loan_status %in% c("Current","Fully_Paid","Charged_Off","In Grace Period","Late (16-30 days)","Late (31-120 days)" ),list(average_loan=mean(loan_amount),average_income=mean(annual_income),average_monthly_installment=mean(monthly_installment),average_outstanding_principal=mean(outstanding_principal),average_principal_repaid=mean(principal_repaid),average_interest_received=mean(interest_received),average_total_payment=mean(total_payment),average_service_fee=mean(service_fee)), by=c("home_ownership")]





q1=ggplot(test2,aes(x=home_ownership,y=average_loan,fill=home_ownership ))+geom_bar(stat= "identity")+ggtitle("Home Ownership and Average Loan size")
q2=ggplot(test2,aes(x=home_ownership,y=average_income,fill=home_ownership))+geom_bar(stat= "identity",position="dodge")+ggtitle("Home Ownership and Average Income")

multiplot(q1,q2)

test2 <- test[loan_status %in% c("Current","Fully_Paid","Charged_Off","In Grace Period","Late (16-30 days)","Late (31-120 days)" ),list(average_loan=mean(loan_amount),average_income=mean(annual_income),average_monthly_installment=mean(monthly_installment),average_outstanding_principal=mean(outstanding_principal),average_principal_repaid=mean(principal_repaid),average_interest_received=mean(interest_received),average_total_payment=mean(total_payment),average_service_fee=mean(service_fee)), by=c("ficogrp")]

q1=ggplot(test2,aes(x=ficogrp,y=average_loan,fill=ficogrp ))+geom_bar(stat= "identity")+ggtitle("Fico Score and Average Loan size")
q2=ggplot(test2,aes(x=ficogrp,y=average_income,fill=ficogrp))+geom_bar(stat= "identity",position="dodge")+ggtitle("Fico Score and Average Income")

multiplot(q1,q2)



#Loan Status and Economics of loan
setkey(test,grade)
test2 <- test[loan_status %in% c("Current","Fully_Paid","Charged_Off","In Grace Period","Late (16-30 days)","Late (31-120 days)" ),list(average_loan=mean(loan_amount),average_income=mean(annual_income),average_monthly_installment=mean(monthly_installment),average_outstanding_principal=mean(outstanding_principal),average_principal_repaid=mean(principal_repaid),average_interest_received=mean(interest_received),average_total_payment=mean(total_payment),average_service_fee=mean(service_fee)), by=c("loan_status")]




p1=ggplot(test2,aes(x=loan_status,y=average_loan,fill=loan_status))+geom_bar(stat= "identity")+ggtitle("Average loan size for each loan state")
p2=ggplot(test2,aes(x=loan_status,y=average_interest_received,fill=loan_status))+geom_bar(stat= "identity")+ggtitle("Average interest recieved for each loan state")
p3=ggplot(test2,aes(x=loan_status,y=average_monthly_installment,fill=loan_status))+geom_bar(stat= "identity")+ggtitle("Average monthly installment for each loan state")
p4=ggplot(test2,aes(x=loan_status,y=average_principal_repaid,fill=loan_status))+geom_bar(stat= "identity")+ggtitle("Average princiapal repaid for each loan state")

multiplot(p1,p2)
multiplot(p3,p4)



