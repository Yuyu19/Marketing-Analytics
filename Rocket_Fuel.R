#Rocket fuel
#Yuyu Fan
#1. was the campaign effective?
t.test(rf$converted~rf$test) # p-value < 2.2e-16
# the campain was effective, group 2 has a significantly higher conversion rate

#2. was the campaign profitable?
# how much money was made excluding advertising costs?
#profit<-sum(rf$converted)*40-(sum(rf$tot_impr)/1000*9)
#profit

profit<-sum(test_group$converted)*40-(sum(rf$tot_impr)/1000*9)
profit
#cost
cost<-sum(rf$tot_impr)/1000*9
cost

#ROI= interest/investment 
ROI=profit/cost
ROI

#opportunity cost of including control group
contr_group<-rf[which(rf$test=='0'),]
actual_conversed<-sum(contr_group$converted)
hypo_conversed<-length(contr_group$converted)*mean(rf$converted)
opportunity_cost<- (hypo_conversed-actual_conversed)*40
opportunity_cost

library(dplyr)
#3. conversion chart
test_group<-rf[which(rf$test=='1'),]
conversion_chart_test<-test_group%>%
  group_by(tot_impr)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_test
plot(conversion_chart_test)

conversion_chart_control<-contr_group%>%
  group_by(tot_impr)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_control
plot(conversion_chart_control)

#implications
#1. For both groups,conversion rates increase exponenionaly for exposed groups under 250.
#2. For test groups, the consersion rate is higher after 500 exposures.
#3. For the design of next compaign, we shoud control the exposures of both group for more than 250
#   because thar's the part we are interested in.


#4.conversion~day of week
conversion_chart_test_weekday<-test_group%>%
  group_by(mode_impr_day)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_test_weekday
plot(conversion_chart_test_weekday) #Monday Saurday

conversion_chart_control_weekday<-contr_group%>%
  group_by(mode_impr_day)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_control_weekday
 
plot(conversion_chart_control_weekday) #Monday Saturday
diff_weekday<-conversion_chart_test_weekday-conversion_chart_control_weekday
diff_weekday
plot(1:7,diff_weekday$conversion_rate)# Tuesday Thursday
     
     

conversion_chart_test_hours<-test_group%>%
  group_by(mode_impr_hour)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_test_hours
plot(conversion_chart_test_hours) #16pm 2am

conversion_chart_control_hours<-contr_group%>%
  group_by(mode_impr_hour)%>%
  summarise(conversion_rate=mean(converted))
conversion_chart_control_hours
plot(conversion_chart_control_hours)#16pm 2am
diff_daytime<-conversion_chart_test_hours-conversion_chart_control_hours
diff_daytime
plot(0:23,diff_daytime$conversion_rate) #6pm  4pm

#Conclusion: Conversion rates are highest for both group at Monday 16pm,
### and are lowest at Saturday 2am.
### Advertising is the most effective on Tuesday 6pm,
### and the leat effective on Thursday 4pm.
