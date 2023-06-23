# Students Exam: Anova Analysis

# Introduction

The aim of this study is to identify the variables—divided into economic, genetic, societal, and personal factors—that influence academic performance as measured by math, reading, and writing scores on some exams.


![Customer-service-in-education jpg](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/7aa37507-c809-4e77-9fd9-3bf8117f41af)

## About Dataset

This dataset includes scores from three test scores of students at a (fictional) public school and a variety of personal and socio-economic factors that may have interaction effects upon them.

**Remark/warning/disclaimer:**

This datasets are fictional and should be used for educational purposes only.

The original dataset generator creator is Mr. Royce Kimmons

### Content (column description)

**`Gender:`** Gender of the student (male/female)

**`EthnicGroup:`** Ethnic group of the student (group A to E)

**`ParentEduc:`** Parent(s) education background (from some_highschool to master's degree)

**`LunchType:`** School lunch type (standard or free/reduced)

**`TestPrep:`** Test preparation course followed (completed or none)

**`ParentMaritalStatus:`** Parent(s) marital status (married/single/widowed/divorced)

**`PracticeSport:`** How often the student parctice sport (never/sometimes/regularly))

**`IsFirstChild:`** If the child is first child in the family or not (yes/no)

**`NrSiblings:`** Number of siblings the student has (0 to 7)

**`TransportMeans:`** Means of transport to school (schoolbus/private)

**`WklyStudyHours:`** Weekly self-study hours(less that 5hrs; between 5 and 10hrs; more than 10hrs)

**`MathScore:`** math test score(0-100)

**`ReadingScore:`** reading test score(0-100)

**`WritingScore:`** writing test score(0-100)

### Loading libraries

Firstly I will start by loading some packages that I will use during the analysis

~~~
library(tidyverse)
library(VIM)
library(ggthemes)
library(ggpubr)
library(ggsci)
library(ggcorrplot)
~~~

**Getting the data**

~~~
Students<-read.csv("Students.csv")
~~~

# Exploration of the data¶

The structure of the data

~~~
##the structure of the data
str(Students)
~~~

~~~
'data.frame':	30641 obs. of  15 variables:
 $ X                  : int  0 1 2 3 4 5 6 7 8 9 ...
 $ Gender             : chr  "female" "female" "female" "male" ...
 $ EthnicGroup        : chr  "" "group C" "group B" "group A" ...
 $ ParentEduc         : chr  "bachelor's degree" "some college" "master's degree" "associate's degree" ...
 $ LunchType          : chr  "standard" "standard" "standard" "free/reduced" ...
 $ TestPrep           : chr  "none" "" "none" "none" ...
 $ ParentMaritalStatus: chr  "married" "married" "single" "married" ...
 $ PracticeSport      : chr  "regularly" "sometimes" "sometimes" "never" ...
 $ IsFirstChild       : chr  "yes" "yes" "yes" "no" ...
 $ NrSiblings         : int  3 0 4 1 0 1 1 1 3 NA ...
 $ TransportMeans     : chr  "school_bus" "" "school_bus" "" ...
 $ WklyStudyHours     : chr  "< 5" "5 - 10" "< 5" "5 - 10" ...
 $ MathScore          : int  71 69 87 45 76 73 85 41 65 37 ...
 $ ReadingScore       : int  71 90 93 56 78 84 93 43 64 59 ...
 $ WritingScore       : int  74 88 91 42 75 79 89 39 68 50 ...
~~~

summary of the data

~~~
##summary of the data
summary(Students)
~~~

~~~
       X            Gender          EthnicGroup       
 Min.   :  0.0   Length:30641       Length:30641      
 1st Qu.:249.0   Class :character   Class :character  
 Median :500.0   Mode  :character   Mode  :character  
 Mean   :499.6                                        
 3rd Qu.:750.0                                        
 Max.   :999.0                                        
                                                      
  ParentEduc         LunchType           TestPrep        
 Length:30641       Length:30641       Length:30641      
 Class :character   Class :character   Class :character  
 Mode  :character   Mode  :character   Mode  :character  
                                                 
 ParentMaritalStatus PracticeSport      IsFirstChild      
 Length:30641        Length:30641       Length:30641      
 Class :character    Class :character   Class :character  
 Mode  :character    Mode  :character   Mode  :character  
                                                     
   NrSiblings    TransportMeans     WklyStudyHours    
 Min.   :0.000   Length:30641       Length:30641      
 1st Qu.:1.000   Class :character   Class :character  
 Median :2.000   Mode  :character   Mode  :character  
 Mean   :2.146                                        
 3rd Qu.:3.000                                        
 Max.   :7.000                                        
 NA's   :1572
                                      
   MathScore       ReadingScore     WritingScore   
 Min.   :  0.00   Min.   : 10.00   Min.   :  4.00  
 1st Qu.: 56.00   1st Qu.: 59.00   1st Qu.: 58.00  
 Median : 67.00   Median : 70.00   Median : 69.00  
 Mean   : 66.56   Mean   : 69.38   Mean   : 68.42  
 3rd Qu.: 78.00   3rd Qu.: 80.00   3rd Qu.: 79.00  
 Max.   :100.00   Max.   :100.00   Max.   :100.00  
~~~

the first fifteen rows of the data

~~~
head(Students,15)
~~~

~~~
    X Gender EthnicGroup         ParentEduc    LunchType  TestPrep ParentMaritalStatus PracticeSport IsFirstChild NrSiblings
1   0 female              bachelor's degree     standard      none             married     regularly          yes          3
2   1 female     group C       some college     standard                       married     sometimes          yes          0
3   2 female     group B    master's degree     standard      none              single     sometimes          yes          4
4   3   male     group A associate's degree free/reduced      none             married         never           no          1
5   4   male     group C       some college     standard      none             married     sometimes          yes          0
6   5 female     group B associate's degree     standard      none             married     regularly          yes          1
7   6 female     group B       some college     standard completed             widowed         never           no          1
8   7   male     group B       some college free/reduced      none             married     sometimes          yes          1
9   8   male     group D        high school free/reduced completed              single     sometimes           no          3
10  9 female     group B        high school free/reduced      none             married     regularly          yes         NA
11 10   male     group C associate's degree     standard      none                         sometimes          yes          1
12 11   male     group D associate's degree     standard      none            divorced     sometimes          yes          1
13 12 female     group B        high school     standard      none             married     regularly           no          1
14 13   male     group A       some college     standard completed              single     sometimes          yes          1
15 14 female     group A    master's degree     standard      none            divorced     sometimes          yes          2
   TransportMeans WklyStudyHours MathScore ReadingScore WritingScore
1      school_bus            < 5        71           71           74
2                         5 - 10        69           90           88
3      school_bus            < 5        87           93           91
4                         5 - 10        45           56           42
5      school_bus         5 - 10        76           78           75
6      school_bus         5 - 10        73           84           79
7         private         5 - 10        85           93           89
8         private           > 10        41           43           39
9         private           > 10        65           64           68
10        private            < 5        37           59           50
11        private         5 - 10        58           54           52
12     school_bus         5 - 10        40           52           43
13        private         5 - 10        66           82           74
14        private           > 10        80           73           71
15        private            < 5        48           53           58
~~~

There are blank cells in the data set; we need to convert them to "NA"

~~~
Students[Students==""] <- NA
colSums(is.na(Students))
~~~

~~~
                  X              Gender         EthnicGroup 
                  0                   0                1840 
         ParentEduc           LunchType            TestPrep 
               1845                   0                1830 
ParentMaritalStatus       PracticeSport        IsFirstChild 
               1190                 631                 904 
         NrSiblings      TransportMeans      WklyStudyHours 
               1572                3134                 955 
          MathScore        ReadingScore        WritingScore 
                  0                   0                   0 
~~~

Determine the amount of missing values for each variable and plot them.

~~~
aggr_plot <- aggr(Students, col=c('#4EE2EC','#fc8d62'),
                  numbers=TRUE, sortVars=TRUE, labels=names(data),
                  cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))
~~~

~~~
 Variables sorted by number of missings: 
            Variable      Count
      TransportMeans 0.10228126
          ParentEduc 0.06021344
         EthnicGroup 0.06005026
            TestPrep 0.05972390
          NrSiblings 0.05130381
 ParentMaritalStatus 0.03883685
      WklyStudyHours 0.03116739
        IsFirstChild 0.02950295
       PracticeSport 0.02059332
                   X 0.00000000
              Gender 0.00000000
           LunchType 0.00000000
           MathScore 0.00000000
        ReadingScore 0.00000000
~~~

![na](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/3fa62241-94d5-4feb-beb3-01c3288fe38a)

There is about 10 percent of the "Transport Means" column missing, and about 6 percent or less from some other columns. We are going to impute them later, but now we are going to deal with the data as it is.

**checking for duplicates**

~~~
dim(Students)
[1] 30641    15
dim(unique(Students))
[1] 30641    15
~~~

There are no duplicates in the data

## Plotting the independent variables against the math, reading, and writing scores.

### Gender

**Math Score**

~~~

##math score density chart for males
plota<-ggplot(subset(Students,Gender=="male"),aes(x=MathScore))+
  geom_density(fill="#fc8d62",alpha=0.5)+
  ggtitle("Distribution of male Math Score")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$Gender=="male"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$Gender=="male"])+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$MathScore[Students$Gender=="male"])))))


##math score density chart for females
plotb<-ggplot(subset(Students,Gender=="female"),aes(x=MathScore))+
  geom_density(fill="#66c2a5",alpha=0.5)+
  ggtitle("Distribution of female Math Score")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$Gender=="female"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$Gender=="female"])+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$MathScore[Students$Gender=="female"])))))

##math score density chart for both males and females
plot1<-ggplot(Students,aes(x=MathScore,fill=Gender))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Math Score by Gender")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())


##boxplot showing math score by gender
plot2<-ggplot(Students,aes(y=MathScore,x=Gender,fill=Gender))+
  geom_boxplot()+
  ggtitle("Boxplot of Math Score")+
  xlab("Gender")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))

##combining the plots together
combined_plot1 <- ggarrange(plotb,plota,
                            plot1,plot2,
                            nrow = 2,
                            ncol = 2)

combined_plot1
~~~

![combined_plot1](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/05b90366-10aa-46bc-9369-c789ac734a85)

The mean math scores of females and males are about 64 and 69, respectively, but is that statistically significant? We need to take a t-test to show if the difference between females and males in math scores is statistically significant or not.

Our null hypothesis will be that there is no difference between the mean of math scores between males and females, and the rejection region will be at P<0.05

~~~
t.test(MathScore~Gender,data=Students)
~~~

~~~
	Welch Two Sample t-test

data:  MathScore by Gender
t = -28.809, df = 30639, p-value < 2.2e-16
alternative hypothesis: true difference in means between group female and group male is not equal to 0
95 percent confidence interval:
 -5.328643 -4.649762
sample estimates:
mean in group female   mean in group male 
            64.08065             69.06986
~~~

as we can see alternative hypothesis: true difference in means between females and males is not equal to 0 and the 95 percent confidence interval:
 -5.328643 -4.649762 


**Reading score**

~~~
##Reading score density chart for males
plotc<-ggplot(subset(Students,Gender=="male"),aes(x=ReadingScore))+
  geom_density(fill="#EFC000FF",alpha=0.5)+
  ggtitle("Distribution of male Reading Score")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$Gender=="male"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$ReadingScore[Students$Gender=="male"])+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$ReadingScore[Students$Gender=="male"])))))


##Reading score density chart for females
plotd<-ggplot(subset(Students,Gender=="female"),aes(x=ReadingScore))+
  geom_density(fill="#0073C2FF",alpha=0.5)+
  ggtitle("Distribution of female Reading Score")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$Gender=="female"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$ReadingScore[Students$Gender=="female"])+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$ReadingScore[Students$Gender=="female"])))))

##Reading score density chart for both males and females
plot3<-ggplot(Students,aes(x=ReadingScore,fill=Gender))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Reading Score by Gender")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())+
  scale_fill_jco()


##boxplot showing Reading Score by gender
plot4<-ggplot(Students,aes(y=ReadingScore,x=Gender,fill=Gender))+
  geom_boxplot()+
  ggtitle("Boxplot of Reading Score")+
  xlab("Gender")+
  ylab("Reading Score")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_jco()

##combining the plots together
combined_plot2 <- ggarrange(plotd,plotc,
                            plot3,plot4,
                            nrow = 2,
                            ncol = 2)

combined_plot2
~~~

![combined_plot2](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/e1209559-5a79-4503-8ff5-d57500ef0f06)

~~~~
t.test(ReadingScore~Gender,data=Students)
~~~~

~~~~
	Welch Two Sample t-test

data:  ReadingScore by Gender
t = 42.717, df = 30622, p-value < 2.2e-16
alternative hypothesis: true difference in means between group female and group male is not equal to 0
95 percent confidence interval:
 6.677514 7.319776
sample estimates:
mean in group female   mean in group male 
            72.85322             65.85457 
~~~~

alternative hypothesis: true difference in means between females and males is not equal to 095 percent confidence interval:
 6.677514 7.319776

**Writing Score**

~~~
##Writing Score density chart for males
plote<-ggplot(subset(Students,Gender=="male"),aes(x=WritingScore))+
  geom_density(fill="#377eb8",alpha=0.5)+
  ggtitle("Distribution of male Writing Score")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$Gender=="male"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$WritingScore[Students$Gender=="male"])+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$WritingScore[Students$Gender=="male"])))))


##Writing Score density chart for females
plotf<-ggplot(subset(Students,Gender=="female"),aes(x=WritingScore))+
  geom_density(fill="#e41a1c",alpha=0.5)+
  ggtitle("Distribution of female Writing Score")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$Gender=="female"]),
             col="#808080")+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$WritingScore[Students$Gender=="female"])+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$WritingScore[Students$Gender=="female"])))))

##Writing Score density chart for both males and females
plot5<-ggplot(Students,aes(x=WritingScore,fill=Gender))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Writing Score by Gender")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())


##boxplot showing Writing Score by gender
plot6<-ggplot(Students,aes(y=WritingScore,x=Gender,fill=Gender))+
  geom_boxplot()+
  ggtitle("Boxplot of Writing Score")+
  xlab("Gender")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))

##combining the plots together
combined_plot3 <- ggarrange(plotf,plote,
                            plot5,plot6,
                            nrow = 2,
                            ncol = 2)

combined_plot3
~~~

![combined_plot3](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/28b21e5e-dd1a-40f9-9856-affa01928896)

~~~
##t-test
t.test(WritingScore~Gender,data=Students)
~~~

~~~
	Welch Two Sample t-test

data:  WritingScore by Gender
t = 52.898, df = 30611, p-value < 2.2e-16
alternative hypothesis: true difference in means between group female and group male is not equal to 0
95 percent confidence interval:
 8.604928 9.267151
sample estimates:
mean in group female   mean in group male 
            72.85646             63.92042 
~~~

alternative hypothesis: true difference in means between females and males is not equal to 095 percent confidence interval:
 8.604928 9.267151

### Ethnic group

**MathScore**

~~~
##histogram of math score by Ethnic Group
plot7<-ggplot(subset(Students,!is.na(EthnicGroup)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#EB5406")+
  facet_grid(~EthnicGroup)+
  ggtitle("Distribution of the Math Score by Ethnic Group")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot of math score by Ethnic Group
plot8<-ggplot(subset(Students,!is.na(EthnicGroup)),
              aes(y=MathScore,x=EthnicGroup,fill=EthnicGroup))+
  geom_boxplot()+
  ggtitle("Boxplot of the Math Score by Ethnic Group")+
  xlab("Ethnic Group")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot4 <- ggarrange(plot7,
                            plot8,
                            nrow = 1,
                            ncol = 2)
combined_plot4
~~~

![combined_plot4](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/0833f010-06d5-42bf-86b7-662290064581)

We need to analyse the variance between variables and the null hypothesis (Ho), which states that there is no significant difference between the variables being measured. If the test yields statistically significant results, then the tester can reject the null hypothesis and accept the alternative hypothesis (H1), stating that the difference between variables is significant.

~~~
summary(aov(MathScore~EthnicGroup,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
EthnicGroup     4  432947  108237   489.5 <2e-16 ***
Residuals   28796 6367701     221                   
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1840 observations deleted due to missingness
~~~

The Anova test showed us the difference between the variables is statistically significant; we will use Tukey's HSD tests to test differences among sample means for significance.

~~~
TukeyHSD(aov(MathScore~EthnicGroup,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MathScore ~ EthnicGroup, data = Students)

$EthnicGroup
                     diff        lwr       upr     p adj
group B-group A  0.498328 -0.5135608  1.510217 0.6639820
group C-group A  1.703835  0.7446104  2.663059 0.0000125
group D-group A  4.674512  3.6943103  5.654713 0.0000000
group E-group A 12.307048 11.2352871 13.378808 0.0000000
group C-group B  1.205507  0.5265118  1.884502 0.0000126
group D-group B  4.176184  3.4678634  4.884504 0.0000000
group E-group B 11.808720 10.9783020 12.639137 0.0000000
group D-group C  2.970677  2.3398760  3.601478 0.0000000
group E-group C 10.603213  9.8378465 11.368579 0.0000000
group E-group D  7.632536  6.8410376  8.424034 0.0000000
~~~

The Tukey's tests showed us that the difference between every ethnic group is statistically significant except between groups A and B.

**Reading Score**

~~~
##histogram of Reading score by Ethnic Group
plot9<-ggplot(subset(Students,!is.na(EthnicGroup)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#EB5406")+
  facet_grid(~EthnicGroup)+
  ggtitle("Distribution of the Reading Score by Ethnic Group")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))


##boxplot of Reading score by Ethnic Group
plot10<-ggplot(subset(Students,!is.na(EthnicGroup)),
               aes(y=ReadingScore,x=EthnicGroup,fill=EthnicGroup))+
  geom_boxplot()+
  ggtitle("Boxplot of the Reading Score by Ethnic Group")+
  xlab("Ethnic Group")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_jco()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot5 <- ggarrange(plot9,
                            plot10,
                            nrow = 1,
                            ncol = 2)

combined_plot5
~~~

![combined_plot5](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/6c984d69-a22a-450f-aea1-06f71f2bf17d)

The Anova test 

~~~
summary(aov(ReadingScore~EthnicGroup,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
EthnicGroup     4  151205   37801   177.7 <2e-16 ***
Residuals   28796 6125870     213                   
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1840 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(ReadingScore~EthnicGroup,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ EthnicGroup, data = Students)

$EthnicGroup
                     diff        lwr      upr     p adj
group B-group A 0.5327178 -0.4597705 1.525206 0.5859170
group C-group A 1.6504905  0.7096571 2.591324 0.0000169
group D-group A 3.5945049  2.6330964 4.555913 0.0000000
group E-group A 7.4636807  6.4124686 8.514893 0.0000000
group C-group B 1.1177727  0.4517960 1.783750 0.0000461
group D-group B 3.0617871  2.3670471 3.756527 0.0000000
group E-group B 6.9309629  6.1164666 7.745459 0.0000000
group D-group C 1.9440144  1.3253074 2.562721 0.0000000
group E-group C 5.8131902  5.0624979 6.563882 0.0000000
group E-group D 3.8691758  3.0928527 4.645499 0.0000000
~~~

**Writing Score**

~~~
##histogram of Writing score by Ethnic Group
plot11<-ggplot(subset(Students,!is.na(EthnicGroup)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#EB5406")+
  facet_grid(~EthnicGroup)+
  ggtitle("Distribution of the Writing Score by Ethnic Group")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot of Writing score by Ethnic Group
plot12<-ggplot(subset(Students,!is.na(EthnicGroup)),
               aes(y=WritingScore,x=EthnicGroup,fill=EthnicGroup))+
  geom_boxplot()+
  ggtitle("Boxplot of the Writing Score by Ethnic Group")+
  xlab("Ethnic Group")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot6 <- ggarrange(plot11,
                            plot12,
                            nrow = 1,
                            ncol = 2)


combined_plot6
~~~

![combined_plot6](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/1f490665-dad5-4f92-84d8-b5f57ef7aade)

The Anova test 

~~~
summary(aov(WritingScore~EthnicGroup,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
EthnicGroup     4  197018   49254   212.2 <2e-16 ***
Residuals   28796 6684369     232                   
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1840 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~EthnicGroup,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = WritingScore ~ EthnicGroup, data = Students)

$EthnicGroup
                    diff        lwr      upr     p adj
group B-group A 0.643210 -0.3935344 1.679954 0.4386413
group C-group A 1.747325  0.7645386 2.730111 0.0000122
group D-group A 5.638928  4.6346497 6.643207 0.0000000
group E-group A 7.425145  6.3270581 8.523232 0.0000000
group C-group B 1.104115  0.4084414 1.799788 0.0001456
group D-group B 4.995718  4.2699991 5.721438 0.0000000
group E-group B 6.781935  5.9311192 7.632751 0.0000000
group D-group C 3.891604  3.2453077 4.537899 0.0000000
group E-group C 5.677820  4.8936535 6.461987 0.0000000
group E-group D 1.786216  0.9752762 2.597157 0.0000000
~~~

### Parent education

**Math Score**

~~~
##histogram of math score by Parent education
plot13<-ggplot(subset(Students,!is.na(ParentEduc)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#997070")+
  facet_grid(~ParentEduc)+
  ggtitle("Distribution of the Math Score by parent education")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))


##boxplot of math score by Parent education
plot14<-ggplot(subset(Students,!is.na(ParentEduc)),
               aes(y=MathScore,x=ParentEduc,fill=ParentEduc))+
  geom_boxplot()+
  ggtitle("Boxplot of the Math Score by parent education")+
  xlab("parent education")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot7 <- ggarrange(plot13,
                            plot14,
                            nrow = 1,
                            ncol = 2)

combined_plot7
~~~

![combined_plot7](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/484741fb-cbc9-4442-99a5-56c4cbb84ace)

The Anova test 

~~~
summary(aov(MathScore~ParentEduc,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
ParentEduc      5  250332   50066   219.7 <2e-16 ***
Residuals   28790 6560735     228                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1845 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(MathScore~ParentEduc,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MathScore ~ ParentEduc, data = Students)

$ParentEduc
                                          diff         lwr       upr     p adj
bachelor's degree-associate's degree  2.101042   1.1629679  3.039115 0.0000000
high school-associate's degree       -3.929855  -4.7415492 -3.118161 0.0000000
master's degree-associate's degree    3.970549   2.8533129  5.087785 0.0000000
some college-associate's degree      -1.975114  -2.7576978 -1.192530 0.0000000
some high school-associate's degree  -5.781573  -6.5994201 -4.963725 0.0000000
high school-bachelor's degree        -6.030897  -6.9646792 -5.097114 0.0000000
master's degree-bachelor's degree     1.869507   0.6606571  3.078357 0.0001524
some college-bachelor's degree       -4.076155  -4.9847478 -3.167563 0.0000000
some high school-bachelor's degree   -7.882614  -8.8217505 -6.943478 0.0000000
master's degree-high school           7.900404   6.7867685  9.014039 0.0000000
some college-high school              1.954741   1.1773062  2.732176 0.0000000
some high school-high school         -1.851718  -2.6646395 -1.038796 0.0000000
some college-master's degree         -5.945663  -7.0382622 -4.853063 0.0000000
some high school-master's degree     -9.752121 -10.8702496 -8.633993 0.0000000
some high school-some college        -3.806459  -4.5903162 -3.022602 0.0000000
~~~

**Reading Score**

~~~
##histogram of Reading score by Parent education
plot15<-ggplot(subset(Students,!is.na(ParentEduc)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#997070")+
  facet_grid(~ParentEduc)+
  ggtitle("Distribution of the Reading Score by parent education")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot of Reading score by Parent education
plot16<-ggplot(subset(Students,!is.na(ParentEduc)),
               aes(y=ReadingScore,x=ParentEduc,fill=ParentEduc))+
  geom_boxplot()+
  ggtitle("Boxplot of the Reading Score by parent education")+
  xlab("parent education")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_jco()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot8 <- ggarrange(plot15,
                            plot16,
                            nrow = 1,
                            ncol = 2)

combined_plot8
~~~

![combined_plot8](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/10cfe2c6-cd53-4574-9b40-1fe0a8c29e4d)

The Anova test 

~~~
summary(aov(ReadingScore~ParentEduc,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
ParentEduc      5  256569   51314   245.4 <2e-16 ***
Residuals   28790 6021180     209                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1845 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(ReadingScore~ParentEduc,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ ParentEduc, data = Students)

$ParentEduc
                                           diff        lwr        upr p adj
bachelor's degree-associate's degree   1.937696   1.039023  2.8363685     0
high school-associate's degree        -3.910327  -4.687929 -3.1327261     0
master's degree-associate's degree     4.708597   3.638287  5.7789068     0
some college-associate's degree       -1.944617  -2.694331 -1.1949029     0
some high school-associate's degree   -5.613539  -6.397036 -4.8300433     0
high school-bachelor's degree         -5.848023  -6.742585 -4.9534616     0
master's degree-bachelor's degree      2.770901   1.612825  3.9289772     0
some college-bachelor's degree        -3.882313  -4.752742 -3.0118830     0
some high school-bachelor's degree    -7.551235  -8.450926 -6.6515447     0
master's degree-high school            8.618925   7.552064  9.6857849     0
some college-high school               1.965711   1.220930  2.7104918     0
some high school-high school          -1.703212  -2.481989 -0.9244345     0
some college-master's degree          -6.653214  -7.699922 -5.6065057     0
some high school-master's degree     -10.322137 -11.393301 -9.2509721     0
some high school-some college         -3.668923  -4.419856 -2.9179890     0
~~~

**Writing Score**

~~~
##histogram of Writing score by Parent education
plot17<-ggplot(subset(Students,!is.na(ParentEduc)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#997070")+
  facet_grid(~ParentEduc)+
  ggtitle("Distribution of the Writing Score by parent education")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot of Writing score by Parent education
plot18<-ggplot(subset(Students,!is.na(ParentEduc)),
               aes(y=WritingScore,x=ParentEduc,fill=ParentEduc))+
  geom_boxplot()+
  ggtitle("Boxplot of the Writing Score by parent education")+
  xlab("parent education")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot9 <- ggarrange(plot17,
                            plot18,
                            nrow = 1,
                            ncol = 2)

combined_plot9
~~~

![combined_plot9](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/8950f641-390b-4a4d-83da-9098dd4b290d)

The Anova test 

~~~
summary(aov(WritingScore~ParentEduc,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)    
ParentEduc      5  406343   81269   361.7 <2e-16 ***
Residuals   28790 6469269     225                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1845 observations deleted due to missingness
~~~
 
Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~ParentEduc,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = WritingScore ~ ParentEduc, data = Students)

$ParentEduc
                                           diff        lwr         upr p adj
bachelor's degree-associate's degree   3.031970   2.100458   3.9634818     0
high school-associate's degree        -4.877963  -5.683979  -4.0719469     0
master's degree-associate's degree     6.057797   4.948376   7.1672173     0
some college-associate's degree       -1.797667  -2.574777  -1.0205571     0
some high school-associate's degree   -6.666690  -7.478817  -5.8545636     0
high school-bachelor's degree         -7.909933  -8.837184  -6.9826826     0
master's degree-bachelor's degree      3.025827   1.825433   4.2262206     0
some college-bachelor's degree        -4.829637  -5.731873  -3.9274003     0
some high school-bachelor's degree    -9.698660 -10.631227  -8.7660934     0
master's degree-high school           10.935760   9.829914  12.0416051     0
some college-high school               3.080296   2.308300   3.8522931     0
some high school-high school          -1.788727  -2.595962  -0.9814916     0
some college-master's degree          -7.855463  -8.940420  -6.7705067     0
some high school-master's degree     -12.724487 -13.834794 -11.6141800     0
some high school-some college         -4.869023  -5.647397  -4.0906492     0
~~~

### Lunch Type
  
**Math Score**
  
~~~
##math score density chart for free/reduced Lunch Type
plote<-ggplot(subset(Students,LunchType=="free/reduced"),aes(x=MathScore))+
  geom_density(fill="#3B4992FF",alpha=0.5)+
  ggtitle("Distribution of Math Score by free/reduced Lunch")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$LunchType=="free/reduced"]))+
  geom_text(size=4.2,
            aes(x=mean(Students$MathScore[Students$LunchType=="free/reduced"])+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$MathScore[Students$LunchType=="free/reduced"])))))


##math score density chart for standard Lunch Type
plotf<-ggplot(subset(Students,LunchType=="standard"),aes(x=MathScore))+
  geom_density(fill="#EE0000FF",alpha=0.5)+
  ggtitle("Distribution of Math Score by standard Lunch")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$LunchType=="standard"]))+
  geom_text(size=4.2,
            aes(x=mean(Students$MathScore[Students$LunchType=="standard"])+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$MathScore[Students$LunchType=="standard"])))))

##math score density chart for both standard and free/reduced Lunch Type
plot19<-ggplot(subset(Students,!is.na(LunchType)),aes(x=MathScore,fill=LunchType))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Math Score by Lunch Type")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing math score by LunchType
plot20<-ggplot(subset(Students,!is.na(LunchType)),
               aes(y=MathScore,x=LunchType,fill=LunchType))+
  geom_boxplot()+
  ggtitle("Boxplot of Math Score")+
  xlab("Lunch Type")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot10 <- ggarrange(plote,plotf,
                             plot19,plot20,
                             nrow = 2,
                             ncol = 2)

combined_plot10
~~~

![combined_plot10](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/1be0e236-dee2-41ce-93e4-66dfef70ef54)
  
~~~  
t.test(MathScore~LunchType,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  MathScore by LunchType
t = -69.026, df = 21772, p-value < 2.2e-16
alternative hypothesis: true difference in means between group free/reduced and group standard is not equal to 0
95 percent confidence interval:
  -12.18345 -11.51063
sample estimates:
  mean in group free/reduced     mean in group standard 
58.86233                   70.70937   
~~~  

**Reading Score**

~~~
##reading score density chart for free/reduced Lunch Type
plotg<-ggplot(subset(Students,LunchType=="free/reduced"),aes(x=ReadingScore))+
  geom_density(fill="#1F77B4FF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by free/reduced Lunch")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$LunchType=="free/reduced"]),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$ReadingScore[Students$LunchType=="free/reduced"])+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$ReadingScore[Students$LunchType=="free/reduced"]))))


##readinf score density chart for standard Lunch Type
ploth<-ggplot(subset(Students,LunchType=="standard"),aes(x=ReadingScore))+
  geom_density(fill="#FF7F0EFF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by standard Lunch")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$LunchType=="standard"]),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$ReadingScore[Students$LunchType=="standard"])+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$ReadingScore[Students$LunchType=="standard"]))))

##reading score density chart for both standard and free/reduced Lunch Type
plot21<-ggplot(subset(Students,!is.na(LunchType)),aes(x=ReadingScore,fill=LunchType))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Reading Score by Lunch Type")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing reading score by LunchType
plot22<-ggplot(subset(Students,!is.na(LunchType)),
               aes(y=ReadingScore,x=LunchType,fill=LunchType))+
  geom_boxplot()+
  ggtitle("Boxplot of Reading Score")+
  xlab("Lunch Type")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot11 <- ggarrange(plotg,ploth,
                             plot21,plot22,
                             nrow = 2,
                             ncol = 2)

combined_plot11
~~~

![combined_plot11](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/d2e6a436-bf55-4b29-861a-74c575e76259)
  
~~~  
t.test(ReadingScore~LunchType,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  ReadingScore by LunchType
t = -46.567, df = 21712, p-value < 2.2e-16
alternative hypothesis: true difference in means between group free/reduced and group standard is not equal to 0
95 percent confidence interval:
  -8.322038 -7.649759
sample estimates:
  mean in group free/reduced     mean in group standard 
64.18974                   72.17563 

~~~  
  
**Writing Score**

~~~
##Writing score density chart for free/reduced Lunch Type
ploti<-ggplot(subset(Students,LunchType=="free/reduced"),aes(x=WritingScore))+
  geom_density(fill="#00468BFF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by free/reduced Lunch")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$LunchType=="free/reduced"]),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$WritingScore[Students$LunchType=="free/reduced"])+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$WritingScore[Students$LunchType=="free/reduced"]))))


##Writing score density chart for standard Lunch Type
plotj<-ggplot(subset(Students,LunchType=="standard"),aes(x=WritingScore))+
  geom_density(fill="#ED0000FF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by standard Lunch")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$LunchType=="standard"]),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$WritingScore[Students$LunchType=="standard"])+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$WritingScore[Students$LunchType=="standard"]))))

##Writing score density chart for both standard and free/reduced Lunch Type
plot23<-ggplot(subset(Students,!is.na(LunchType)),aes(x=WritingScore,fill=LunchType))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Writing Score by Lunch Type")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing Writing score by LunchType
plot24<-ggplot(subset(Students,!is.na(LunchType)),
               aes(y=WritingScore,x=LunchType,fill=LunchType))+
  geom_boxplot()+
  ggtitle("Boxplot of Writing Score")+
  xlab("Lunch Type")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot12 <- ggarrange(ploti,plotj,
                             plot23,plot24,
                             nrow = 2,
                             ncol = 2)

combined_plot12
~~~

![combined_plot12](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/64b8bccb-3e30-4bbb-8376-d8edcd29586b)

~~~  
t.test(WritingScore~LunchType,data=Students)
~~~

~~~
  Welch Two Sample t-test

data:  WritingScore by LunchType
t = -49.69, df = 21687, p-value < 2.2e-16
alternative hypothesis: true difference in means between group free/reduced and group standard is not equal to 0
95 percent confidence interval:
  -9.229441 -8.528948
sample estimates:
  mean in group free/reduced     mean in group standard 
62.65052                   71.52972   
~~~  


### test preparation
  
**MathScore**
  
~~~
##math score density chart for completed test preparation Students
plotk<-ggplot(subset(Students,TestPrep=="completed"),aes(x=MathScore))+
  geom_density(fill="#BC3C29FF",alpha=0.5)+
  ggtitle("Distribution of Math Score by completed test preparation ")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$TestPrep=="completed"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$TestPrep=="completed"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$MathScore[Students$TestPrep=="completed"],na.rm = TRUE)))))


##math score density chart for non taken test preparation Students
plotl<-ggplot(subset(Students,TestPrep=="none"),aes(x=MathScore))+
  geom_density(fill="#0072B5FF",alpha=0.5)+
  ggtitle("Distribution of Math Score by none taken test preparation")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$TestPrep=="none"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$TestPrep=="none"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$MathScore[Students$TestPrep=="none"],na.rm = TRUE)))))


##math score density chart for both taken and not taken the test preparation Students
plot25<-ggplot(subset(Students,!is.na(TestPrep)),aes(x=MathScore,fill=TestPrep))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Math Score by test preparation")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_nejm()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing math score by test preparation
plot26<-ggplot(subset(Students,!is.na(TestPrep)),
               aes(y=MathScore,x=TestPrep,fill=TestPrep))+
  geom_boxplot()+
  ggtitle("Boxplot of Math Score")+
  xlab("test preparation")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_nejm()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot13 <- ggarrange(plotk,plotl,
                             plot25,plot26,
                             nrow = 2,
                             ncol = 2)

combined_plot13
~~~

![combined_plot13](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/125013a5-75f2-4ddc-8aec-58dafba1c7ae)

~~~  
t.test(MathScore~TestPrep,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  MathScore by TestPrep
t = 24.49, df = 20459, p-value < 2.2e-16
alternative hypothesis: true difference in means between group completed and group none is not equal to 0
95 percent confidence interval:
  4.229892 4.965888
sample estimates:
  mean in group completed      mean in group none 
69.54666                64.94877   
~~~  

**Reading Score**

~~~
##reading score density chart for completed test preparation Students
plotm<-ggplot(subset(Students,TestPrep=="completed"),aes(x=ReadingScore))+
  geom_density(fill="#f8db27",alpha=0.5)+
  ggtitle("Distribution of Reading Score by completed Lunch")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$TestPrep=="completed"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$ReadingScore[Students$TestPrep=="completed"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$ReadingScore[Students$TestPrep=="completed"],na.rm = TRUE))))

##reading score density chart for non taken test preparation Students
plotn<-ggplot(subset(Students,TestPrep=="none"),aes(x=ReadingScore))+
  geom_density(fill="#2f64d6",alpha=0.5)+
  ggtitle("Distribution of Reading Score by none Lunch")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$TestPrep=="none"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$ReadingScore[Students$TestPrep=="none"],na.rm = TRUE)+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$ReadingScore[Students$TestPrep=="none"],na.rm = TRUE))))

##reading score density chart for both taken and not taken the test preparation Students
plot27<-ggplot(subset(Students,!is.na(TestPrep)),aes(x=ReadingScore,fill=TestPrep))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Reading Score by test preparation")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_simpsons()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing reading score by test preparation
plot28<-ggplot(subset(Students,!is.na(TestPrep)),
               aes(y=ReadingScore,x=TestPrep,fill=TestPrep))+
  geom_boxplot()+
  ggtitle("Boxplot of Reading Score")+
  xlab("test preparation")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_simpsons()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot14 <- ggarrange(plotm,plotn,
                             plot27,plot28,
                             nrow = 2,
                             ncol = 2)

combined_plot14
~~~

![combined_plot14](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/ab6eb901-8a5f-4fca-a2ef-e287024c206b)
  
~~~  
t.test(ReadingScore~TestPrep,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  ReadingScore by TestPrep
t = 37.672, df = 20690, p-value < 2.2e-16
alternative hypothesis: true difference in means between group completed and group none is not equal to 0
95 percent confidence interval:
  6.334264 7.029591
sample estimates:
  mean in group completed      mean in group none 
73.73300                67.05107 

~~~  
  
**Writing Score**

~~~
##Writing score density chart for completed test preparation Students
ploto<-ggplot(subset(Students,TestPrep=="completed"),aes(x=WritingScore))+
  geom_density(fill="#ED0000FF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by completed Lunch")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$TestPrep=="completed"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$WritingScore[Students$TestPrep=="completed"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$WritingScore[Students$TestPrep=="completed"],na.rm = TRUE))))


##Writing score density chart for non taken test preparation Students
plotp<-ggplot(subset(Students,TestPrep=="none"),aes(x=WritingScore))+
  geom_density(fill="#00468BFF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by none Lunch")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$TestPrep=="none"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$WritingScore[Students$TestPrep=="none"],na.rm = TRUE)+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$WritingScore[Students$TestPrep=="none"],na.rm = TRUE))))

##Writing score density chart for both taken and not taken the test preparation Students
plot29<-ggplot(subset(Students,!is.na(TestPrep)),aes(x=WritingScore,fill=TestPrep))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Writing Score by test preparation")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_startrek()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing Writing score by test preparation
plot30<-ggplot(subset(Students,!is.na(TestPrep)),
               aes(y=WritingScore,x=TestPrep,fill=TestPrep))+
  geom_boxplot()+
  ggtitle("Boxplot of Writing Score")+
  xlab("test preparation")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_startrek()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot15 <- ggarrange(ploto,plotp,
                             plot29,plot30,
                             nrow = 2,
                             ncol = 2)

combined_plot15
~~~

![combined_plot15](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/adef9a4a-1deb-4eb1-a4a7-8f2093c3ff66)
  
~~~  
t.test(WritingScore~TestPrep,data=Students)
~~~

~~~
  Welch Two Sample t-test

data:  WritingScore by TestPrep
t = 53.139, df = 20889, p-value < 2.2e-16
alternative hypothesis: true difference in means between group completed and group none is not equal to 0
95 percent confidence interval:
  9.256019 9.965000
sample estimates:
  mean in group completed      mean in group none 
74.70326                65.09276   
~~~  

### Parent Marital Status

**Math score**

~~~
##histogram showing math score by Parent Marital Status
plot31<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#6667AB")+
  facet_grid(~ParentMaritalStatus)+
  ggtitle("Distribution of the Math Score by Parent Marital Status")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing math score by Parent Marital Status
plot32<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),
               aes(y=MathScore,x=ParentMaritalStatus,fill=ParentMaritalStatus))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Math Score by Parent Marital Status")+
  xlab("Parent Marital Status")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_tron()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot16 <- ggarrange(plot31,
                             plot32,
                             nrow = 1,
                             ncol = 2)


combined_plot16
~~~

![combined_plot16](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/0508d659-a954-403b-838b-c48eccb98e61)

The Anova test 

~~~
summary(aov(MathScore~ParentMaritalStatus,data=Students))
~~~

~~~
                       Df  Sum Sq Mean Sq F value Pr(>F)  
ParentMaritalStatus     3    1734   578.1   2.454 0.0613 .
Residuals           29447 6937325   235.6                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1190 observations deleted due to missingness
~~~
 
Tukey's HSD tests

~~~
TukeyHSD(aov(MathScore~ParentMaritalStatus,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MathScore ~ ParentMaritalStatus, data = Students)

$ParentMaritalStatus
                        diff        lwr        upr     p adj
married-divorced -0.03387135 -0.6729338 0.60519112 0.9990995
single-divorced  -0.52549358 -1.2570522 0.20606500 0.2520720
widowed-divorced  0.67766893 -1.0390081 2.39434601 0.7411571
single-married   -0.49162223 -1.0496504 0.06640589 0.1067178
widowed-married   0.71154028 -0.9386711 2.36175166 0.6847732
widowed-single    1.20316251 -0.4850243 2.89134933 0.2587076
~~~

**Reading score**

~~~
##histogram showing reading score by Parent Marital Status
plot33<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#6667AB")+
  facet_grid(~ParentMaritalStatus)+
  ggtitle("Distribution of the Reading Score by Parent Marital Status")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_startrek()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing reading score by Parent Marital Status
plot34<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),
               aes(y=ReadingScore,x=ParentMaritalStatus,fill=ParentMaritalStatus))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Reading Score by Parent Marital Status")+
  xlab("Parent Marital Status")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_startrek()
theme(plot.title = element_text(hjust = 0.5))

combined_plot17 <- ggarrange(plot33,
                             plot34,
                             nrow = 1,
                             ncol = 2)


combined_plot17
~~~

![combined_plot17](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/6cd3eda5-a912-4eec-aa62-76746569690b)

The Anova test 

~~~
summary(aov(ReadingScore~ParentMaritalStatus,data=Students))
~~~

~~~
                       Df  Sum Sq Mean Sq F value Pr(>F)
ParentMaritalStatus     3     769   256.3   1.178  0.316
Residuals           29447 6405186   217.5               
1190 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(ReadingScore~ParentMaritalStatus,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ ParentMaritalStatus, data = Students)

$ParentMaritalStatus
                         diff        lwr       upr     p adj
married-divorced -0.265436258 -0.8794996 0.3486271 0.6830574
single-divorced  -0.497761639 -1.2007028 0.2051795 0.2641922
widowed-divorced -0.003572941 -1.6530963 1.6459504 0.9999999
single-married   -0.232325381 -0.7685243 0.3038735 0.6814359
widowed-married   0.261863317 -1.3237944 1.8475210 0.9743488
widowed-single    0.494188698 -1.1279589 2.1163363 0.8623672
~~~

**Writing score**

~~~
##histogram showing Writing score by Parent Marital Status
plot35<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#6667AB")+
  facet_grid(~ParentMaritalStatus)+
  ggtitle("Distribution of the Writing Score by Parent Marital Status")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing Writing score by Parent Marital Status
plot36<-ggplot(subset(Students,!is.na(ParentMaritalStatus)),
               aes(y=WritingScore,x=ParentMaritalStatus,fill=ParentMaritalStatus))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Writing Score by Parent Marital Status")+
  xlab("Parent Marital Status")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot18 <- ggarrange(plot35,
                             plot36,
                             nrow = 1,
                             ncol = 2)

combined_plot18
~~~

![combined_plot18](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/47e78466-e62b-4510-87af-ed0e570307ad)

The Anova test 

~~~
summary(aov(WritingScore~ParentMaritalStatus,data=Students))
~~~

~~~
                       Df  Sum Sq Mean Sq F value Pr(>F)
ParentMaritalStatus     3    1146   381.8   1.602  0.187
Residuals           29447 7017235   238.3               
1190 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~ParentMaritalStatus,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = WritingScore ~ ParentMaritalStatus, data = Students)

$ParentMaritalStatus
                       diff        lwr       upr     p adj
married-divorced -0.3781654 -1.0208980 0.2645672 0.4304823
single-divorced  -0.6247063 -1.3604661 0.1110536 0.1284122
widowed-divorced -0.2356944 -1.9622302 1.4908415 0.9852020
single-married   -0.2465409 -0.8077737 0.3146920 0.6718589
widowed-married   0.1424710 -1.5172174 1.8021595 0.9962172
widowed-single    0.3890119 -1.3088701 2.0868938 0.9355619

~~~


### Practice Sport

**Math score**

~~~
##histogram showing math score by weekly Practice Sport
plot37<-ggplot(subset(Students,!is.na(PracticeSport)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#86608E")+
  facet_grid(~PracticeSport)+
  ggtitle("Distribution of the Math Score by Practice Sport")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_tron()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing math score by weekly Practice Sport
plot38<-ggplot(subset(Students,!is.na(PracticeSport)),
               aes(y=MathScore,x=PracticeSport,fill=PracticeSport))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Math Score by Practice Sport")+
  xlab("Practice Sport")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_tron()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot19 <- ggarrange(plot37,
                             plot38,
                             nrow = 1,
                             ncol = 2)

combined_plot19
~~~

![combined_plot19](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/c50fc464-30c2-4562-8537-cb77e03584a8)

The Anova test 

~~~
summary(aov(MathScore~PracticeSport,data=Students))
~~~

~~~
                 Df  Sum Sq Mean Sq F value Pr(>F)    
PracticeSport     2   41747   20874   88.85 <2e-16 ***
Residuals     30007 7049503     235                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
631 observations deleted due to missingness
~~~
 
Tukey's HSD tests

~~~
TukeyHSD(aov(MathScore~PracticeSport,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MathScore ~ PracticeSport, data = Students)

$PracticeSport
                         diff       lwr       upr p adj
regularly-never      3.668076  3.003356  4.332796     0
sometimes-never      2.103752  1.465696  2.741807     0
sometimes-regularly -1.564324 -2.016418 -1.112231     0
~~~

**Reading score**

~~~
##histogram showing reading score by weekly Practice Sport
plot39<-ggplot(subset(Students,!is.na(PracticeSport)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#86608E")+
  facet_grid(~PracticeSport)+
  ggtitle("Distribution of the Reading Score by Practice Sport")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_startrek()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing reading score by weekly Practice Sport
plot40<-ggplot(subset(Students,!is.na(PracticeSport)),
               aes(y=ReadingScore,x=PracticeSport,fill=PracticeSport))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Reading Score by Practice Sport")+
  xlab("Practice Sport")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_startrek()
  theme(plot.title = element_text(hjust = 0.5))

combined_plot20 <- ggarrange(plot39,
                             plot40,
                             nrow = 1,
                             ncol = 2)


combined_plot20
~~~

![combined_plot20](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/8813d70a-eeb4-4c73-912f-dbe5ef7b2c89)

The Anova test 

~~~
summary(aov(ReadingScore~PracticeSport,data=Students))
~~~

~~~
                 Df  Sum Sq Mean Sq F value  Pr(>F)    
PracticeSport     2    8063    4031    18.5 9.3e-09 ***
Residuals     30007 6537113     218                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
631 observations deleted due to missingness
~~~
 
Tukey's HSD tests

~~~
TukeyHSD(aov(ReadingScore~PracticeSport,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ PracticeSport, data = Students)

$PracticeSport
                          diff        lwr        upr     p adj
regularly-never      1.6053563  0.9652493  2.2454632 0.0000000
sometimes-never      0.9036444  0.2892148  1.5180741 0.0016432
sometimes-regularly -0.7017118 -1.1370654 -0.2663583 0.0004648
~~~

**Writing score**

~~~
##histogram showing writing score by weekly Practice Sport
plot41<-ggplot(subset(Students,!is.na(PracticeSport)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#86608E")+
  facet_grid(~PracticeSport)+
  ggtitle("Distribution of the Writing Score by Practice Sport")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing writing score by weekly Practice Sport
plot42<-ggplot(subset(Students,!is.na(PracticeSport)),
               aes(y=WritingScore,x=PracticeSport,fill=PracticeSport))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Writing Score by Practice Sport")+
  xlab("Practice Sport")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot21 <- ggarrange(plot41,
                             plot42,
                             nrow = 1,
                             ncol = 2)

combined_plot21
~~~

![combined_plot21](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/9e0d2121-f87b-4f6b-81d5-96be0b16e521)

The Anova test 

~~~
summary(aov(WritingScore~PracticeSport,data=Students))
~~~

~~~
                 Df  Sum Sq Mean Sq F value Pr(>F)    
PracticeSport     2   31381   15690   66.02 <2e-16 ***
Residuals     30007 7131917     238                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
631 observations deleted due to missingness
~~~
 
Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~PracticeSport,data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = WritingScore ~ PracticeSport, data = Students)

$PracticeSport
                         diff        lwr       upr p adj
regularly-never      3.081275  2.4126810  3.749870     0
sometimes-never      1.549711  0.9079365  2.191485     0
sometimes-regularly -1.531565 -1.9862931 -1.076836     0
~~~

### If It's the First Child

**Math score**
~~~
##math score density chart for the family First Child Students
plotq<-ggplot(subset(Students,IsFirstChild=="yes"),aes(x=MathScore))+
  geom_density(fill="#EE000099",alpha=0.5)+
  ggtitle("Distribution of Math Score by If It's the First Child")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$IsFirstChild=="yes"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$IsFirstChild=="yes"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$MathScore[Students$IsFirstChild=="yes"],na.rm = TRUE)))))

##math score density chart for not the family First Child Students
plotr<-ggplot(subset(Students,IsFirstChild=="no"),aes(x=MathScore))+
  geom_density(fill="#3B499299",alpha=0.5)+
  ggtitle("Distribution of Math Score by if it isn't the First Child")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$IsFirstChild=="no"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="808080",
            aes(x=mean(Students$MathScore[Students$IsFirstChild=="no"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$MathScore[Students$IsFirstChild=="no"],na.rm = TRUE)))))

##math score density chart for both first and not first family child Students
plot43<-ggplot(subset(Students,!is.na(IsFirstChild)),aes(x=MathScore,fill=IsFirstChild))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Math Score by if it is the First Child")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing math score by if the Students are their family first child
plot44<-ggplot(subset(Students,!is.na(IsFirstChild)),
               aes(y=MathScore,x=IsFirstChild,fill=IsFirstChild))+
  geom_boxplot(alpha=0.5)+
  ggtitle("Boxplot of Math Score")+
  xlab("if it is the First Child")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot22 <- ggarrange(plotq,plotr,
                             plot43,plot44,
                             nrow = 2,
                             ncol = 2)

combined_plot22
~~~

![combined_plot22](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/2c8988b7-2d84-4b13-a325-37bb1702e42f)

~~~
t.test(MathScore~IsFirstChild,data=Students)
~~~

~~~
	Welch Two Sample t-test

data:  MathScore by IsFirstChild
t = -2.6779, df = 22399, p-value = 0.007413
alternative hypothesis: true difference in means between group no and group yes is not equal to 0
95 percent confidence interval:
 -0.8552532 -0.1323731
sample estimates:
 mean in group no mean in group yes 
         66.24683          66.74065 
~~~

**Reading Score**

~~~
##reading score density chart for the family First Child Students
plots<-ggplot(subset(Students,IsFirstChild=="yes"),aes(x=ReadingScore))+
  geom_density(fill="#FF7F0EFF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by If It's the First Child")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$IsFirstChild=="yes"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$ReadingScore[Students$IsFirstChild=="yes"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$ReadingScore[Students$IsFirstChild=="yes"],na.rm = TRUE))))

##reading score density chart for not the family First Child Students
plott<-ggplot(subset(Students,IsFirstChild=="no"),aes(x=ReadingScore))+
  geom_density(fill="#1F77B4FF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by if it isn't the First Child")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$IsFirstChild=="no"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$ReadingScore[Students$IsFirstChild=="no"],na.rm = TRUE)+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$ReadingScore[Students$IsFirstChild=="no"],na.rm = TRUE))))

##reading score density chart for both first and not first family child Students
plot45<-ggplot(subset(Students,!is.na(IsFirstChild)),aes(x=ReadingScore,fill=IsFirstChild))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Reading Score by if it is the First Child")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing reading score by if the Students are their family first child
plot46<-ggplot(subset(Students,!is.na(IsFirstChild)),
               aes(y=ReadingScore,x=IsFirstChild,fill=IsFirstChild))+
  geom_boxplot()+
  ggtitle("Boxplot of Reading Score")+
  xlab("if it is the First Child")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot23 <- ggarrange(plots,plott,
                             plot45,plot46,
                             nrow = 2,
                             ncol = 2)

combined_plot23
~~~

![combined_plot23](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/f27ae14f-aa25-4619-a738-c87d2c8b3677)

~~~  
t.test(ReadingScore~IsFirstChild,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  ReadingScore by IsFirstChild
t = -2.3064, df = 22274, p-value = 0.0211
alternative hypothesis: true difference in means between group no and group yes is not equal to 0
95 percent confidence interval:
  -0.75832026 -0.06155853
sample estimates:
  mean in group no mean in group yes 
69.13261          69.54255 

~~~  

  
**Writing Score**

~~~
##Writing score density chart for the family First Child Students
plotu<-ggplot(subset(Students,IsFirstChild=="yes"),aes(x=WritingScore))+
  geom_density(fill="#ED0000FF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by If It's the First Child")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$IsFirstChild=="yes"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#808080",
            aes(x=mean(Students$WritingScore[Students$IsFirstChild=="yes"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$WritingScore[Students$IsFirstChild=="yes"],na.rm = TRUE))))


##Writing score density chart for not the family First Child Students
plotv<-ggplot(subset(Students,IsFirstChild=="no"),aes(x=WritingScore))+
  geom_density(fill="#00468BFF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by if it isn't the First Child")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$IsFirstChild=="no"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$WritingScore[Students$IsFirstChild=="no"],na.rm = TRUE)+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$WritingScore[Students$IsFirstChild=="no"],na.rm = TRUE))))

##Writing score density chart for both first and not first family child Students
plot47<-ggplot(subset(Students,!is.na(IsFirstChild)),aes(x=WritingScore,fill=IsFirstChild))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Writing Score by if it is the First Child")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing Writing score by if the Students are their family first child
plot48<-ggplot(subset(Students,!is.na(IsFirstChild)),
               aes(y=WritingScore,x=IsFirstChild,fill=IsFirstChild))+
  geom_boxplot()+
  ggtitle("Boxplot of Writing Score")+
  xlab("if it is the First Child")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot24 <- ggarrange(plotu,plotv,
                             plot47,plot48,
                             nrow = 2,
                             ncol = 2)

combined_plot24
~~~

![combined_plot24](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/6b4bb047-e1fc-4d11-8e24-5db5549f92bc)  

~~~  
t.test(WritingScore~IsFirstChild,data=Students)
~~~

~~~
  Welch Two Sample t-test

data:  WritingScore by IsFirstChild
t = -1.8693, df = 22312, p-value = 0.0616
alternative hypothesis: true difference in means between group no and group yes is not equal to 0
95 percent confidence interval:
  -0.71207967  0.01688461
sample estimates:
  mean in group no mean in group yes 
68.21089          68.55848   
~~~  

### number of siblings

**Math score**

~~~
##histogram showing math score by number of siblings
plot49<-ggplot(subset(Students,!is.na(NrSiblings)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#C38EC7")+
  facet_grid(~NrSiblings)+
  ggtitle("Distribution of the Math Score by number of siblings")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_tron()+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing math score by number of siblings
plot50<-ggplot(subset(Students,!is.na(NrSiblings)),
               aes(y=MathScore,x=NrSiblings,fill=as.character(NrSiblings)))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Math Score by number of siblings")+
  xlab("number of siblings")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_tron()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

combined_plot25 <- ggarrange(plot49,
                             plot50,
                             nrow = 1,
                             ncol = 2)

combined_plot25
~~~

![combined_plot25](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/058d2111-625a-42ea-adad-b8a006ae234f)

The Anova test 

~~~
summary(aov(MathScore~NrSiblings,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)
NrSiblings      1      10    9.78   0.042  0.839
Residuals   29067 6846676  235.55               
1572 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(MathScore~as.character(NrSiblings),data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MathScore ~ as.character(NrSiblings), data = Students)

$`as.character(NrSiblings)`
           diff        lwr       upr     p adj
1-0 -0.34555336 -1.3277758 0.6366691 0.9636935
2-0 -0.26451522 -1.2745179 0.7454875 0.9934838
3-0 -0.10035700 -1.1274018 0.9266878 0.9999904
4-0 -0.57395422 -1.7727796 0.6248711 0.8331889
5-0 -0.18914592 -1.7190875 1.3407957 0.9999518
6-0 -0.90223040 -3.7069729 1.9025121 0.9779797
7-0  0.79567133 -2.0569465 3.6482891 0.9904520
2-1  0.08103814 -0.6818693 0.8439456 0.9999829
3-1  0.24519637 -0.5401337 1.0305265 0.9814444
4-1 -0.22840085 -1.2279530 0.7711513 0.9972123
5-1  0.15640745 -1.2229560 1.5357709 0.9999733
6-1 -0.55667704 -3.2822050 2.1688509 0.9986394
7-1  1.14122469 -1.6335457 3.9159951 0.9178919
3-2  0.16415822 -0.6556514 0.9839678 0.9988029
4-2 -0.30943900 -1.3363027 0.7174247 0.9848994
5-2  0.07536930 -1.3239120 1.4746506 0.9999998
6-2 -0.63771518 -3.3733773 2.0979469 0.9968380
7-2  1.06018655 -1.7245388 3.8449119 0.9446137
4-3 -0.47359722 -1.5172277 0.5700333 0.8686634
5-3 -0.08878892 -1.5004205 1.3228426 0.9999995
6-3 -0.80187341 -3.5438732 1.9401263 0.9873412
7-3  0.89602833 -1.8949232 3.6869799 0.9782256
5-4  0.38480830 -1.1563162 1.9259328 0.9951428
6-4 -0.32827619 -3.1391344 2.4825820 0.9999673
7-4  1.36962555 -1.4890056 4.2282567 0.8326499
6-5 -0.71308449 -3.6803160 2.2541470 0.9961722
7-5  0.98481724 -2.0277084 3.9973429 0.9758684
7-6  1.69790173 -2.1232460 5.5190494 0.8808437
~~~

**Reading test**

~~~
##histogram showing reading score by number of siblings
plot51<-ggplot(subset(Students,!is.na(NrSiblings)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#C38EC7")+
  facet_grid(~NrSiblings)+
  ggtitle("Distribution of the Reading Score by number of siblings")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_startrek()
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing reading score by number of siblings
plot52<-ggplot(subset(Students,!is.na(NrSiblings)),
               aes(y=ReadingScore,x=NrSiblings,fill=as.character(NrSiblings)))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Reading Score by number of siblings")+
  xlab("number of siblings")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_startrek()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title =element_blank())

combined_plot26 <- ggarrange(plot51,
                             plot52,
                             nrow = 1,
                             ncol = 2)

combined_plot26
~~~

![combined_plot26](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/528e685c-62e7-418e-bbb5-d3e0a8adc5a9)

The Anova test 

~~~
summary(aov(ReadingScore~NrSiblings,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)
NrSiblings      1       5    4.83   0.022  0.882
Residuals   29067 6324482  217.58               
1572 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(ReadingScore~as.character(NrSiblings),data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ as.character(NrSiblings), data = Students)

$`as.character(NrSiblings)`
           diff        lwr       upr     p adj
1-0 -0.28871500 -1.2327682 0.6553382 0.9835414
2-0 -0.07579432 -1.0465482 0.8949595 0.9999979
3-0 -0.05965259 -1.0467863 0.9274811 0.9999997
4-0 -0.40364334 -1.5558822 0.7485955 0.9645298
5-0 -0.09402411 -1.5645120 1.3764638 0.9999995
6-0 -0.74648749 -3.4422373 1.9492623 0.9908591
7-0  0.28036670 -2.4613980 3.0221314 0.9999868
2-1  0.21292068 -0.5203401 0.9461815 0.9878616
3-1  0.22906240 -0.5257497 0.9838745 0.9842664
4-1 -0.11492834 -1.0756378 0.8457811 0.9999615
5-1  0.19469088 -1.1310704 1.5204522 0.9998457
6-1 -0.45777249 -3.0773861 2.1618411 0.9995087
7-1  0.56908170 -2.0978608 3.2360242 0.9981970
3-2  0.01614173 -0.7718100 0.8040934 1.0000000
4-2 -0.32784902 -1.3148086 0.6591106 0.9735892
5-2 -0.01822979 -1.3631348 1.3266752 1.0000000
6-2 -0.67069317 -3.3000471 1.9586608 0.9944581
7-2  0.35616102 -2.3203495 3.0326716 0.9999206
4-3 -0.34399075 -1.3470656 0.6590841 0.9684519
5-3 -0.03437152 -1.3911469 1.3224039 1.0000000
6-3 -0.68683490 -3.3222802 1.9486104 0.9936766
7-3  0.34001930 -2.3424756 3.0225141 0.9999429
5-4  0.30961923 -1.1716170 1.7908555 0.9984206
6-4 -0.34284415 -3.0444721 2.3587838 0.9999424
7-4  0.68401004 -2.0635343 3.4315544 0.9952313
6-5 -0.65246338 -3.5043879 2.1994612 0.9971909
7-5  0.37439082 -2.5210677 3.2698494 0.9999346
7-6  1.02685419 -2.6458033 4.6995117 0.9903140
~~~

**Writing score**

~~~
##histogram showing writing score by number of siblings
plot53<-ggplot(subset(Students,!is.na(NrSiblings)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#C38EC7")+
  facet_grid(~NrSiblings)+
  ggtitle("Distribution of the Writing Score by number of siblings")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot showing writing score by number of siblings
plot54<-ggplot(subset(Students,!is.na(NrSiblings)),
               aes(y=WritingScore,x=NrSiblings,fill=as.character(NrSiblings)))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Writing Score by number of siblings")+
  xlab("number of siblings")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title =element_blank())

combined_plot27 <- ggarrange(plot53,
                             plot54,
                             nrow = 1,
                             ncol = 2)

combined_plot27
~~~

![combined_plot27](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/0206d336-c7b5-4b16-94b7-77269d885c1e)

The Anova test 

~~~
summary(aov(WritingScore~NrSiblings,data=Students))
~~~

~~~
               Df  Sum Sq Mean Sq F value Pr(>F)
NrSiblings      1      45    45.2    0.19  0.663
Residuals   29067 6926890   238.3               
1572 observations deleted due to missingness
~~~

Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~as.character(NrSiblings),data=Students))
~~~

~~~
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = WritingScore ~ as.character(NrSiblings), data = Students)

$`as.character(NrSiblings)`
           diff        lwr       upr     p adj
1-0 -0.50117038 -1.4891071 0.4867664 0.7871169
2-0 -0.22398226 -1.2398608 0.7918963 0.9977779
3-0 -0.09601728 -1.1290371 0.9370026 0.9999932
4-0 -0.67307099 -1.8788707 0.5327288 0.6927091
5-0 -0.46393964 -2.0027820 1.0749027 0.9848576
6-0 -0.88558824 -3.7066478 1.9354714 0.9808545
7-0  0.23973890 -2.6294746 3.1089524 0.9999967
2-1  0.27718812 -0.4901577 1.0445339 0.9580940
3-1  0.40515311 -0.3847458 1.1950520 0.7773871
4-1 -0.17190061 -1.1772679 0.8334667 0.9995745
5-1  0.03723074 -1.3501575 1.4246190 1.0000000
6-1 -0.38441786 -3.1258021 2.3569664 0.9998869
7-1  0.74090928 -2.0500039 3.5318224 0.9929210
3-2  0.12796498 -0.6966140 0.9525440 0.9997767
4-2 -0.44908873 -1.4819264 0.5837489 0.8924127
5-2 -0.23995738 -1.6473792 1.1674645 0.9995825
6-2 -0.66160598 -3.4131833 2.0899714 0.9961596
7-2  0.46372116 -2.3372048 3.2646472 0.9996564
4-3 -0.57705372 -1.6267557 0.4726483 0.7092954
5-3 -0.36792236 -1.7877664 1.0519216 0.9938944
6-3 -0.78957097 -3.5475228 1.9683809 0.9888419
7-3  0.33575617 -2.4714323 3.1429447 0.9999615
5-4  0.20913135 -1.3409590 1.7592217 0.9999128
6-4 -0.21251725 -3.0397282 2.6146937 0.9999984
7-4  0.91280989 -1.9624519 3.7880717 0.9795770
6-5 -0.42164861 -3.4061426 2.5628454 0.9998811
7-5  0.70367854 -2.3263731 3.7337302 0.9969123
7-6  1.12532714 -2.7180509 4.9687051 0.9872501
~~~

### Transport Means
  
**Math Score**
  
~~~
## math Score density chart for students using private cars as transport means
plotw<-ggplot(subset(Students,TransportMeans=="private"),aes(x=MathScore))+
  geom_density(fill="#EE000099",alpha=0.5)+
  ggtitle("Distribution of Math Score by private Transport Means")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$TransportMeans=="private"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="5E5A80",
            aes(x=mean(Students$MathScore[Students$TransportMeans=="private"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = "
                             ,round(mean(Students$MathScore[Students$TransportMeans=="private"],na.rm = TRUE)))))

##math Score density chart for students using the school bus as transport
plotx<-ggplot(subset(Students,TransportMeans=="school_bus"),aes(x=MathScore))+
  geom_density(fill="#3B499299",alpha=0.5)+
  ggtitle("Distribution of Math Score by school bus Transport Means")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$MathScore[Students$TransportMeans=="school_bus"],na.rm = TRUE))+
  geom_text(size=4.2,
            col="5E5A80",
            aes(x=mean(Students$MathScore[Students$TransportMeans=="school_bus"],na.rm = TRUE)+5,
                y=0.01,
                label=paste0("mean = ",
                             round(mean(Students$MathScore[Students$TransportMeans=="school_bus"],na.rm = TRUE)))))

##math Score density chart for students using both school buses and private cars as transport means
plot55<-ggplot(subset(Students,!is.na(TransportMeans)),aes(x=MathScore,fill=TransportMeans))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Math Score by Transport Means")+
  xlab("Math Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot of math score by Transport Means
plot56<-ggplot(subset(Students,!is.na(TransportMeans)),
               aes(y=MathScore,x=TransportMeans,fill=TransportMeans))+
  geom_boxplot(alpha=0.5)+
  ggtitle("Boxplot of Math Score")+
  xlab("Transport Means")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_aaas()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot28 <- ggarrange(plotw,plotx,
                             plot55,plot56,
                             nrow = 2,
                             ncol = 2)

combined_plot28
~~~

![combined_plot28](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/c190348e-f45f-4197-b858-ed29296a0629)

~~~  
t.test(MathScore~TransportMeans,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  MathScore by TransportMeans
t = -0.86411, df = 24104, p-value = 0.3875
alternative hypothesis: true difference in means between group private and group school_bus is not equal to 0
95 percent confidence interval:
  -0.533655  0.207090
sample estimates:
  mean in group private mean in group school_bus 
66.51135                 66.67464   
~~~  

**Reading Score**

~~~
## reading Score density chart for students using private cars as transport means
ploty<-ggplot(subset(Students,TransportMeans=="private"),aes(x=ReadingScore))+
  geom_density(fill="#FF7F0EFF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by private Transport Means")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$TransportMeans=="private"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#5E5A80",
            aes(x=mean(Students$ReadingScore[Students$TransportMeans=="private"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$ReadingScore[Students$TransportMeans=="private"],na.rm = TRUE))))

##reading Score density chart for students using the school bus as transport
plotz<-ggplot(subset(Students,TransportMeans=="school_bus"),aes(x=ReadingScore))+
  geom_density(fill="#1F77B4FF",alpha=0.5)+
  ggtitle("Distribution of Reading Score by school bus Transport Means")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$ReadingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            aes(x=mean(Students$ReadingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE)+5,
                y=0.01),
            col="#5E5A80",
            label=paste0("mean = ",
                         round(mean(Students$ReadingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE))))
##reading Score density chart for students using both school buses and private cars as transport means
plot57<-ggplot(subset(Students,!is.na(TransportMeans)),aes(x=ReadingScore,fill=TransportMeans))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Reading Score by Transport Means")+
  xlab("Reading Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot of reading score by Transport Means
plot58<-ggplot(subset(Students,!is.na(TransportMeans)),
               aes(y=ReadingScore,x=TransportMeans,fill=TransportMeans))+
  geom_boxplot()+
  ggtitle("Boxplot of Reading Score")+
  xlab("Transport Means")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_d3()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot29 <- ggarrange(ploty,plotz,
                             plot57,plot58,
                             nrow = 2,
                             ncol = 2)

combined_plot29
~~~

![combined_plot29](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/1bebeee0-33c6-44de-9edf-e6843c23a1a3)

~~~  
t.test(ReadingScore~TransportMeans,data=Students)
~~~
  
~~~
  Welch Two Sample t-test

data:  ReadingScore by TransportMeans
t = 0.14425, df = 24180, p-value = 0.8853
alternative hypothesis: true difference in means between group private and group school_bus is not equal to 0
95 percent confidence interval:
  -0.3292769  0.3815924
sample estimates:
  mean in group private mean in group school_bus 
69.47236                 69.44621 
~~~  
  
**Writing Score**

~~~
## Writing Score density chart for students using private cars as transport means
plotaa<-ggplot(subset(Students,TransportMeans=="private"),aes(x=WritingScore))+
  geom_density(fill="#ED0000FF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by private Transport Means")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$TransportMeans=="private"],na.rm = TRUE),
             col="#808080")+
  geom_text(size=4.2,
            col="#5E5A80",
            aes(x=mean(Students$WritingScore[Students$TransportMeans=="private"],na.rm = TRUE)+5,
                y=0.01),
            label=paste0("mean = "
                         ,round(mean(Students$WritingScore[Students$TransportMeans=="private"],na.rm = TRUE))))


##Writing Score density chart for students using the school bus as transport
plotbb<-ggplot(subset(Students,TransportMeans=="school_bus"),aes(x=WritingScore))+
  geom_density(fill="#00468BFF",alpha=0.5)+
  ggtitle("Distribution of Writing Score by school bus Transport Means")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept=mean(Students$WritingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE),
             col="#5E5A80")+
  geom_text(size=4.2,
            aes(x=mean(Students$WritingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE)+5,
                y=0.01),
            col="#808080",
            label=paste0("mean = ",
                         round(mean(Students$WritingScore[Students$TransportMeans=="school_bus"],na.rm = TRUE))))

##Writing Score density chart for students using both school buses and private cars as transport means
plot59<-ggplot(subset(Students,!is.na(TransportMeans)),aes(x=WritingScore,fill=TransportMeans))+
  geom_density(alpha=0.5)+
  ggtitle("Distribution of Writing Score by Transport Means")+
  xlab("Writing Score")+
  ylab("Density")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.08,0.87))+
  theme(legend.title=element_blank())

##boxplot of writing score by Transport Means
plot60<-ggplot(subset(Students,!is.na(TransportMeans)),
               aes(y=WritingScore,x=TransportMeans,fill=TransportMeans))+
  geom_boxplot()+
  ggtitle("Boxplot of Writing Score")+
  xlab("Transport Means")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_lancet()+
  theme(plot.title = element_text(hjust = 0.5))


combined_plot30 <- ggarrange(plotaa,plotbb,
                             plot59,plot60,
                             nrow = 2,
                             ncol = 2)

combined_plot30
~~~

![combined_plot30](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/cd4dc603-cb72-4bae-885b-92f22b3d9ea1)
  
~~~  
t.test(WritingScore~TransportMeans,data=Students)
~~~

~~~
  Welch Two Sample t-test

data:  WritingScore by TransportMeans
t = 0.09079, df = 24150, p-value = 0.9277
alternative hypothesis: true difference in means between group private and group school_bus is not equal to 0
95 percent confidence interval:
  -0.3550136  0.3894992
sample estimates:
  mean in group private mean in group school_bus 
68.50959                 68.49235   
~~~  

### Weekly Study Hours

**math score**

~~~
##histogram showing math score by Weekly Study Hours
plot61<-ggplot(subset(Students,!is.na(WklyStudyHours)),aes(x=MathScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#D291BC")+
  facet_grid(~WklyStudyHours)+
  ggtitle("Distribution of the Math Score by Weekly Study Hours")+
  xlab("Math Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing math score by Weekly Study Hours
plot62<-ggplot(subset(Students,!is.na(WklyStudyHours)),
               aes(y=MathScore,x=WklyStudyHours,fill=WklyStudyHours))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Math Score by Weekly Study Hours")+
  xlab("Weekly Study Hours")+
  ylab("Math Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set2")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot31 <- ggarrange(plot61,
                             plot62,
                             nrow = 1,
                             ncol = 2)


combined_plot31
~~~~

![combined_plot31](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/b346eeb8-62b4-4d33-baa8-4e12b62bbde9)

The Anova test 

~~~  
summary(aov(MathScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Df  Sum Sq Mean Sq F value Pr(>F)    
WklyStudyHours     2   57599   28799   123.1 <2e-16 ***
  Residuals      29683 6946059     234                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
955 observations deleted due to missingness  
~~~ 

Tukey's HSD tests

~~~    
TukeyHSD(aov(MathScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = MathScore ~ WklyStudyHours, data = Students)

$WklyStudyHours
diff       lwr       upr p adj
> 10-< 5     4.116296  3.481373  4.751219     0
5 - 10-< 5   2.290132  1.805207  2.775057     0
5 - 10-> 10 -1.826164 -2.397317 -1.255011     0  
~~~ 

**Reading score**

~~~
##histogram showing reading score by Weekly Study Hours 
plot63<-ggplot(subset(Students,!is.na(WklyStudyHours)),aes(x=ReadingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#D291BC")+
  facet_grid(~WklyStudyHours)+
  ggtitle("Distribution of the Reading Score by Weekly Study Hours")+
  xlab("Reading Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_jco()
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing reading score by Weekly Study Hours 
plot64<-ggplot(subset(Students,!is.na(WklyStudyHours)),
               aes(y=ReadingScore,x=WklyStudyHours,fill=WklyStudyHours))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Reading Score by Weekly Study Hours")+
  xlab("Weekly Study Hours")+
  ylab("Reading Score")+
  theme_solarized()+
  scale_fill_jco()
theme(plot.title = element_text(hjust = 0.5))

combined_plot32 <- ggarrange(plot63,
                             plot64,
                             nrow = 1,
                             ncol = 2)

combined_plot32
~~~

![combined_plot32](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/e016c0a0-7837-4d6f-9c74-7a3269c186f5)

The Anova test 

~~~    
summary(aov(ReadingScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Df  Sum Sq Mean Sq F value Pr(>F)    
WklyStudyHours     2   18268    9134   42.09 <2e-16 ***
  Residuals      29683 6441711     217                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
955 observations deleted due to missingness  
~~~

Tukey's HSD tests

~~~    
TukeyHSD(aov(ReadingScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = ReadingScore ~ WklyStudyHours, data = Students)

$WklyStudyHours
diff       lwr        upr     p adj
> 10-< 5     2.1893014  1.577863  2.8007394 0.0000000
5 - 10-< 5   1.4843968  1.017409  1.9513850 0.0000000
5 - 10-> 10 -0.7049045 -1.254931 -0.1548778 0.0075197  
~~~ 

**writing score**
  
~~~
##histogram showing writing score by Weekly Study Hours
plot65<-ggplot(subset(Students,!is.na(WklyStudyHours)),aes(x=WritingScore))+
  geom_histogram(alpha=0.5,bins = 10,fill="#D291BC")+
  facet_grid(~WklyStudyHours)+
  ggtitle("Distribution of the Writing Score by Weekly Study Hours")+
  xlab("Writing Score")+
  ylab("number of students")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))

##boxplot showing writing score by Weekly Study Hours
plot66<-ggplot(subset(Students,!is.na(WklyStudyHours)),
               aes(y=WritingScore,x=WklyStudyHours,fill=WklyStudyHours))+
  geom_boxplot(na.rm = TRUE)+
  ggtitle("Boxplot of the Writing Score by Weekly Study Hours")+
  xlab("Weekly Study Hours")+
  ylab("Writing Score")+
  theme_solarized()+
  scale_fill_brewer(palette="Set1")+
  theme(plot.title = element_text(hjust = 0.5))

combined_plot33 <- ggarrange(plot65,
                             plot66,
                             nrow = 1,
                             ncol = 2)

combined_plot33
~~~

![combined_plot33](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/08e420d7-1cf4-4ef6-82a0-d2b1e02d9336)

The Anova test 

~~~  
summary(aov(WritingScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Df  Sum Sq Mean Sq F value Pr(>F)    
WklyStudyHours     2   24913   12457   52.45 <2e-16 ***
  Residuals      29683 7049675     237                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
955 observations deleted due to missingness  
~~~  

Tukey's HSD tests

~~~
TukeyHSD(aov(WritingScore~WklyStudyHours,data=Students))
~~~
  
~~~
  Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = WritingScore ~ WklyStudyHours, data = Students)

$WklyStudyHours
diff       lwr        upr   p adj
> 10-< 5     2.687586  2.047945  3.3272271 0.0e+00
5 - 10-< 5   1.546088  1.057559  2.0346163 0.0e+00
5 - 10-> 10 -1.141498 -1.716895 -0.5661008 9.9e-06
~~~  

## Correlation analysis

Let's see the correlation matrix of the dataset

~~~
model.matrix(~0+., data=select(Students,-X)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
~~~

![cor_plot](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/4e7fc0f4-b6d0-4769-a5bd-d82c92b35e7d)

correlation table between math, reading, and writing scores

~~~
cor(Students[,13:15])
~~~

~~~
             MathScore ReadingScore WritingScore
MathScore    1.0000000    0.8178249    0.8071182
ReadingScore 0.8178249    1.0000000    0.9525844
WritingScore 0.8071182    0.9525844    1.0000000
~~~

plotting The relationships between math, reading, and writing scores

~~~
##plotting The relation between math and reading scores
plot67<-ggplot(Students,aes(x=MathScore,y=ReadingScore))+
  geom_point(alpha=0.5,col="#95B9C7")+
  theme_solarized()+
  geom_smooth(method = "loess",col="#1AA260")+
  ggtitle("The relation between math and reading scores")

##plotting The relation between math and writing scores
plot68<-ggplot(Students,aes(x=MathScore,y=WritingScore))+
  geom_point(alpha=0.5,col="#95B9C7")+
  theme_solarized()+
  geom_smooth(method = "loess",col="#1AA260")+
  ggtitle("The relation between math and writing scores")

##plotting The relation between writing and reading scores
plot69<-ggplot(Students,aes(x=WritingScore,y=ReadingScore))+
  geom_point(alpha=0.5,col="#95B9C7")+
  theme_solarized()+
  geom_smooth(method = "loess",col="#1AA260")+
  ggtitle("The relation between writing and reading scores")


combined_plot34 <- ggarrange(plot67,plot68,plot69,
                            nrow = 1,
                            ncol = 3)

combined_plot34
~~~

![combined_plot34](https://github.com/mahmoud26202620/Students-Exam-Anova-Analysis/assets/41892582/66fe867a-feef-40ed-9fa1-a99a75e3aae8)

# The results

- It seems that gender matters when it comes to exam scores; math scores tend to favour males, and females tend to outperform in reading and writing tests.

- Exam results and academic achievement may be impacted by genetic variables, which are reflected here by the ethnic group. All of the disparities between the ethnic groups, with the exception of those between groups A and B, are statistically significant, in the same order as the scores in math, reading, and writing.

- Obviously, parents with higher levels of education have sons who perform better in school, whether because of genetic factors, economic factors, or the parents own interest in that field pushing their sons to be better at it.

- Participating in a sport helps exam performance, maybe because it helps improve the cognitive and memory functions of the brain or because of its Psychological benefits.

- Of course, the more time spent studying, the better the impact on exam scores. Also, the students who took test preparation are doing better than the students who didn't.

- The fact that you are the older son in your family might provide you a tiny advantage over your academic accomplishments; nevertheless, neither the number of your siblings nor the marital status of your parents seems to matter.

- There are other economic factors that may affect the exam scores, like the type of lunch, which may  come from the economic ability of the family, and other factors that seem to be ineffective, like the transport means the students use to go to school.

- Test results in reading, writing, and math have a strong relationship with one another; math test results have a less clear relationship with them.
