#1.Generate frequency distribution, relative frequency distribution, and percent frequency distribution for variable “cyl”.
cyl_freq <- mtcars_df %>%
  group_by(cyl)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n),percent_freq=(n/ sum(n)) * 100)
head(cyl_freq)
#2.Draw bar chart and pie chart of variable “cyl”.
ggplot(data=mtcars_df)+
  geom_bar(mapping=aes(x=cyl))
barplot(cyl_freq$n, names.arg = cyl_freq$cyl,
        xlab = "cyl", ylab = "Frequency", main = "Bar Chart of cyl")
pie(cyl_freq$n, main = "Pie Chart of cyl",
    labels = paste(cyl_freq$cyl, " (", cyl_freq$percent_freq, "%)", sep = ""))
#3.Determine the appropriate number of bins for variable “mpg” using Sturge’s method, and generate the corresponding frequency distribution, relative frequency distribution, percent frequency distribution, cumulative frequency distribution, cumulative relative frequency distribution, and cumulative percent distribution.
mpg_bin_num<-nclass.Sturges(mtcars_df$mpg)
mtcars_df$mpg_bin <- cut(mtcars_df$mpg, mpg_bin_num)
mpg_freq <- data.frame(table(mtcars_df$mpg_bin))
names(mpg_freq)[1] <- "mpg_freq"
mpg_freq$Rel_Freq <- mpg_freq$Freq / sum(mpg_freq$Freq)
mpg_freq$Pct_Freq <- 100.00 * mpg_freq$Rel_Freq
mpg_freq$Cum_Freq <- cumsum(mpg_freq$Freq)
mpg_freq$Cum_Rel_Freq <- cumsum(mpg_freq$Rel_Freq)
mpg_freq$Cum_Pct_Freq <- cumsum(mpg_freq$Pct_Freq)
mpg_freq
#4.Draw histogram of variable “mpg”.
hist(mtcars_df$mpg,xlab = "mpg",
     main = "Histogram of mpg")
#5.Determine the appropriate number of bins for variable “mpg” using Sturge’s method, then generate crosstabulation for variables “mpg” and “gear”.
mpg_bin_num<-nclass.Sturges(mtcars_df$mpg)
mtcars_df$mpg_bin <- cut(mtcars_df$mpg, mpg_bin_num)
mtcars_df$gear <-factor(mtcars_df$gear,levels = c("3", "4", "5"))
mtcars_freq <-table(mtcars_df$mpg_bin, mtcars_df$gear)
mtcars_freq
#6.Draw scatter chart using variables “mpg” (as horizontal axis) and “hp” (as vertical axis), then add the trendline.
ggplot(data=mtcars_df,aes(x=mpg,y=hp))+
  geom_point()+
  geom_smooth(method="lm")
#7.Compute mean, median, minimum, maximum, range, variance, and standard deviation of variable “hp”.
hp_sta <-mtcars_df%>%
  summarise(ave_hp=mean(hp),
            med_hp=median(hp),
            min_hp=min(hp),
            max_hp=max(hp),
            range_hp=(max(hp)-min(hp)),
            variance_hp=var(hp),
            std_hp=sd(hp))
hp_sta
#8.Compute covariance and correlation coefficient between variables “mpg” and “hp”.
print(paste("Covariance:", cov(mtcars_df$mpg, mtcars_df$hp)))
print(paste("Correlation coefficient:", cor(mtcars_df$mpg, mtcars_df$hp)))


