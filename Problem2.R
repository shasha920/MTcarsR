cyl_freq <- mtcars_df %>%
  group_by(cyl)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n),percent_freq=(n/ sum(n)) * 100)
head(cyl_freq)
ggplot(data=mtcars_df)+
  geom_bar(mapping=aes(x=cyl))
barplot(cyl_freq$n, names.arg = cyl_freq$cyl,
        xlab = "cyl", ylab = "Frequency", main = "Bar Chart of cyl")
pie(cyl_freq$n, main = "Pie Chart of cyl",
    labels = paste(cyl_freq$cyl, " (", cyl_freq$percent_freq, "%)", sep = ""))
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
hist(mtcars_df$mpg,xlab = "mpg",
     main = "Histogram of mpg")
mpg_bin_num<-nclass.Sturges(mtcars_df$mpg)
mtcars_df$mpg_bin <- cut(mtcars_df$mpg, mpg_bin_num)
mtcars_df$gear <-factor(mtcars_df$gear,levels = c("3", "4", "5"))
mtcars_freq <-table(mtcars_df$mpg_bin, mtcars_df$gear)
mtcars_freq
ggplot(data=mtcars_df,aes(x=mpg,y=hp))+
  geom_point()+
  geom_smooth(method=lm)
hp_sta <-mtcars_df%>%
  summarise(ave_hp=mean(hp),
            med_hp=median(hp),
            min_hp=min(hp),
            max_hp=max(hp),
            range_hp=(max(hp)-min(hp)),
            variance_hp=var(hp),
            std_hp=sd(hp))
hp_sta
