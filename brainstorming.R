# SIMULATION

checking <- 0.007
prob_check <- 1

stocks <- 0.1




## S&P 500 DATA

sp <- read.csv("~/Downloads/^GSPC.csv")
sp$daily_change <- sp$Close - sp$Open
sp$daily_change_perc <- (sp$Close - sp$Open) / sp$Open
sp$pos <- ifelse(sp$daily_change > 0, 1, 0)

for(i in 1:dim(sp)[1]){
    if(i == 1){
        sp$streak[i] <- 1
    } else if (sp$pos[i - 1] == sp$pos[i]) {
        sp$streak[i] <- sp$streak[i - 1] + 1
    } else {
        sp$streak[i] <- 1
    }
}

tmp <- sp[, c("pos", "streak")]
tmp <- rbind(data.frame(pos = NA, 
                        streak = NA),
             tmp)
tmp <- tmp[-1260, ]
names(tmp) <- c("yest_pos", "yest_streak")

sp <- cbind(sp, tmp)

lin <- lm(daily_change_perc ~ yest_pos + yest_streak, data = sp)
summary(lin)

library(corrplot)

correlations <- cor(sp[-1, 8:13])
corrplot(correlations, method = "circle")

library(rpart)

tree <- rpart(as.factor(pos) ~ yest_pos + yest_streak, data = sp[-1, ], minsplit = 2, minbucket = 1)
plot(tree)
text(tree)

library(ggplot2)

ggplot(data = sp[-1, ], aes(x = daily_change_perc, group = as.factor(yest_pos), 
                            color = as.factor(yest_pos))) + 
    geom_boxplot() + 
    facet_wrap(~ yest_streak, scales = "free") + 
    geom_vline(xintercept = 0)


ggplot(data = sp[-1, ], aes(x = daily_change_perc)) + 
    geom_histogram()

avg_change <- mean(sp$daily_change_per)
sd_change <- sd(sp$daily_change_perc)

mean(sp$pos)
mean(sp$daily_change)
mean(sp$daily_change_perc)
1.685539 = (1 + x)^5
1.110063 - 1

(1.110063) ^ (1 / 365) - 1

initial <- 200
avg_rate <- (1.110063) ^ (1 / 365) - 1

df <- data.frame(day = 0,
                 investment = initial,
                 exp_roi = initial * avg_rate,
                 total_value = initial + (initial * avg_rate))

for(day in 1:365){
    invest = df$investment[df$day == (day - 1)] + 1
    beg_tot_val = df$total_value[df$day == (day - 1)] + 1
    ex_roi = beg_tot_val * avg_rate
    fin_tot_val = beg_tot_val + ex_roi
    
    tmp <- data.frame(day = day,
                      investment = invest,
                      exp_roi= ex_roi,
                      total_value = fin_tot_val)
    
    df <- rbind(df, tmp)
}

full_sim <- data.frame()

for(sim in 1:1000){
    initial <- 200
    avg_rate <- (1.110063) ^ (1 / 365) - 1
    
    df <- data.frame(day = 0,
                     investment = initial,
                     rate_today = avg_rate,
                     exp_roi = 0,
                     total_value = initial)
    
    for(day in 1:365){
        invest = df$investment[df$day == (day - 1)] + 1
        beg_tot_val = df$total_value[df$day == (day - 1)] + 1
        rt = rnorm(1, avg_change, sd_change)
        ex_roi = beg_tot_val * rt
        fin_tot_val = beg_tot_val + ex_roi
        
        tmp <- data.frame(day = day,
                          investment = invest,
                          rate_today = rt,
                          exp_roi= ex_roi,
                          total_value = fin_tot_val)
        
        df <- rbind(df, tmp)
    }
    df$simulation <- sim
    
    full_sim <- rbind(full_sim, df)
}

full_sim$ROI <- (full_sim$total_value - full_sim$investment) / full_sim$investment
hist(full_sim$ROI[full_sim$day == 365])
boxplot(full_sim$ROI[full_sim$day == 365])
