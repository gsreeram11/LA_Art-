

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(purrr)

coupon <- read_csv("coupon.csv")
la_art <- read_csv("LAart.csv")

la_art <- left_join(la_art, coupon, by = "CustID") %>% 
  mutate(X1.y = ifelse(is.na(X1.y), 0, 1)) %>% 
  rename(got_coupon = X1.y) %>% 
  dplyr::select(-X1.x)

# commission on paintings, jewelry is 8%
# commission on mosaics and sculptures is 12%
commissions <- c(0.08, 0.08, 0.12, 0.12)

la_art <- la_art %>% 
  mutate(revenue = Paintings + Jewelry + Mosaics + Sculpture,
         commission = Paintings*commissions[1] + Jewelry*commissions[1] +
           Mosaics*commissions[3] + Sculpture*commissions[4],
         age = as.numeric((as.Date(Sys.Date()) - DOB) / 365),
         days_as_customer = as.numeric((as.Date(Sys.Date()) - JoinDate)))

lm1 <- lm(revenue ~ Gender + Visits + got_coupon + days_as_customer + age, data = la_art)

lm2 <- lm(commission ~ Gender + Visits + got_coupon + days_as_customer + age, data = la_art)

rev <- la_art %>% 
  group_by(got_coupon) %>% 
  summarise(rev = mean(revenue),
            com = mean(commission))


la_art_box <- la_art %>% 
  mutate(
    got_coupon = factor(got_coupon)
  )


means <- aggregate(revenue ~ got_coupon, la_art_box, mean)
p_means <- ggplot(la_art_box, aes(x = got_coupon, y = revenue)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape = 18, size = 3,show_guide = FALSE) +
  geom_text(data = means, aes(label = round(revenue, 2), y = round(revenue, 2) - 20)) +
  labs(y = "Revenue", x = "Coupon", title = "Revenue by Discount") +
  scale_x_discrete(labels = c("No", "Yes"))

rev_coupon <- la_art %>% 
  filter(got_coupon == 1)

p_rev_coupon <- ggplot(rev_coupon, mapping = aes(x = 1:nrow(rev_coupon), y = revenue)) +
  geom_point() +
  labs(x = "Customer", y = "Revenue", title = "Grouping in Revenue in Customers who Received a Discount")


high_spend <- la_art %>% 
  filter(got_coupon == 1 & revenue > 700)

summary(high_spend)

low_spend <- la_art %>% 
  filter(got_coupon == 1 & revenue < 700)

summary(low_spend)


# If we run a regression controlling for all variables we see that the discount 
# has a significant negative impact on both revenue and commissions. 
# So at a first glance LA art should not offer the discount code to everyone

#------------------------------------------------------------------------------

# Coupons may have greater sway in getting new customers to buy art since they
# are more likely to be building up their art collection, rather than looking
# for a specific piece


# Group Customers into quartiles
la_art <- la_art %>% 
  mutate(time_as_customer = ifelse(days_as_customer < 1913, 1,
                                   ifelse(days_as_customer < 1990, 2,
                                          ifelse(days_as_customer < 2085, 3,
                                                 4))))

by_time <- la_art %>% 
  group_by(time_as_customer, got_coupon) %>% 
  summarise(mean_rev = mean(revenue),
            mean_comm = mean(commission)) %>% 
  ungroup() %>% 
  mutate(got_coupon = as.factor(got_coupon))

first_quartile <- la_art %>% 
  dplyr::filter(time_as_customer == 1)

quartile1_lm_rev <- lm(revenue ~ got_coupon + Gender + age + 
                         Visits + Gender:got_coupon, data = first_quartile)

quartile1_lm_com <- lm(commission ~ got_coupon + Gender + 
                         age + Gender:got_coupon, data = first_quartile)

# For newer customers we find that coupons provide a statistically significant,
# increase to revenues but it does not increase commissions. Newer customers are
# buying the low margin items.

# When adding an interaction effect between getting the coupon and gender we see that males, we see
# that the coupon had a significant effect on increasing revenues.

# For new customers the coupon has a negative effect on commissions, however commissions are
# statistically higher from male customers.


rev_p1 <- ggplot(by_time, aes(fill = got_coupon, y = mean_rev, x = time_as_customer)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Time as Customer Quartile", y = "", title = "Revenue") +
  theme(legend.position = "none")

com_p1 <- ggplot(by_time, aes(fill = got_coupon, y = mean_comm, x = time_as_customer)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Time as Customer Quartile", y = "", title = "Commission") +
  scale_fill_discrete(name = "Coupon", labels = c("No", "Yes"))

cowplot::plot_grid(rev_p1, com_p1)


# Comparing the money spent on low margin items and high
# margin items

margin_comp <- la_art %>% 
  mutate(low_margin = Paintings + Jewelry,
         high_margin = Mosaics + Sculpture) %>% 
  group_by(time_as_customer) %>% 
  summarise(mean_low = mean(low_margin),
            mean_high = mean(high_margin))# %>% 


margin_comp2 <- tibble(quartile = c(1,1,2,2,3,3,4,4),
                      high_low = factor(rep(c(1,0),4)),
                      value = c(margin_comp$mean_low[1], margin_comp$mean_high[1],
                                margin_comp$mean_low[2], margin_comp$mean_high[2],
                                margin_comp$mean_low[3], margin_comp$mean_high[3],
                                margin_comp$mean_low[4], margin_comp$mean_high[4]))


marg_p1 <- ggplot(margin_comp2, aes(fill = high_low, y = value, x = quartile)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Time as Customer Quartile", y = "", title = "Money Spent on Low and High Margin Items") +
  scale_fill_discrete(name = "", labels = c("Low Margin", "High Margin"))

# The plot confirms the suspicion that newer customers are buying lower margin items.

#------------------------------------------------------------------------------

# We know that coupons are effective at getting newer customers to purchase items 
# but they aren't buying the "right" items, how do we incentivize the customers
# to buy the high margin items, we could offer a higher coupon for those items

# Because of the difference in margins, for two items costing the same,
# a coupon of 10% on the low margin and a coupon of 40% on the high margin,
# yield the same commissions. The company could offer coupons only on
# high margin items or offer different coupon rates.


#------------------------------------------------------------------------------

library(cluster)
library(factoextra)

la_art_clust <- la_art %>% 
  dplyr::select(Gender, Visits, revenue, commission, age, days_as_customer) %>% 
  mutate(Gender = ifelse(Gender == "Female", 1, 0)) %>% 
  scale()


clust <- kmeans(la_art_clust, centers = 5, nstart = 100)

la_art2 <- la_art %>% 
  mutate(clust = clust$cluster) %>% 
  group_by(clust) %>% 
  nest() %>% 
  mutate(reg_rev = map(data, ~lm(.$revenue ~ .$got_coupon + .$Visits + .$days_as_customer + .$age)),
         reg_com = map(data, ~lm(.$commission ~ .$got_coupon + .$Visits + .$days_as_customer +.$age)))


fviz_cluster(clust, data = la_art_clust)

#------------------------------------------------------------------------------


