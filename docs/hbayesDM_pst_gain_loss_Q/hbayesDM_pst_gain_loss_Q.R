###パッケージない場合自動でゲット

if (!require(hBayesDM)) install.packages("hBayesDM")
if (!require(dplyr)) install.packages("dplyr")


library(dplyr)

library(hBayesDM)


### いらん環境削除


rm(list = ls())

#### 目先の罰と報酬に影響されまくる人作成


set.seed(1)
type1 <- rep(12, 50)


subjID1 <-c(
  rep(1, 50)
)

reward_A_1 <- c(
  rep(1, 40),
  rep(0,10)
) %>% sample(.) 



reward_B_1 <- c(
  rep(0, 40),
  rep(1,10)
) %>% sample(.) 



choice_1 <- c(0, rep(NA, 49))




df1 <- cbind(subjID1, type1, reward_A_1, reward_B_1, choice_1) %>% data.frame()


df1 <- df1 %>% mutate(., reward_1 = ifelse(
  choice_1 == 1, reward_A_1,
  ifelse(choice_1 == 0, reward_B_1, NA)
))





for (i in 2:50) {
df1$choice_1[i] <- ifelse(df1$choice_1[i-1] == 0 & df1$reward_1[i-1] == 0, 1, 
                          ifelse(df1$choice_1[i-1] == 0 & df1$reward_1[i-1] == 1, 0,
                                 ifelse(df1$choice_1[i-1] == 1 & df1$reward_1[i-1] == 0, 0,
                                        ifelse(df1$choice_1[i-1] == 1 & df1$reward_1[i-1] == 1, 1, NA)
                                 )
                          )
                          )

df1$reward_1[i] <- ifelse(df1$choice_1[i] == 1, reward_A_1[i],
                        ifelse(df1$choice_1[i] == 0, reward_B_1[i], NA))

}





names(df1) <- c("subjID", "type", "choice", "reward_A", "reward_B", "reward")





###　完全にランダムに適当に選ぶ人作成

set.seed(2)

subjID2 <- rep(2, 50)

type2 <- rep(12, 50)


reward_A_2 <- c(
  rep(1, 40),
  rep(0, 10)
) %>% sample


reward_B_2 <- c(
  rep(0, 40),
  rep(1, 10)
) %>% sample


choice_2 <- rep(c(1,0), 25) %>% sample(.)
 
df2 <- cbind(subjID2, type2, choice_2, reward_A_2, reward_B_2) %>% data.frame()

df2 <- df2 %>% mutate(., reward_2 = ifelse(choice_2 == 1, reward_A_2, 
                                           ifelse(choice_2 == 0, reward_B_2, NA)
                                           )
                      )

names(df2) <- c("subjID", "type", "choice", "reward_A", "reward_B", "reward")


df <- rbind(df1, df2)





alpha_list <- hBayesDM::pst_gainloss_Q(df, ncore = 8)


alpha_df <- alpha_list[["allIndPars"]]