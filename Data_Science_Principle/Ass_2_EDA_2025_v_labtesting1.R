# After importing with the Wizard, make sure your data is called df:
# df <- your_data_name

# ---------- Part A ----------
# 1) Basic info
dim(df)           # _____, _____
names(df)         # column names
head(df, 5)   # first rows
str(df)           # structure

# 2) Churn rate (counts + %)
tbl_churn <- table(df$Churn)
tbl_churn
prop.table(tbl_churn)

# ---------- Part B ----------
# 3) Histograms
par(mfrow = c(2,2))
hist(df[["Day.Mins"]],   breaks = 10, main = "Day Mins",   xlab = "Minutes")
hist(df[["Eve.Mins"]],   breaks = 10, main = "Eve Mins",   xlab = "Minutes")
hist(df[["Night.Mins"]], breaks = 10, main = "Night Mins", xlab = "Minutes")
hist(df[["Intl.Mins"]],  breaks = 10, main = "Intl Mins",  xlab = "Minutes")
par(mfrow = c(1,1))

# 4) Bar chart: Customer Service Calls (counts)
barplot(table(df[["CustServ Calls"]]),
        main = "Customer Service Calls (counts)",
        xlab = "Customer Service Calls", ylab = "_____")

# ---------- Part C ----------
# 5) Boxplots: usage vs Churn
par(mfrow = c(2,2))
boxplot(`Day .Mins`   ~ _____, data = df, main = "Day .Mins by Churn",   ylab = "Day .Mins")
boxplot(`Eve .Mins`   ~ _____, data = df, main = "Eve .Mins by Churn",   ylab = "Eve .Mins")
boxplot(`Night .Mins` ~ _____, data = df, main = "Night .Mins by Churn", ylab = "Night .Mins")
boxplot(`Intl .Mins`  ~ _____, data = df, main = "Intl .Mins by Churn",  ylab = "Intl .Mins")
par(mfrow = c(1,1))

# 6) Churn proportion by Customer Service Calls
tab_calls_churn <- table(df[["CustServ Calls"]], df$Churn)
prop_calls <- prop.table(tab_calls_churn, 1)  # 1 = within each call count
barplot(t(prop_calls), beside = TRUE, legend = TRUE,
        main = "Churn Proportion by Customer Service Calls",
        xlab = "Customer Service Calls", ylab = "Proportion")

# 7) Churn proportion by Int'l Plan and VMail Plan
tab_intl  <- table(df[["Int'l Plan"]], df$Churn)
prop_intl <- prop.table(tab_intl, 1)
barplot(t(prop_intl), beside = TRUE, legend = TRUE,
        main = "Churn Proportion by Int'l Plan",
        xlab = "Int'l Plan", ylab = "Proportion")

tab_vmail  <- table(df[["VMail Plan"]], df$Churn)
prop_vmail <- prop.table(tab_vmail, 1)
barplot(t(prop_vmail), beside = TRUE, legend = TRUE,
        main = "Churn Proportion by VMail Plan",
        xlab = "VMail Plan", ylab = "Proportion")

# ---------- Part D (bonus) ----------
# 8) Calls × Int'l Plan × Churn (proportion of “True” in each Calls × Plan)
tab_3d <- table(df[["CustServ Calls"]], df[["Int'l Plan"]], df$_____)
prop_3d <- prop.table(tab_3d, c(_____, _____))   # proportions within each (Calls × Plan)
prop_true <- prop_3d[, , "_____"]                # pick the "True" layer
prop_true
barplot(prop_true, beside = TRUE, legend = TRUE,
        main = "Churn (True) by Calls × Int'l Plan",
        xlab = "Customer Service Calls", ylab = "Proportion (Churn=True)")

# ---------- Part E ----------
# 9) Scatter: Day Charge vs Day .Mins
plot(df[["Day .Mins"]], df[["Day Charge"]],
     main = "Day Charge vs Day .Mins",
     xlab = "_____", ylab = "_____", pch = 20)

# 10) Charge rate per minute
rate <- df[["Day Charge"]] / df[["_____"]]
summary(rate)
mean(rate, na.rm = TRUE)
