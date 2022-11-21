library(lfe)

final <- readr::read_csv("data/analysis.csv")

# models (linear fixed effects specifications)
print("migration = f(foreign aid)")
baseline <- felm(mig ~ faid, final)
summary(baseline)

print("migration = f(foreign aid | donor fixed effects)")
feols1 <- felm(mig ~ faid | i_cou_faid, final)
summary(feols1)

print("migration = f(foreign aid | recipient fixed effects)")
feols2 <- felm(mig ~ faid | j_cou_mig, final)
summary(feols2)

print("migration = f(foreign aid | donor + receiver fixed effects)")
feols3 <- felm(mig ~ faid | i_cou_faid + j_cou_mig, final)
summary(feols3)

print("migration = f(foreign aid | sector fixed effects)")
feols4 <- felm(mig ~ faid | s, final)
summary(feols4)

print("migration = f(foreign aid | education fixed effects)")
feols5 <- felm(mig ~ faid | e, final)
summary(feols5)

print("migration = f(foreign aid | gender fixed effects)")
feols6 <- felm(mig ~ faid | g, final)
summary(feols6)

print("migration = f(foreign aid | donor, recipient, sector, education, gender fixed effects)")
feols7 <- felm(mig ~ faid | i_cou_faid + j_cou_mig + s + e + g, final)
summary(feols7)

