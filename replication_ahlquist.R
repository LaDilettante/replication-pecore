rm(list=ls())
toLoad <- c("plyr", "reshape2", "stargazer", "car", "xtable")
lapply(toLoad, library, character.only=TRUE)

# ---- Convenience function ----
summarize.r = function(reg.lm, ...){
  z = summary.lm(reg.lm)
  W.VCOV = vcovHC(reg.lm,type="HC0")
  sig.sq = ((summary(reg.lm)$sigma)^2)
  coef = z$coefficients[,1]
  
  White.SE <- sqrt(diag(W.VCOV)) t.robust <- coef/White.SE ## Element-by-element division 
  df <- reg.lm$df.residual 
  p.val.robust <- 2*(1-pt(abs(t.robust),df)) 
  z$cov.unscaled = W.VCOV/sig.sq
  z$coefficients[,2] = White.SE
  z$coefficients[,3] = t.robust
  z$coefficients[,4] = p.val.robust z 
}

# ---- Load and create variables ----

data <- read.csv("Ahlquist ISQ 2006 data.csv")
names(data)

# ---- Clean data ----

data.melt <- melt(data, id.vars=c("id", "wbcode", "polcode", "country", "cid", "year"), na.rm=TRUE)
data.country_mean <- dcast(data.melt, country~variable, mean)

rhs.between <- c("deficit + govex + inflation.deflt + official.xr.pct + 
                 cum.default.60 + polity + varpol.5 + external.debt + growth + l.gdp.pc + l.gdp")

fm_iir.between <- formula(paste0("IIR.annual ~ ", rhs.between))
m_iir.btwn <- lm(fm_iir.between, data=data.country_mean)
m_iir.btwn_se <- sqrt(diag(hccm(lm(fm_iir.between, data=data.country_mean), type = "hc0")))

length(m_iir.btwn$coeff)
length(m_iir.btwn_se)

xtable(data.frame(m_iir.btwn$coeff, m_iir.btwn_se))

out1 <- stargazer(m_iir.btwn, t(m11))
out11 <- stargazer(m11)
