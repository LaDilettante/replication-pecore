rm(list=ls())
toLoad <- c("plyr", "reshape2", "stargazer", "car", "xtable")
lapply(toLoad, library, character.only=TRUE)

# ---- Function ----
## Heteroskedasticity-robust standard error calculation.
summaryw <- function(model) {
  s <- summary(model)
  X <- model.matrix(model)
  u2 <- residuals(model)^2
  XDX <- 0
  
  ## Here one needs to calculate X'DX. But due to the fact that
  ## D is huge (NxN), it is better to do it with a cycle.
  for(i in 1:nrow(X)) {
    XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
  }
  
  # inverse(X'X)
  XX1 <- solve(t(X)%*%X)
  
  # Variance calculation (Bread x meat x Bread)
  varcovar <- XX1 %*% XDX %*% XX1
  
  # degrees of freedom adjustment
  dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
  
  # Standard errors of the coefficient estimates are the
  # square roots of the diagonal elements
  stdh <- dfc*sqrt(diag(varcovar))
  
  t <- model$coefficients/stdh
  p <- 2*pnorm(-abs(t))
  results <- cbind(model$coefficients, stdh, t, p)
  dimnames(results) <- dimnames(s$coefficients)
  results
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


res <- data.frame(coeff=m_iir.btwn$coeff, se=m_iir.btwn_se)
res <- within(res, t <- coeff / se)
res <- within(res, p <- 2 * pt(abs(t), df=m_iir.btwn$df, lower.tail=FALSE))


format( round(summaryw(m_iir.btwn)[ , 4], 2))

data.frame(summaryw(m_iir.btwn)[ ,1:3], format( round(summaryw(m_iir.btwn)[ , 4], 2)) )


format(round(summaryw(m_iir.btwn), 2), nsmall=2)

prettyNum(summaryw(m_iir.btwn), format="f")

apply(summaryw(m_iir.btwn), 2, formatC, digits=2)

summary(m_iir.btwn)



length(m_iir.btwn$coeff)
length(m_iir.btwn_se)

xtable(data.frame(m_iir.btwn$coeff, m_iir.btwn_se))

out1 <- stargazer(m_iir.btwn, t(m11))
out11 <- stargazer(m11)
