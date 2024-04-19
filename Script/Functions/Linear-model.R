# print the results of the fixed effects 
lm_results <- function(model){
  print("Anova")
  anova <- anova(model, ddf = "Kenward-Roger", type = 2)
  print(anova)
  print("Fixed effect estimates")
  print(summary(model)$coefficients)
}

# provide diagnostic assumptions of linear models
lm_diagnosis <- function(model){
  print("check constant variance over fitted values")
  print(check_model(model, check="homogeneity"))
  print(paste0("Normality of ", attr(attr(attr(model,"frame"), "terms"),"varnames.fixed")[1]))
  qqnorm(residuals(model)) 
  qqline(residuals(model))
  print("Normality random residuals")
  text(qqnorm(ranef(model)$Fish[,1]), labels=rownames(ranef(model)$Fish), cex= 0.7, pos = 1)
  qqline(ranef(model)$Fish[,1])
  check_model(model, check="pp_check")
}

# Calculate the contrasts between Jar, Small, Medium and Large
contr_tank.size <- function(model){ 
  temp <- emmeans(model, ~ Tank, 
                  at = list(Tank = c("Jar", "Small", "Medium", "Large"), 
                            Order = c("1", "2", "3", "4"))) %>% 
    contrast(.,
             list(jar_vs_small     = c(1,-1, 0, 0), 
                  jar_vs_medium    = c(1, 0,-1, 0), 
                  jar_vs_large     = c(1, 0, 0,-1),
                  small_vs_medium  = c(0, 1,-1, 0),
                  small_vs_large   = c(0, 1, 0,-1), 
                  medium_vs_large  = c(0, 0, 1,-1)),
             adjust = "holm")
  temp2 <- confint(temp)
  temp3 <- merge(temp, temp2, by=c("contrast", "estimate","SE","df"), sort=FALSE)
  temp3$estimate <- sprintf("%.0f",temp3$estimate)
  temp3$SE <- sprintf("%.1f",temp3$SE)
  temp3$df <- sprintf("%.0f",temp3$df)
  temp3$t.ratio <- sprintf("%.2f",temp3$t.ratio)
  temp3$p.value <- scales::pvalue(temp3$p.value)
  temp3$lower.CL <- sprintf("%.0f",temp3$lower.CL)
  temp3$upper.CL <- sprintf("%.0f",temp3$upper.CL)
  print(temp3)
}

# Calculate the contrasts between Large and Barren
## contrasts between Large (over Orders 1 to 4) and Barren
contr_tank.enrich <- function(model){ 
  emm1 <- emmeans(model, ~ Tank , 
                  at = list(Tank = c("Large"), 
                            Order = c("1", "2", "3", "4")))
  emm2 <- emmeans(model, ~ Tank , 
                  at = list(Tank = c("Barren"), 
                            Order = c("5")))
  temp <- contrast(rbind(emm1,emm2), list(large_vs_barren= c(1,-1)))
  temp2 <- confint(temp)
  temp3 <- merge(temp, temp2, by=c("contrast", "estimate","SE","df"), sort=FALSE)
  temp3$estimate <- sprintf("%.0f",temp3$estimate)
  temp3$SE <- sprintf("%.1f",temp3$SE)
  temp3$df <- sprintf("%.0f",temp3$df)
  temp3$t.ratio <- sprintf("%.2f",temp3$t.ratio)
  temp3$p.value <- scales::pvalue(temp3$p.value)
  temp3$lower.CL <- sprintf("%.0f",temp3$lower.CL)
  temp3$upper.CL <- sprintf("%.0f",temp3$upper.CL)
  print(temp3)
}

# Calculate the contrasts between tanks for the generalised linear models
calc_contrasts_bin <- function(model){ 
  temp <- emmeans(model, ~ Tank) %>% 
    contrast(.,
             list(jar_vs_small     = c(1,-1, 0, 0, 0), 
                  jar_vs_medium    = c(1, 0,-1, 0, 0), 
                  jar_vs_large     = c(1, 0, 0,-1, 0),
                  small_vs_medium  = c(0, 1,-1, 0, 0),
                  small_vs_large   = c(0, 1, 0,-1, 0), 
                  medium_vs_large  = c(0, 0, 1,-1, 0),
                  large_vs_barren  = c(0, 0, 0, 1,-1)),
             adjust = "holm",
             type = "response")
  temp2 <- confint(temp)
  temp3 <- merge(temp, temp2, by=c("contrast","odds.ratio","SE","df"), sort=FALSE)
  temp3$odds.ratio <- sprintf("%.2f",temp3$odds.ratio)
  temp3$SE <- sprintf("%.2f",temp3$SE)
  temp3$z.ratio <- sprintf("%.2f",temp3$z.ratio)
  temp3$p.value <- scales::pvalue(temp3$p.value)
  temp3$asymp.LCL <- sprintf("%.3f",temp3$asymp.LCL)
  temp3$asymp.UCL <- sprintf("%.3f",temp3$asymp.UCL)
  print(temp3)
}
