# Strains and reps have their own intercepts, and all treatments are given interactions
lmm_full_cfu <- lmer(log1p(FinalCFU) ~ PD * resistance * Irr + (1|YR/Rep) + (1|Strains), data = dat)
lmm_full_audpc <- lmer(log1p(AUDPC) ~ PD * resistance * Irr + (1|YR/Rep) + (1|Strains), data = dat)
lmm_full_yield <- lmer(YldKgHa ~ PD * resistance * Irr + (1|YR/Rep) + (1|Strains), data = dat)

# Try out some random slope models as well, in case it improves things.
# This model takes a very long time to run.
lmm_cfu2 <- lmer(log1p(FinalCFU) ~ PD * resistance * Irr + (PD * resistance * Irr|YR/Rep) + (PD * resistance * Irr|Strains), data = dat) # failed to converge
lmm_cfu3 <- lmer(log1p(FinalCFU) ~ PD * resistance * Irr + (PD * resistance * Irr|YR/Rep) + (resistance|Strains), data = dat) # Failed to converge
lmm_cfu4 <- lmer(log1p(FinalCFU) ~ PD * resistance * Irr + (1|YR/Rep) + (resistance|Strains), data = dat) # Singular
