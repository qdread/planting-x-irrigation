yld_vs_audpc_lmm <- lmer(YldKgHa ~ log1p(AUDPC) * PD * Irr * resistance + (1|YR/Rep), data = dat)

n_points <- 101

# Set ranges for predicted data based on range of observed data
ranges <- dat[, as.list(range(AUDPC, na.rm = TRUE)), by = .(PD, Irr, resistance)] |>
  setnames(old = c('V1','V2'), new = c('min','max'))

# Generate predictors and get point estimate of predictions
pred_dat <- ranges[, .(AUDPC = expm1(seq(log1p(min), log1p(max), length.out = n_points))), by = .(PD, Irr, resistance)]
pred_dat[, YldKgHa := predict(yld_vs_audpc_lmm, newdata = pred_dat, re.form = ~ 0)]

boot_pred <- bootMer(yld_vs_audpc_lmm, function(fit) predict(fit, newdata = pred_dat, re.form = ~ 0), nsim = 999)
boot_quant <- apply(boot_pred$t, 2, quantile, probs = c(0.025, 0.975))
dimnames(boot_quant)[[1]] <- c('q025', 'q975')

pred_dat <- cbind(pred_dat, t(boot_quant))

ggplot(pred_dat, aes(x = AUDPC, y = YldKgHa, color = PD, fill = PD)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.25, color = NA) +
  geom_line() +
  geom_point(data = dat) +
  facet_grid(resistance ~ Irr, labeller = labeller(Irr = c(Irr = 'irrigated', Nonirr = 'non-irrigated'),
                                                   resistance = c(R = 'resistant', S = 'susceptible'))) +
  scale_y_continuous(name = parse(text = 'Yield~(kg~ha^-1)')) +
  scale_x_continuous(trans = 'log1p', breaks = 10^(1:5), labels = scales::comma) +
  scale_color_viridis_d(name = 'planting date', labels = c('April','May','June')) +
  theme(legend.position = c(0.15, 0.2)) +
  guides(fill = 'none')


# alternative figure ------------------------------------------------------

# Use the one without resistance factor.
pred_dat_long <- melt(pred_dat, id.vars = c('PD','Irr','AUDPC'), variable.name = 'q', value.name = 'y')


ggplot(pred_dat_long, aes(x = AUDPC, color = PD, fill = PD)) +
  geom_line(aes(y = y, linetype = q), size = 0.8) +
  geom_point(data = dat, aes(y = YldKgHa), alpha = 0.75) +
  facet_wrap(~ Irr, labeller = labeller(Irr = c(Irr = 'irrigated', Nonirr = 'non-irrigated'))) +
  scale_linetype_manual(values = c(YldKgHa = 'solid', q025 = 'dashed', q975 = 'dashed')) +
  scale_y_continuous(name = parse(text = 'Yield~(kg~ha^-1)')) +
  scale_x_continuous(trans = 'log1p', breaks = 10^(1:5), labels = scales::comma) +
  scale_color_viridis_d(name = 'planting date', labels = c('April','May','June')) +
  theme(legend.position = c(0.15, 0.2)) +
  guides(fill = 'none', linetype = 'none')
