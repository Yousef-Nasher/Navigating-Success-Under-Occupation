## =============================================================================
## 07_figures.R -- publication figures
## -----------------------------------------------------------------------------
## Figure 1  STROBE participant flow and outcome-specific analytic samples
## Figure 2  The decomposition: every contrast, panelled A -> B -> C. Reading a
##           row across the panels reads the decomposition of that outcome.
## Figure 3  Within-Palestinian: conflict items (mutually adjusted) -> outlook
##
## Colourblind-safe palette; 300 dpi PNG. No chartjunk.
## =============================================================================

suppressWarnings(suppressMessages({library(ggplot2); library(scales)}))

pal <- c(A = "#0072B2", B = "#D55E00", C = "#009E73")   # Okabe-Ito, greyscale-safe
theme_pub <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

## ---- Figure 1: STROBE flow --------------------------------------------------
flow <- read.csv(file.path(PATH$out, "flow_counts.csv"))
box <- data.frame(
  y = rev(seq_len(nrow(flow))),
  label = sprintf("%s\n(n = %s)", flow$step, format(flow$n, big.mark = ",")))
f1 <- ggplot(box, aes(x = 1, y = y)) +
  geom_tile(width = 0.9, height = 0.7, fill = "grey95", colour = "grey40") +
  geom_text(aes(label = label), size = 3.1, lineheight = 0.9) +
  geom_segment(data = box[-nrow(box), ],
               aes(x = 1, xend = 1, y = y - 0.35, yend = y - 0.65),
               arrow = arrow(length = unit(0.12, "cm")), colour = "grey40") +
  scale_y_continuous(expand = expansion(add = 0.6)) +
  labs(title = "Figure 1. Participant flow (STROBE)") +
  theme_void(base_size = 12) + theme(plot.title = element_text(face = "bold"))
ggsave(file.path(PATH$figures, "Figure1_flow.png"), f1,
       width = 6.5, height = 8, dpi = 300)

## ---- Figure 2: decomposition ------------------------------------------------
t3 <- read.csv(file.path(PATH$tables, "Table3_decomposition.csv"))
## Common signed-effect axis: b for SD outcomes, log(OR) for binary outcomes.
## 0 = no difference on both scales.
is_or <- t3$metric == "OR"
t3$x   <- ifelse(is_or, log(t3$est), t3$est)
t3$xlo <- ifelse(is_or, log(t3$lo),  t3$lo)
t3$xhi <- ifelse(is_or, log(t3$hi),  t3$hi)
out_order <- c("Post-traumatic stress", "Anxiety", "Educational disruption",
               "Dream-major attainment",
               "Structural (vs internal/no) barrier attribution",
               "Names the political situation as a barrier")
t3$outcome  <- factor(t3$outcome, levels = rev(out_order))
t3$block    <- factor(t3$block, levels = c("A", "B", "C"))
block_lab <- c(A = "A. vs non-FCS reference", B = "B. vs FCS stratum", C = "C. within Palestinians")

f2 <- ggplot(t3, aes(x = x, y = outcome, colour = block)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
  geom_errorbarh(aes(xmin = xlo, xmax = xhi), height = 0.22,
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(shape = block), size = 2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ block, nrow = 1, labeller = labeller(block = block_lab)) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_shape_manual(values = c(16, 17, 15), guide = "none") +
  labs(title = "Figure 2. Childhood exposure and all outcomes, decomposed",
       x = "Effect (SD units for symptom/education outcomes; log odds ratio for binary outcomes)",
       y = NULL) +
  theme_pub
ggsave(file.path(PATH$figures, "Figure2_decomposition.png"), f2,
       width = 11, height = 5.5, dpi = 300)

## ---- Figure 3: within-Palestinian, items mutually adjusted ------------------
wp <- readRDS(file.path(PATH$out, "within_palestinian.rds"))$panelB
wp$Outcome <- factor(wp$Outcome,
  levels = c("Perceived career/educational limitation", "Migration intention", "Hope for the future"))
wp$item_lab <- factor(wp$`Conflict experience`,
  levels = rev(c("Displacement, raids or arrests",
                 "Prevented from attending school or work",
                 "Does not feel safe expressing political views")))
f3 <- ggplot(wp, aes(x = est, y = item_lab)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey55") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  facet_wrap(~ Outcome, nrow = 1) +
  scale_x_log10() +
  labs(title = "Figure 3. Individual conflict experience and occupation-specific outlook (Palestinians, mutually adjusted)",
       x = "Adjusted odds ratio (log scale)", y = NULL) +
  theme_pub
ggsave(file.path(PATH$figures, "Figure3_within_palestinian.png"), f3,
       width = 11, height = 4, dpi = 300)

message("07_figures.R done: Figure 1, 2 and 3 written to output/figures/.")
