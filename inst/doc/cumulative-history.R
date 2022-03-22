## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

library(dplyr)
library(ggplot2)

## ----out.width="80%", fig.align='center'--------------------------------------
knitr::include_graphics("nc-stimulus.png")

## ----out.width="80%", fig.align='center'--------------------------------------
knitr::include_graphics("gamma-distribution.png")

## ----fig.width=8, fig.height=4, out.width="80%", fig.align='center'-----------
t <- seq(0, 20, length.out=100)
df <- 
  data.frame(h0 = c(0, 0.7, 0.9), S = c(1, 0, 0.5), tau = c(1, 4, 2)) %>%
  group_by(h0, S, tau) %>%
  summarise(h0 = h0[1],
            S = S[1],
            tau = tau[1],
            t = t,
            h = S + (h0 - S) * exp(-t / tau),
            Parameters = sprintf("h(0) = %.1f, S = %.1f, tau = %d", h0[1], S[1], tau[1]),
            .groups="keep")

ggplot(df, aes(x=t, y=h, color=Parameters)) + 
  geom_line() + 
  xlab("dt [s]") +
  ylab("h(t + Δt)") + 
  ylab("h(t + \u0394t)") + 
  ylim(0, 1) + 
  theme(legend.position="top")

