# Code for package hex sticker

library(tidyverse)
library(canpumf)
library(hexSticker)

sfs_data <- get_pumf("SFS","2019")

d <- sfs_data |>
  mutate(inheritance = case_when(is.na(PINHERT) ~ "No inheritance",
                                 PINHERT<= 0 ~ "Zero inheritance",
                                 PINHERT<=75000 ~ "Less than $75k",
                                 TRUE ~ "Over $75k")) |>
  mutate(own=PFTENUR != "Do not own") |>
  summarise(Count=sum(PWEIGHT),
            across(matches("BSW_\\d+"),sum),
            cases=n(),
            .by=c(PAGEMIEG,own,inheritance)) |>
  collect() |>
  pivot_longer(matches("BSW_\\d+"), names_to="Weight",values_to="Value") |>
  mutate(Share=Count/sum(Count),
         Share_w=Value/sum(Value),
         sum_cases=sum(cases),
         .by=c(Weight,PAGEMIEG,inheritance)) |>
  mutate(inheritance=factor(inheritance, levels=c("No inheritance","Less than $75k","Over $75k")))

g <- d |>
  filter(PAGEMIEG!="Under 20 years") |>
  filter(sum_cases>=5, own) |>
  filter(inheritance %in% c("No inheritance","Over $75k")) |>
  ggplot(aes(x=PAGEMIEG)) +
  geom_boxplot(aes(y=Share_w, fill=inheritance), show.legend = FALSE, shape=1) +
  #geom_point(aes(y=Share), color="brown") +
  theme_void() +
  scale_fill_manual(values=c("#C0C0C0","gold")) +
  theme(axis.text = element_blank()) +
  # facet_wrap(~inheritance, labeller=as_labeller(c("No inheritance"="","Over $75k"="")),
  #            ncol=1) +
  labs(x=NULL,y=NULL, fill=NULL)


s<-sticker(~plot(g, cex=.5, cex.axis=.5,  xlab="", ylab=""),
        package="canpumf",
        s_x=1, s_y=1.1, s_width=1.4, s_height=1,
        p_y=0.6, p_size = 35,
        h_color="#E60013", h_fill="#444444", h_size = 3) +
  theme_transparent() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_blank(), # Removes main gridlines
        panel.grid.minor = element_blank(), # Removes minor gridlines
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())


ggsave(here::here("inst/figures/canpumf-logo.png"), width = 3.5,height=3.5)

