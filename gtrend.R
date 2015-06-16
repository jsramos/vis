devtools::install_github("dvanclev/GTrendsR")
devtools::install_github("trinker/gtrend")
library(gtrend) 
library(dplyr) 
library(ggplot2) 
library(scales)

 terms <- c("Indice de Precios y Cotizaciones")

out <- gtrend_scraper("afdesignlab@gmail.com", "IchheisseG1", terms, geo = "MX")

out %>%
  trend2long() %>%
  plot() 


out %>%
  trend2long() %>%
  ggplot(aes(x=start, y=trend, color=term)) +
  geom_line() +
  facet_wrap(~term) +
  guides(color=FALSE)


names(out)[1]
dat <- out[[1]][["trend"]]
colnames(dat)[3] <- "trend"

dat2 <- dat[dat[["start"]] > as.Date("2011-01-01"), ]

rects <- dat2  %>%
  mutate(year=format(as.Date(start), "%y")) %>%
  group_by(year) %>%
  summarize(xstart = as.Date(min(start)), xend = as.Date(max(end)))

ggplot() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, 
                              ymax = Inf, fill = factor(year)), alpha = 0.4) +
  geom_line(data=dat2, aes(x=start, y=trend), size=1.2) + 
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("month"),
               expand = c(0,0), 
               limits = c(as.Date("2011-01-02"), as.Date("2014-12-31"))) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) 



dat3 <- dat[dat[["start"]] > as.Date("2010-12-21") & 
              dat[["start"]] < as.Date("2012-01-01"), ]

ggplot() +
  geom_line(data=dat3, aes(x=start, y=trend), size=1.2) + 
  scale_x_date(labels = date_format("%b %y"), 
               breaks = date_breaks("month"),
               expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  theme_bw() + theme(panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank()) + 
  ggplot2::annotate("text", x = as.Date("2011-01-15"), y = 50, 
                    label = "Winter\nBreak Ends") +
  ggplot2::annotate("text", x = as.Date("2011-05-08"), y = 70, 
                    label = "Summer\nBreak\nAcademia") +
  ggplot2::annotate("text", x = as.Date("2011-06-15"), y = 76, 
                    label = "Summer\nBreak\nTeachers") +
  ggplot2::annotate("text", x = as.Date("2011-08-18"), y = 63, 
                    label = "Academia\nReturns") +
  ggplot2::annotate("text", x = as.Date("2011-08-17"), y = 78, 
                    label = "Teachers\nReturn")+
  ggplot2::annotate("text", x = as.Date("2011-11-17"), y = 61, 
                    label = "Thanksgiving")



out %>%
  trend2long() %>%
  filter(term %in% c("literacy+research+association", 
                     "international+reading+association")) %>%
  as.trend2long() %>%
  plot() + 
  guides(color=FALSE) +
  ggplot2::annotate("text", x = as.Date("2011-08-17"), y = 60, 
                    label = "International\nReading\nAsociation", color="#F8766D")+
  ggplot2::annotate("text", x = as.Date("2006-01-17"), y = 38, 
                    label = "Literacy\nResearch\nAssociation", color="#00BFC4")	+
  theme_bw() +
  stat_smooth()



out %>%
  trend2long() %>%
  filter(term %in% names(out)[1:7]) %>%
  as.trend2long() %>%
  plot() + scale_colour_brewer(palette="Set1") +
  facet_wrap(~term, ncol=2) +
  guides(color=FALSE)



out %>%
  trend2long() %>%
  filter(term %in% names(out)[c(1, 3, 5, 7)]) %>%
  as.trend2long() %>%
  plot() 



out %>%
  trend2long() %>%
  filter(term %in% names(out)[c(2, 4, 6)]) %>%
  as.trend2long() %>%
  plot() 