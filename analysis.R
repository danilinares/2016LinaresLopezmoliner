### Humans do not evidence choice biases in simple discrimination tasks ########
### by Daniel Linares and Joan López-Moliner (2016) ############################
### Data and code to conduct the statistics and produce the figures ############

### libraries ##################################################################
library(cowplot)
library(quickpsy)
library(tidyr)

### number of bootstrap samples ################################################
B <- 100 # we used a 1000 for the paper, but it could take long (~ .5 days) 

### plotting parameters ########################################################
oneColumnWidth <- 3.42
onehalfColumnWidth <- 4.5
twoColumnWidth <- 7
sizeLine1 <- .25
sizePoint1 <- 1
sizePoint2 <- 1.2
theme_bias <- theme_set(theme_classic(10))
theme_bias <- theme_update(axis.line.x = element_line(colour = 'black', 
                                                      size=sizeLine1, 
                                                      linetype='solid'),
                           axis.line.y = element_line(colour = 'black', 
                                                      size=sizeLine1, 
                                                      linetype='solid'),
                           axis.ticks= element_line(size = sizeLine1))
textReference <- 'Reference'
textProb <- 'Prob. responding clockwise'

### read and prepare the data ##################################################
dat <- quickreadfiles(path = 'data',
                      subject = c('a', 'b', 'c', 'd', 'e','f'),
                      session = as.character(1:6))

dat <- dat %>%
  select(subject, session, orLarge, orSmall, task, response) %>%
  mutate(response = ifelse(
                    (orLarge == 0 & response == 'right') |
                    (orLarge == 90 & response == 'down') |
                    (orLarge == 180 & response == 'left') |
                    (orLarge == 270 & response == 'up') |
                    (response == 'm'), 1, 0),
         vertical = ifelse(orLarge == 0 | orLarge == 180, TRUE, FALSE))

dat$subject <- factor(dat$subject,
              labels = paste0('Participant ', 1:length(levels(dat$subject))))

dat$orLarge <- factor(dat$orLarge,
                      levels = c(0, 90, 180, 270),
                      labels = c('Top', 'Right', 'Bottom', 'Left'))

datcomp <- dat %>% filter(task == 'comp')
datequ <- dat %>% filter(task == 'equ')

### fit cumulative normal with lapses ##########################################
fitcomp <- quickpsy(datcomp, orSmall, response,
                    grouping = .(subject, orLarge, vertical),
                    guess = TRUE, lapses = TRUE, xmax = -4, xmin = 4,
                    parini = list(c(-2, 2), c(0.1, 3), c(0, .4), c(0, .4)),
                    bootstrap = 'nonparametric',
                    B = B)

### PND comparisons
fitcomp$thresholdcomparisons %>% filter(subject==subject2, vertical==vertical2)

### choice bias prediction
choicebiascurves <- fitcomp$curves %>%
  filter(orLarge=='Top' | orLarge=='Right' ) %>%
  merge(fitcomp$par %>% filter(parn == 'p1')) %>%
  mutate(x = x - 2*par) %>%
  filter(x > -2.1, x < 2.1)
  
### correlation 
fitcompthrelong <- fitcomp$thresholds %>%
  select(-threinf, -thresup, -vertical) %>% spread(orLarge, thre)
fitcompthrelonginf <- fitcomp$thresholds %>%
  select(-thre, -thresup, -vertical) %>% spread(orLarge, threinf) %>%
  rename(Topinf = Top, Rightinf = Right, Bottominf = Bottom, Leftinf = Left)
fitcompthrelongsup <- fitcomp$thresholds %>%
  select(-thre, -threinf, -vertical) %>% spread(orLarge, thresup) %>%
  rename(Topsup = Top, Rightsup = Right, Bottomsup = Bottom, Leftsup = Left)

fitcompthrelongwithci <- merge(fitcompthrelong,
                               merge(fitcompthrelonginf, fitcompthrelongsup))

topbot <- fitcompthrelongwithci %>% select(subject, Top, Bottom) %>%
  rename(x = Top, y = Bottom)
riglef <- fitcompthrelongwithci %>% select(subject, Right, Left) %>%
  rename(x = Right, y = Left)
topbotriglef <- rbind(topbot, riglef)
cor.test(topbotriglef$x, topbotriglef$y)

toprig <- fitcompthrelongwithci %>% select(subject, Top, Right) %>%
  rename(x = Top, y = Right)
lefbot <- fitcompthrelongwithci %>% select(subject, Left, Bottom) %>%
  rename(x = Left, y = Bottom)
topriglefbot <- rbind(toprig, lefbot)
cor.test(topriglefbot$x, topriglefbot$y)

### figure 2
funpsychocomp <- function(flagVertical, flagOrder) {
  colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot(fitcomp$averages %>% filter(vertical==flagVertical),
         aes(x = orSmall, y = prob, color = orLarge, shape=orLarge)) +
    facet_wrap(~subject,scales = 'free_x') +
    geom_vline(xintercept = 0, lty = 2, size  = sizeLine1)+
    geom_point(size = sizePoint1) +
    geom_line(data = fitcomp$curves %>% filter(vertical==flagVertical),
                aes(x = x, y = y),
                size  = sizeLine1) +
     geom_line(data = choicebiascurves %>% filter(vertical==flagVertical),
               aes(x = x, y = y),
               size  = sizeLine1, lty =2, color = colorchoicebiascurves) +
    geom_segment(data = fitcomp$thresholds %>%
                   filter(vertical==flagVertical),
                 aes(x=threinf,xend = thresup, y = .5, yend = 0.5,
                     color=orLarge),
                 size  = sizeLine1) +
    scale_color_manual(values = c(colorcurves1,colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = 'Orientation (deg)', y = textProb,
         color = textReference, shape = textReference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(strip.background = element_blank())

}
plotcomp0 <- funpsychocomp(TRUE, FALSE)
plotcomp90 <- funpsychocomp(FALSE, TRUE)

pcor2 <- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Right, color ='Top-Right', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Right, yend = Right,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Rightinf, yend = Rightsup,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Left,y=Bottom, color='Left-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Leftinf, xend = Leftsup, y = Bottom, yend = Bottom,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Left, xend = Left, y = Bottominf, yend = Bottomsup,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_color_manual(values = c('#a65628','#f781bf')) +
  scale_shape_discrete(solid=F) +
  labs(x = 'PND (deg)', y = 'PND (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

pcor1 <- ggplot(data = fitcompthrelongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Bottom, color ='Top-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Bottom, yend = Bottom,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Bottominf, yend = Bottomsup,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, color='Right-Left', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PND (deg)', y = 'PND (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))


pcomppsy <- plot_grid(plotcomp90, plotcomp0, labels = c('A','B'),
                      ncol = 1, hjust = 0, vjust = 1)
pcor <- plot_grid(pcor1, labels = 'C',  hjust = 0)
pcomp <- plot_grid(pcomppsy, pcor, ncol =1, rel_heights = c(2.6,.8))
save_plot('figures/fig2.pdf', pcomp,
          base_width = onehalfColumnWidth,
          base_height = 1.5 * onehalfColumnWidth)

### fit two cumulative normal ##################################################
f <- function(x, p) pnorm(x, p[1] - p[3], p[2]) - pnorm(x, p[1] + p[3], p[2])
fitequcumnorm <- quickpsy(datequ, orSmall, response,
                          grouping = .(subject,orLarge,vertical),
                          B = B, fun = f,
                          parini=list(c(-2,2),c(0.1,3),c(0.1,3)),
                          bootstrap = 'nonparametric', thresholds = F)

### calculating PMR
equcumnormmax <- fitequcumnorm$curves %>%
  summarise(maxi=approx(x=y,y=x,xout=max(y))[[1]])

### PMR comparisons
fitequcumnorm$parcomparisons %>%
  filter(parn =='p1', subject == subject2, vertical == vertical2)

### correlation 
pse <- fitequcumnorm$par %>% filter(parn=='p1') %>% merge(equcumnormmax)
fiteqpselong <- pse %>%
  select(-parinf, -parsup, -vertical, -maxi) %>% spread(orLarge, par)
fiteqpselonginf <- pse %>%
  select(-par, -parsup, -vertical,-maxi) %>% spread(orLarge, parinf) %>%
  rename(Topinf = Top, Rightinf = Right, Bottominf = Bottom, Leftinf = Left)
fiteqpselongsup <- pse %>%
  select(-par, -parinf, -vertical,-maxi) %>% spread(orLarge, parsup) %>%
  rename(Topsup = Top, Rightsup = Right, Bottomsup = Bottom, Leftsup = Left)

fiteqpselongwithci <- merge(fiteqpselong,
                               merge(fiteqpselonginf, fiteqpselongsup))

topboteq <- fiteqpselong %>% select(subject, Top, Bottom) %>%
  rename(x = Top, y = Bottom)
riglefeq <- fiteqpselong %>% select(subject, Right, Left) %>%
  rename(x = Right, y = Left)
topbotriglefeq <- rbind(topboteq, riglefeq)
cor.test(topbotriglefeq$x, topbotriglefeq$y)

toprigeq <- fiteqpselong %>% select(subject, Top, Right) %>%
  rename(x = Top, y = Right)
lefboteq <- fiteqpselong %>% select(subject, Left, Bottom) %>%
  rename(x = Left, y = Bottom)
topriglefboteq <- rbind(toprigeq, lefboteq)

cor.test(topriglefboteq$x, topriglefboteq$y)

### figure 3
textProb2 <- 'Prob. responding aligned'
funpsychoeq <- function(flagVertical, flagOrder) {
  colorcurves1 <- ifelse(flagVertical,'#e41a1c','#4daf4a')
  colorcurves2 <- ifelse(flagVertical,'#377eb8','#984ea3')
  colorchoicebiascurves <- ifelse(flagVertical,'#377eb8','#984ea3')
  ggplot() +
    facet_wrap(~subject,scales = 'free_x') +
    geom_vline(xintercept = 0, lty = 2, size  = sizeLine1)+
    geom_rect(data = fitcomp$thresholds %>%
                   filter(vertical==flagVertical),
                 aes(xmin=threinf,xmax=thresup,ymin=0, ymax=1, fill=orLarge),
                  show.legend = FALSE,alpha = .25) +
    geom_point(data=fitequcumnorm$averages %>% filter(vertical==flagVertical),
               aes(x = orSmall, y = prob, color = orLarge, shape=orLarge),
               size = sizePoint1) +
    geom_segment(data = pse %>% filter(vertical==flagVertical),
                 aes(x = parinf, xend = parsup, y = 0, yend = 0,
                     color = orLarge),size  = sizeLine1) +
    geom_segment(data = pse %>% filter(vertical==flagVertical),
                 aes(x = par, xend = par, y = 0, yend = maxi,
                     color = orLarge),size  = sizeLine1) +
    geom_line(data = fitequcumnorm$curves %>% filter(vertical==flagVertical),
              aes(x = x, y = y, color = orLarge),
              size  = sizeLine1) +
    scale_color_manual(values = c(colorcurves1,colorcurves2)) +
    guides(color = guide_legend(reverse=flagOrder),
           shape = guide_legend(reverse=flagOrder)) +
    labs(x = 'Orientation (deg)', y = textProb2,
         color = textReference, shape = textReference) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    coord_cartesian(xlim=c(-2.1, 2.1),ylim=c(0,1)) +
    theme(strip.background = element_blank())

}
ploteq0 <- funpsychoeq(TRUE, FALSE)
ploteq90 <- funpsychoeq(FALSE, TRUE)

pcoreq2 <- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Right, color ='Top-Right', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Right, yend = Right,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Rightinf, yend = Rightsup,
                   color ='Top-Right', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Left,y=Bottom, color='Left-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Leftinf, xend = Leftsup, y = Bottom, yend = Bottom,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Left, xend = Left, y = Bottominf, yend = Bottomsup,
                   color ='Left-Bottom', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#a65628','#f781bf')) +
  labs(x = 'PMR (deg)', y = 'PMR (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

pcoreq1 <- ggplot(data = fiteqpselongwithci )+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=Top,y=Bottom, color ='Top-Bottom', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Topinf, xend = Topsup, y = Bottom, yend = Bottom,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Top, xend = Top, y = Bottominf, yend = Bottomsup,
                   color ='Top-Bottom', shape = subject), size  = sizeLine1) +
  geom_point(aes(x=Right,y=Left, color='Right-Left', shape = subject),
             size = sizePoint2) +
  geom_segment(aes(x = Rightinf, xend = Rightsup, y = Left, yend = Left,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = Right, xend = Right, y = Leftinf, yend = Leftsup,
                   color ='Right-Left', shape = subject), size  = sizeLine1) +
  guides(shape = FALSE) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = c('#ff7f00','#999999')) +
  labs(x = 'PMR (deg)', y = 'PMR (deg)') +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

peqpsy <- plot_grid(ploteq90, ploteq0, labels = c('A','B'),
                      ncol = 1, hjust = 0, vjust = 1)
pcoreq <- plot_grid(pcoreq1, labels = 'C', hjust = 0)
peq <- plot_grid(peqpsy, pcoreq, ncol =1, rel_heights = c(2.6,.8))

save_plot('figures/fig3.pdf', peq,
          base_width = onehalfColumnWidth,
          base_height = 1.5 * onehalfColumnWidth)

#### PND PMR comparisons #######################################################
compeq <- fitequcumnorm$parbootstrap %>% filter(parn == 'p1') %>%
  select(-parn) %>% merge(fitcomp$thresholdsbootstrap %>% select(-prob)) %>%
  mutate(dif = par - thre) %>% group_by(subject, orLarge) %>%
  summarise(inf = quantile(dif, .025), sup = quantile(dif,.975),
            sign = ifelse(inf * sup > 0, 1, 0))

all <- merge(pse, fitcomp$thresholds)
confint(lm(all$thre~all$par))
cor.test(all$par,all$thre)

### fig 4 
pcorcompeq <- ggplot(data = all )+ #facet_wrap(~orLarge)+
  geom_abline(slope = 1, lty =2, size  = sizeLine1)+
  geom_point(aes(x=par,y=thre,color=orLarge,shape = subject))+
  geom_segment(aes(x = parinf, xend = parsup, y = thre, yend = thre,
                   color =orLarge, shape = subject), size  = sizeLine1) +
  geom_segment(aes(x = par, xend = par, y = threinf, yend = thresup,
                   color =orLarge, shape = subject), size  = sizeLine1) +
  scale_shape_discrete(solid=F) +
  scale_x_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  scale_y_continuous(breaks =seq(-1,1,.5),
                     labels = c('-1','-0.5','0','0.5','1')) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))+
  labs(x = 'PMR asymmetric task (deg)', y = 'PND symmetric task (deg)') +
  guides(shape = FALSE) +
  scale_colour_brewer(palette = 'Set1')+
  labs(color = textReference) +
  theme(legend.key.size = unit(1,'line'),
        plot.margin = unit(c(-5,0,0,0), 'line'))

save_plot('figures/fig4.pdf', pcorcompeq,
          base_width = oneColumnWidth,
          base_height = oneColumnWidth)

### correlation orthogonal axes ################################################
### fig s1
psup <-plot_grid(pcor2, pcoreq2, labels = c('A','B'), ncol=1, hjust = 0)

save_plot('figures/figs1.pdf', psup,
          base_width = oneColumnWidth,
          base_height = 1.5*oneColumnWidth)


