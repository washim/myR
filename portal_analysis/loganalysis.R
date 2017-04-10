library(tidyverse)
library(xts)
library(ggplot2)
library(gridExtra)

load("~/portal_analysis/access_log.RData")

df <- access_log %>%
  group_by(time,server,node) %>%
  summarize(frequency = n())

enterprise_node1 <- filter(df, server == "Enterprise Solutions", node == 1)
enterprise_node2 <- filter(df, server == "Enterprise Solutions", node == 2)
enterprise_node1_ts <- xts(enterprise_node1[,4], order.by = enterprise_node1$time)
enterprise_node1_ts <- apply.daily(enterprise_node1_ts, sum)
enterprise_node2_ts <- xts(enterprise_node2[,4], order.by = enterprise_node2$time)
enterprise_node2_ts <- apply.daily(enterprise_node2_ts, sum)

chauffeur <- filter(df, server == "Chauffeur", node == 1)
chauffeur_ts <- xts(chauffeur[,4], order.by = chauffeur$time)
chauffeur_ts <- period.apply(chauffeur_ts, endpoints(chauffeur_ts, on = "hours", k = 1), sum)

lookahead_node1 <- filter(df, server == "Lookahead", node == 1)
lookahead_node2 <- filter(df, server == "Lookahead", node == 2)
lookahead_node1_ts <- xts(lookahead_node1[,4], order.by = lookahead_node1$time)
lookahead_node1_ts <- period.apply(lookahead_node1_ts, endpoints(lookahead_node1_ts, on = "days", k = 1), sum)
lookahead_node2_ts <- xts(lookahead_node2[,4], order.by = lookahead_node2$time)
lookahead_node2_ts <- period.apply(lookahead_node2_ts, endpoints(lookahead_node2_ts, on = "days", k = 1), sum)

programe_node1 <- filter(df, server == "Programe Status", node == 1, frequency > 0)
programe_node2 <- filter(df, server == "Programe Status", node == 2, frequency > 0)
programe_node1_ts <- xts(programe_node1[,4], order.by = programe_node1$time)
programe_node1_ts <- period.apply(programe_node1_ts, endpoints(programe_node1_ts, on = "days", k = 1), sum)
programe_node2_ts <- xts(programe_node2[,4], order.by = programe_node2$time)
programe_node2_ts <- period.apply(programe_node2_ts, endpoints(programe_node2_ts, on = "days", k = 1), sum)

png("~/portal_analysis/plot.png", width=780)
ggplot(data = enterprise_node1_ts) +
  geom_point(aes(x = index(enterprise_node1_ts), y = frequency)) + 
  geom_line(aes(x = index(enterprise_node1_ts), y = frequency, color = "Node 1")) +
  geom_point(data = enterprise_node2_ts, aes(x = index(enterprise_node2_ts), y = frequency)) + 
  geom_line(data = enterprise_node2_ts, aes(x = index(enterprise_node2_ts), y = frequency, color = "Node 2")) +
  scale_x_datetime(name = "Date", date_breaks = "month") +
  scale_y_continuous(name='sum of incoming requests') +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

png("~/portal_analysis/chauffeur.png", width=780)
ggplot(data = chauffeur_ts) +
  geom_point(aes(x = index(chauffeur_ts), y = frequency)) + 
  geom_line(aes(x = index(chauffeur_ts), y = frequency, color = "Node 1")) +
  scale_x_datetime(name = "Distributed Hourly for date 2017-01-24", date_breaks = "hours") +
  scale_y_continuous(name='sum of incoming requests') +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

png("~/portal_analysis/lookahead.png", width=900)
grid.arrange(
ggplot(data = lookahead_node1_ts) +
  geom_point(aes(x = index(lookahead_node1_ts), y = frequency)) + 
  geom_line(aes(x = index(lookahead_node1_ts), y = frequency)) +
  scale_x_datetime(name = "", date_breaks = "month") +
  scale_y_continuous(name="") +  ggtitle("Node1") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)),
ggplot(data = lookahead_node2_ts) +
  geom_point(aes(x = index(lookahead_node2_ts), y = frequency)) + 
  geom_line(data = lookahead_node2_ts, aes(x = index(lookahead_node2_ts), y = frequency)) +
  scale_x_datetime(name = "", date_breaks = "month") +
  scale_y_continuous(name="") + ggtitle("Node2") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)), ncol = 2
)
dev.off()

png("~/portal_analysis/programe.png", width=900)
grid.arrange(
ggplot(data = programe_node1_ts) +
  geom_point(aes(x = index(programe_node1_ts), y = frequency)) + 
  geom_line(aes(x = index(programe_node1_ts), y = frequency)) +
  scale_x_datetime(name = "", date_breaks = "week") +
  scale_y_continuous(name="") +  ggtitle("Node1") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)),
ggplot(data = programe_node2_ts) +
  geom_point(aes(x = index(programe_node2_ts), y = frequency)) + 
  geom_line(aes(x = index(programe_node2_ts), y = frequency)) +
  scale_x_datetime(name = "", date_breaks = "week") +
  scale_y_continuous(name="") +  ggtitle("Node2") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)), ncol = 2
)
dev.off()



