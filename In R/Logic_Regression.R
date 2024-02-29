Logic <- glm(Overall ~ Sex +
               Program + 
               Facilities + 
               Club + 
               Environment + 
               Opportunities + 
               Faculties, data=data, family="binomial")
summary(Logic)

#Graph 
predicted.data <- data.frame(
  probability.of.Overall=Logic$fitted.values,
  Overall=data$Overall
)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.Overall, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.Overall)) +
  geom_point(aes(color=Overall), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of satisfaction")
ggsave("satisfaction_probabilities.pdf")



