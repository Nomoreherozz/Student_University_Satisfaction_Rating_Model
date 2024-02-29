####enable the package
library(ggplot2)
####create data frame
predicted.data_1 <- data.frame(
  probability.of.Overall=logistic_full_no_big_std_error$fitted.values,
  Overall=train$Overall
)
predicted.data_2 <- predicted.data_1[
  order(predicted.data_1$probability.of.Overall, decreasing=FALSE),]

predicted.data_2$rank <- 1:nrow(predicted.data_2)
####create predicted graph
ggplot(data=predicted.data_2, aes(x=rank, y=probability.of.Overall)) +
  geom_point(aes(color=Overall), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of satisfaction") + 
  geom_line(aes(linetype="dotted"), size=1)