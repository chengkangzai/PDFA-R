selection = c("Strongly Disagree",
              "Disagree",
              "Neutral",
              "Agree",
              "Strongly Agree")
Accessibility = sample(selection, replace = TRUE, size = 100)
Ease_of_access = sample(selection, replace = TRUE, size = 100)
Speed = sample(selection, replace = TRUE, size = 100)
Maintainability = sample(selection, replace = TRUE, size = 100)
Availability = sample(selection, replace = TRUE, size = 100)

data = data.frame(
  Accessibility = Accessibility,
  Ease_of_access = Ease_of_access,
  Speed = Speed,
  Maintainability = Maintainability,
  Availability
)

a = ggplot(data = data, aes(Accessibility)) + geom_histogram(stat ="count")
ggplot(data = data, aes(Ease_of_access)) + geom_area()
View(data)

