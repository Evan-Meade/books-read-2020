# Import libraries
library(ggplot2)

# Reading in the data
books <- read.csv("books_read.csv")

# Basic statistics
summary(books)

# Histogram of page length
page_hist <- ggplot(data = books) +
  geom_histogram(mapping = aes(x = pages), binwidth = 200) +
  labs(title = "Book Length Histogram")
print(page_hist)

# Histogram of year published
year_hist <- ggplot(data = books) +
  geom_histogram(mapping = aes(x = year.published), binwidth = 10) +
  labs(title = "Year Published Histogram")
print(year_hist)

# Scatter of page length and year published
len_year_scatter <- ggplot(data = books) +
  geom_point(mapping = aes(x = year.published, y = pages)) +
  labs(title = "Book Length vs. Year Published")
print(len_year_scatter)

# Feature for total number of pages read that year
books[1, "pages.total"] <- books[1, "pages"]
for (i in 2:nrow(books)) {
  books[i, "pages.total"] <- books[i, "pages"] + books[i - 1, "pages.total"]
}

# Total pages read over time
total_pages_line <- ggplot(data = books) +
  geom_line(mapping = aes(x = order, y = pages.total)) +
  labs(title = "Total Pages Read")
print(total_pages_line)

# Pages read in order
pages_line <- ggplot(data = books) +
  geom_line(mapping = aes(x = order, y = pages)) +
  labs(title = "Page Length in Oder")
print(pages_line)






