# Import libraries
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

# Reading in the data
books <- read.csv("books_read.csv")


# 
# Exploration of the data
# 

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


# Pages per author
pages_bar <- ggplot(data = books) +
  geom_bar(mapping = aes(x = author, y = pages), stat = "identity") +
  labs(title = "Pages Read by Author")
print(pages_bar)

# Percent of pages done by each author
author_bar <- ggplot(data = books) +
  geom_bar(mapping = aes(x = "Author", y = pages, fill = author),
           stat = "identity",
           position = "fill") +
  labs(title = "Authors' Proportions of Pages Read")
print(author_bar)


# 
# Final Plots
# 

# Simplified percent of pages done by each author
author_pages <- aggregate(pages ~ author, books, sum)
author_pages <- author_pages[order(author_pages$pages, decreasing = TRUE), ]
rownames(author_pages) <- 1:nrow(author_pages)
simp_author_pages <- author_pages[1:5, ]
simp_author_pages[6, "author"] <- "Other (17)"
simp_author_pages[6, "pages"] <- sum(author_pages[6:nrow(author_pages), "pages"])
simp_author_pages$author <- factor(simp_author_pages$author, levels = simp_author_pages$author)
simp_author_bar <- ggplot(data = simp_author_pages) +
  theme_fivethirtyeight(base_size = 24) +
  geom_bar(mapping = aes(x = "", y = pages, fill = author),
           stat = "identity") +
  labs(title = "Total Pages Read, Segmented by Author",
       y = "Pages Read") +
  scale_fill_manual(name = "Author", values = rev(brewer.pal(6, "Spectral"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
print(simp_author_bar)

# Cleaned up histogram of page length
page_hist_clean <- ggplot(data = books) +
  theme_fivethirtyeight(base_size = 24) +
  geom_histogram(mapping = aes(x = pages), fill = "#f7766d",
                 binwidth = 200, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  scale_y_continuous(breaks = seq(0, 18, 3)) +
  labs(title = "Distribution of Book Page Lengths")
print(page_hist_clean)

# Histogram of year published
year_hist <- ggplot(data = books) +
  theme_fivethirtyeight(base_size = 24) +
  geom_histogram(mapping = aes(x = year.published), fill = "#6d904f",
                 binwidth = 10, boundary = 1920, closed = "left") +
  scale_x_continuous(breaks = seq(1920, 2020, 20)) +
  scale_y_continuous(breaks = seq(0, 15, 3)) +
  labs(title = "Distribution of Publication Years")
print(year_hist)
