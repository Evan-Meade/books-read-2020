# 
# Evan Meade, 2021
# 
# The following is code to generate graphics which describe my reading
# habits in 2020.
# 

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
print(paste0("Estimated hours read: ", sum(books$pages) / 25))

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
books$pages.mean <- cut(books$pages, c(0, page_mean, max(books$pages)),
                        labels = c("below", "above"))
pages_bar <- ggplot(data = books) +
  geom_bar(mapping = aes(x = order, y = pages, fill = pages.mean),
           stat = "identity") +
  geom_hline(yintercept = mean(books$pages)) +
  labs(title = "Page Length in Order")
print(pages_bar)

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

# Bar chart of genres
unique_genres <- sort(unique(books$genre))
genres <- data.frame()
for (i in 1:length(unique_genres)) {
  genres[2 * i - 1, "genre"] <- unique_genres[i]
  genres[2 * i - 1, "type"] <- "# of Books"
  genres[2 * i - 1, "weight"] <- length(which(books$genre == unique_genres[i])) / nrow(books)
  
  genres[2 * i, "genre"] <- unique_genres[i]
  genres[2 * i, "type"] <- "# of Pages"
  genres[2 * i, "weight"] <- sum(books$pages[which(books$genre == unique_genres[i])]) / sum(books$pages)
}
genre_bar <- ggplot(data = genres) +
  theme_fivethirtyeight() +
  geom_bar(mapping = aes(x = type, y = weight, fill = genre), stat = "identity") +
  labs(title = "Genre Breakdown, Weighted by")
print(genre_bar)

# Genre mean page lengths
genre_lengths <- ggplot(data = aggregate(pages ~ genre, books, mean)) +
  geom_bar(mapping = aes(x = genre, y = pages, fill = genre), stat = "identity")
print(genre_lengths)

# Genres read in order
genres_bar <- ggplot(data = books) +
  geom_bar(mapping = aes(x = order, y = pages, fill = genre),
           stat = "identity") +
  geom_hline(yintercept = mean(books$pages)) +
  labs(title = "Genres Read in Order")
print(genres_bar)


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

# Bar chart of pages read in order
pages_bar <- ggplot(data = books) +
  theme_fivethirtyeight(base_size = 24) +
  geom_bar(mapping = aes(x = order, y = pages, fill = genre),
           stat = "identity") +
  geom_hline(yintercept = mean(books$pages),
             color = "#8b8b8b",
             linetype = "solid",
             size = 2) +
  labs(title = "Book Page Lengths, in Order Read") +
  scale_fill_manual(name = "Genre", values = rev(brewer.pal(length(unique_genres), "Spectral"))) +
  geom_text(mapping = aes(x = 3, y = 400, label = "Mean"), color = "#8b8b8b", size = 12)
print(pages_bar)

# Refined bar chart of genres
genre_bar_final <- ggplot(data = genres) +
  theme_fivethirtyeight(base_size = 24) +
  geom_bar(mapping = aes(x = type, y = weight, fill = genre), stat = "identity") +
  scale_fill_manual(name = "Genre", values = rev(brewer.pal(length(unique_genres), "Spectral"))) +
  labs(title = "Genre Breakdown, Weighted by")
print(genre_bar_final)
