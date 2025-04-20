

# Install wordVectors from GitHub
install.packages(c("tidyverse", "ggplot2", "Rcpp")) 
devtools::install_github("bmschmidt/wordVectors") 

#Libraries
library(wordVectors)
library(tidyverse)
library(ggplot2)
library(Rcpp)  

#Variables
if (!file.exists("art.txt")) prep_word2vec(origin = "wvtxt", destination = "art.txt", lowercase = TRUE, bundle_ngrams = 1)

if (!file.exists("art.bin")) {
    model <- train_word2vec("art.txt", "art.bin", vectors = 150, threads = 8, window = 12, iter = 5, negative_samples = 0)
} else {
    model <- read.vectors("art.bin")
}

print(dim(model))  


#Words I will be using for word vectors
word_similarities_df <- model %>% closest_to(c("mill", "textiles"), 100)  
  word_similarities_df <- as.data.frame(word_similarities_df)  
    colnames(word_similarities_df) <- c("word", "similarity")  

print(head(word_similarities_df)) #Just a check

# Using specific similar words close and far
if (nrow(word_similarities_df) > 0) {
    word_similarities_df <- model %>% closest_to("textiles", 75)  # Limit to 75 words
      word_similarities_df <- as.data.frame(word_similarities_df)  
       colnames(word_similarities_df) <- c("word", "similarity")  

    
    word_similarities_df <- word_similarities_df %>%
        mutate(category = case_when(
            similarity >= 0.4 ~ "Closest",  
            similarity <= 0.1 ~ "Farthest",  
            TRUE ~ "Neutral"
        ))

    # Dor Chart Chart
    dot_chart_plot <- ggplot(word_similarities_df, aes(x = similarity, y = reorder(word, similarity), color = category)) +
        geom_point(size = 3) +
          geom_text(aes(label = word), hjust = 0, color = "black", size = 3, check_overlap = TRUE) +
            scale_color_manual(values = c("Closest" = "blue", "Farthest" = "red", "Neutral" = "gray")) +
             theme_minimal() +
             theme(
    #Had to change colors so could be read
            panel.background = element_rect(fill = "white"),  
            plot.background = element_rect(fill = "white"),  
            panel.grid.major = element_line(color = "black"),  
            axis.text.y = element_text(color = "black", size = 8, face = "bold")  
        ) +
        labs(title = "Nearest & Farthest Words from 'Textiles'", x = "Cosine Similarity", y = "Words")
    #printing chart
    ggsave("/Users/jenjones/wordvectors/dot_chart_plot.png", plot = dot_chart_plot, width = 14, height = 12)  
    print("Saved: dot_chart_plot.png")
} else {
    print("Error: No valid words found—dot chart not created.")
}

#Dendrogram Chart
#Wanted to broaden things a bit to include more social 
cultural_mill <- c("education", "social", "physical", "health")
  term_set <- lapply(
    cultural_mill,
    function(term) {
        if (!is.null(model[[term]])) {
            nearest_words <- model %>% closest_to(model[[term]], 20)
            nearest_words$word
        } else {
            return(NULL)
        }
    }
) %>% unlist()

print(term_set)  # Debugging step

if (length(term_set) > 0) {
    subset <- model[[term_set, average = F]]

    dendrogram_plot <- subset %>%
        cosineDist(subset) %>%
         as.dist() %>%
           hclust()
#Print the chart
    png("/Users/jenjones/wordvectors/dendrogram_plot.png", width = 800, height = 600)
      plot(dendrogram_plot, main = "Dendrogram of Cultural Terms in Textile Industry")
        dev.off()
    print("Saved: dendrogram_plot.png")
} else {
    print("Error: No valid words found for dendrogram!")
}

#Gender words comparison chart
gender_words_female <- c("feminine", "femininity", "woman", "women")
gender_words_male <- c("masculine", "masculinity", "men", "man")

word_scores <- data.frame(word = rownames(model))

word_scores$female_score <- model %>%
    cosineSimilarity(model[[gender_words_female]]) %>%
    as.vector()

word_scores$male_score <- model %>%
    cosineSimilarity(model[[gender_words_male]]) %>%
    as.vector()

word_scores$category <- ifelse(word_scores$female_score > word_scores$male_score, "Female", "Male")

comparison_plot <- ggplot(word_scores %>% filter(abs(female_score - male_score) > 0.3)) +
    geom_col(aes(x = reorder(word, female_score - male_score), y = female_score - male_score, fill = category), show.legend = TRUE) +
      coord_flip() +
        scale_fill_manual(values = c("Female" = "red", "Male" = "green")) +
          labs(title = "Gendered Word Comparison", x = "Words", y = "Relative Gender Score")
#Print the chart
ggsave("/Users/jenjones/wordvectors/gender_comparison_plot.png", plot = comparison_plot, width = 8, height = 6)
print("Saved: gender_comparison_plot.png")

# PCA chart
selected_words <- model %>% closest_to("textiles", 150)  # Increase word count to 150
selected_words <- selected_words$word  # Extract word list

word_matrix <- model[[selected_words, average = FALSE]]

pca_result <- prcomp(word_matrix, center = TRUE, scale. = TRUE)
word_positions <- data.frame(word = selected_words, x = pca_result$x[,1], y = pca_result$x[,2])

# Scatter plot of word embeddings
pca_plot <- ggplot(word_positions, aes(x = x, y = y, label = word)) +
    geom_point(color = "blue", size = 3) +
    geom_text(aes(label = word), vjust = -0.5, color = "black", size = 3, check_overlap = TRUE) +
    theme_minimal() +
    theme(
        #Needed Colors to change for readability
        panel.background = element_rect(fill = "white"),  
          plot.background = element_rect(fill = "white"),  
           panel.grid.major = element_line(color = "black"),  # Black grid lines
            axis.text.x = element_text(color = "black", size = 10),  
              axis.text.y = element_text(color = "black", size = 10)  
    ) +
    labs(title = "PCA Projection of Textile Industry Terms", x = "Principal Component 1", y = "Principal Component 2")

#Print Scattered Plot Chart
ggsave("/Users/jenjones/wordvectors/pca_plot.png", plot = pca_plot, width = 14, height = 12)
print("Saved: pca_plot.png")
