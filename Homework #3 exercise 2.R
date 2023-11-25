library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(forcats)

data_RNA <- read_excel("data/WT vs dosR _ 02042020.xlsx")

data_RNA <- data_RNA %>% 
  rename(gene = gene_id, 
         log2fc = logFC,
         padj = FDR)

DE_genes <- data_RNA %>% filter(padj <= 0.05) %>%
  filter(log2fc >= 2 | log2fc <= -2) %>% 
  select(log2fc, gene, padj, logCPM) %>% 
  arrange(log2fc)

my_title <- expression(paste(italic("Mabs"), "WT vs ", italic("Mabs dosRS"), "KO"))

DE_genes <- DE_genes %>% mutate(Expression = if_else(log2fc <= -2,"Downregulated", "Upregulated")) %>% 
  group_by(Expression) 

upregulated <- filter(DE_genes, log2fc >= 2) %>%
  mutate(gene = fct_reorder(gene, log2fc))

downregulated <- filter(DE_genes, log2fc <= 2) %>%
  mutate(gene = fct_reorder(gene, log2fc, .desc = T))
 
a <- ggplot(upregulated) +
  geom_point(aes(x = log2fc, y = gene)) + 
  ggtitle("Upregulated genes") +
  labs(x = "Expression (log2FC)", y = "Gene number") +
  theme(axis.text = element_text(size = 5))

b <- ggplot(downregulated) +
  geom_point(aes(x = log2fc, y = gene)) + 
  ggtitle("Downregulated genes") +
  labs(x = "Expression (log2FC)", y = "Gene number") +
  theme(axis.text = element_text(size = 5))

grid.arrange(a, b, ncol = 2)

  coord_flip() 
  geom_segment(aes(x = 0, xend = Expression, y = 0, yend = Expression)) +
  labs(x = "", y = "Number of genes differentially expressed
       (log2FC > 2 or < -2)") +
  ggtitle(my_title) +
+
  theme_few()
