suppressPackageStartupMessages(suppressWarnings(library(ggplot2)))
suppressPackageStartupMessages(suppressWarnings(library(arrow)))
suppressPackageStartupMessages(suppressWarnings(library(cowplot)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(argparser)))

# set the arg parser
parser <- arg_parser("Plot the cell count data")

parser <- add_argument(parser, "--cell_type", help = "Input file path", required = TRUE)

args <- parse_args(parser)

cell_type <- args$cell_type

# import path
cell_count_path <- file.path(paste0("./results/",cell_type,"_cell_counts.parquet"))
output_path <- file.path(paste0("./Figures/cell_counts_plate2/",cell_type,"/","cell_counts.png"))
# read in the data
cell_count_df <- read_parquet(cell_count_path)

# aggregate the data mean and sd
cell_count_df <- cell_count_df %>%
    group_by(oneb_Metadata_Treatment_Dose_Inhibitor_Dose) %>%
    summarize(mean = mean(Metadata_number_of_singlecells), sd = sd(Metadata_number_of_singlecells))
# set max cell count and min cell count
max_cell_count <- max(cell_count_df$mean)
median_cell_count <- median(cell_count_df$mean)
min_cell_count <- min(cell_count_df$mean)

options(repr.plot.width = 12, repr.plot.height = 10)
# plot the data in a bar plot
cell_count_bar <- (
    ggplot(cell_count_df, aes(x = oneb_Metadata_Treatment_Dose_Inhibitor_Dose, y = mean, fill = oneb_Metadata_Treatment_Dose_Inhibitor_Dose))
    + geom_bar(stat = "identity", position = "dodge")
    + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9))
    + labs(x = "Treatment", y = "Number of cells", title = "Number of cells per well")
    + theme_bw()
    + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    + scale_fill_manual(values=viridis::viridis(36))
    # horizontal line at 0
    + geom_hline(yintercept = min_cell_count, linetype = "dashed", color = "black")
    + geom_hline(yintercept = max_cell_count, linetype = "dashed", color = "black")
    + ylim(0, max_cell_count)
    # set the title of the plot to be centere
    + theme(plot.title = element_text(hjust = 0.5))
    + theme(axis.title.x = element_text(size = 16))
    + theme(axis.title.y = element_text(size = 16))
    + theme(axis.text.x = element_text(size = 14))
    + theme(axis.text.y = element_text(size = 14))
)
# get the legend
legend <- get_legend(cell_count_bar)
# drop the legend
cell_count_bar <- cell_count_bar + guides(fill = FALSE)
# save the plot
ggsave(output_path, plot = cell_count_bar, width = 12, height = 10, units = "in", dpi = 300)




