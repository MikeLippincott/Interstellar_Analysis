suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(argparse))

# define command line arguments
parser <- ArgumentParser(description = "Visualize linear modeling results")
# add arguments
parser$add_argument('--celltype', type='character', help='Cell type to visualize')

# parse arguments from command line
args <- parser$parse_args()

# define cell type
celltype <- args$celltype


celltype = "SHSY5Y"


lm_file <- file.path(paste0("./results/", celltype, "/lm_four_beta.tsv"))

lm_cp_fig <- file.path(paste0("./figures/", celltype, "/lm_four_beta.pdf"))
lm_cp_fig_abs <- file.path(paste0("./figures/", celltype, "/lm_four_beta_abs.pdf"))

# if path does not exist, create it
if (!dir.exists(file.path(paste0("./figures/", celltype)))) {
    dir.create(file.path(paste0("./figures/", celltype)))
}

lm_df <- readr::read_tsv(lm_file, col_types = readr::cols(.default = "d", feature ="c", inducer1__inducer1_dose__inhibitor__inhibitor_dose = "c"))
head(lm_df)

unique(lm_df$inducer1__inducer1_dose__inhibitor__inhibitor_dose)


# Arrange by absolute value coefficient
# Split out components of feature name for visualization
lm_df <- lm_df %>%
    dplyr::arrange(desc(abs(fourb_Inhibitor_Dose))) %>%
    tidyr::separate(
        feature,
        into = c(
            "compartment",
            "feature_group",
            "measurement",
            "channel",
            "parameter1",
            "parameter2"
        ),
        sep = "_",
        remove = FALSE
    ) %>%
    dplyr::mutate(channel_cleaned = channel)



unique(lm_df$channel)

unique(lm_df$channel_cleaned)

# Clean channel for visualization
lm_df$channel_learned <- dplyr::recode(lm_df$channel_cleaned,
        "CorrDNA" = "nuclei",
        "CorrMito" = "Mito",
        "CorrER" = "ER",
        "CorrGasdermin" = "gasdermin",
        "CorrPM" = "PM",
        .default = "other",
        .missing="other"
    )

print(dim(lm_df))
head(lm_df, 2)
unique(lm_df$channel_learned)
lm_df$abs_Metadata_number_of_singlecells <- abs(lm_df$Metadata_number_of_singlecells)
lm_df$abs_fourb_Treatment <- abs(lm_df$fourb_Treatment)
lm_df$abs_fourb_Treatment_Dose <- abs(lm_df$fourb_Treatment_Dose)
lm_df$abs_fourb_Inhibitor <- abs(lm_df$fourb_Inhibitor)
lm_df$abs_fourb_Inhibitor_Dose <- abs(lm_df$fourb_Inhibitor_Dose)

loop_list <- unique(lm_df$inducer1__inducer1_dose__inhibitor__inhibitor_dose)
x_list_abs <- c('abs_fourb_Treatment','abs_fourb_Treatment_Dose','abs_fourb_Inhibitor','abs_fourb_Inhibitor_Dose')
x_list <- c('fourb_Treatment','fourb_Treatment_Dose','fourb_Inhibitor','fourb_Inhibitor_Dose')

pdf(file=lm_cp_fig )
for (i in 1:length(loop_list)){
    df <- lm_df[lm_df$inducer1__inducer1_dose__inhibitor__inhibitor_dose == loop_list[i],]
    for (j in 1:length(x_list)){
        col = x_list[j]
        # print(x_list[j])
        # print(df[1,col])
        if (df[1,col] == 0){
            next
        } else {
            lm_fig_gg <- (
                ggplot(df, aes(x = Metadata_number_of_singlecells, y = .data[[x_list[j]]]))
                + geom_point(aes(size = r2_score, color = channel_learned), alpha = 0.7)
                + theme_bw()
                + guides(
                    color = guide_legend(title = "Channel\n(if applicable)", order = 1),
                    size = guide_legend(title = "R2 score of LM feature")
                )
                + geom_vline(xintercept = 0, linetype = "dashed", color = "red")
                + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
                + geom_density2d(color="black", show.legend = FALSE)
                + ylab(paste0(x_list[j]," contribution (LM beta coefficient)"))
                + xlab("Cell count contribution (LM beta coefficient)")
                + ggtitle(paste0("How CellProfiler features contribute\nto ",loop_list[i], "\ntreatments and cell density"))
            )
            plot(lm_fig_gg)
        }
    }
}
dev.off()

pdf(file=lm_cp_fig_abs )
for (i in 1:length(loop_list)){
    df <- lm_df[lm_df$inducer1__inducer1_dose__inhibitor__inhibitor_dose == loop_list[i],]
    for (j in 1:length(x_list_abs)){
        lm_fig_gg <- (
                ggplot(df, aes(x = Metadata_number_of_singlecells, y = .data[[x_list[j]]]))
                + geom_point(aes(size = r2_score, color = channel_learned), alpha = 0.7)
                + theme_bw()
                + guides(
                    color = guide_legend(title = "Channel\n(if applicable)", order = 1),
                    size = guide_legend(title = "R2 score of LM feature")
                )
                + geom_vline(xintercept = 0, linetype = "dashed", color = "red")
                + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
                + geom_density2d(color="black", show.legend = FALSE)
                + ylab(paste0(x_list[j]," contribution (LM beta coefficient)"))
                + xlab("Cell count contribution (LM beta coefficient)")
                + ggtitle(paste0("How CellProfiler features contribute\nto ",loop_list[i], "\ntreatments and cell density"))
            )
        plot(lm_fig_gg)
    }
}
dev.off()


