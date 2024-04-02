source(here::here("00_helpers.R"))
source(here::here("01b_load_data.R"))



ggsave("overall_bfd_pn.png", make_sig_plot("PN", "BFD", long_all), path = here("round_3_plots"))
ggsave("overall_bfd_n.png", make_sig_plot("N", "BFD", long_all), path = here("round_3_plots"))
ggsave("overall_nfd_f.png", make_sig_plot_f(1, long_all), path = here("round_3_plots"))
ggsave("overall_nfd_uf.png", make_sig_plot_f(0, long_all), path = here("round_3_plots"))

### Make gender sig plots

ggsave("sex_mv_bfd_n.png", plot_sex_per_city("Montevideo", "BFD", "N"), path = here("round_3_plots"))
ggsave("sex_d_bfd_n.png", plot_sex_per_city("Durazno", "BFD", "N"), path = here("round_3_plots"))
ggsave("sex_mv_bfd_pn.png", plot_sex_per_city("Montevideo", "BFD", "PN"), path = here("round_3_plots"))
ggsave("sex_d_bfd_pn.png", plot_sex_per_city("Durazno", "BFD", "PN"), path = here("round_3_plots"))

ggsave("sex_d_nfd_f.png", plot_sex_per_city_nfd("Durazno", "NFD", 1), path = here("round_3_plots"))
ggsave("sex_d_nfd_uf.png", plot_sex_per_city_nfd("Durazno", "NFD", 0), path = here("round_3_plots"))
ggsave("sex_mv_nfd_f.png", plot_sex_per_city_nfd("Montevideo", "NFD", 1), path = here("round_3_plots"))
ggsave("sex_mv_nfd_uf.png", plot_sex_per_city_nfd("Montevideo", "NFD", 0), path = here("round_3_plots"))

