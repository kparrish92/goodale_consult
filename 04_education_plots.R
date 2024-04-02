source(here::here("00_helpers.R"))
source(here::here("01b_load_data.R"))

ggsave("ed_mv_bfd_n.png", plot_ed_per_city("Montevideo", "BFD", "N"), path = here("round_3_plots"))
ggsave("ed_d_bfd_n.png", plot_ed_per_city("Durazno", "BFD", "N"), path = here("round_3_plots"))
ggsave("ed_mv_bfd_pn.png", plot_ed_per_city("Montevideo", "BFD", "PN"), path = here("round_3_plots"))
ggsave("ed_d_bfd_pn.png", plot_ed_per_city("Durazno", "BFD", "PN"), path = here("round_3_plots"))

ggsave("ed_d_nfd_f.png", plot_ed_per_city_nfd("Durazno", "NFD", 1), path = here("round_3_plots"))
ggsave("ed_d_nfd_uf.png", plot_ed_per_city_nfd("Durazno", "NFD", 0), path = here("round_3_plots"))
ggsave("ed_mv_nfd_f.png", plot_ed_per_city_nfd("Montevideo", "NFD", 1), path = here("round_3_plots"))
ggsave("ed_mv_nfd_uf.png", plot_ed_per_city_nfd("Montevideo", "NFD", 0), path = here("round_3_plots"))

