make_sig_plot("N", "BFD", long_all)

make_sig_plot("PN1", "BFD", long_all)

make_sig_plot("PN2", "BFD", long_all)

make_sig_plot("PN3", "BFD", long_all)

make_sig_plot("PN4", "BFD", long_all)


make_sig_plot_gender("N1", "NFD", long_all) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# combine all pns - use stacked bars for pitch accents 

make_sig_plot("N", "BFD", long_all)


# peak later = L+<H*

# sentence by sentence word by word indvidually (not combined cities)

# look at sex differences/education/age within city 

# like figure 6: instead of N, looked at focused word in each utterance - then 
# we know which pitch accent marks narrow focus in each 



plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Montevideo", "BFD1")
plot_word_by_word("Montevideo", "BFD1")

