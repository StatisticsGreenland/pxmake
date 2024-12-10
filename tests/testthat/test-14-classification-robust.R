# Check classification on many 'real-life' .vs and .agg files

test_that(" ", {
  vs_path_inverse <- function(vs_path) {
    px_classification(vs_path = vs_path) %>%
      expect_save_read_preserves_classification()
  }

  vs_path_inverse(get_classification_path("BEXSTALLlcode.vs"))
  vs_path_inverse(get_classification_path("BEXSTALLlkode.vs"))
  vs_path_inverse(get_classification_path("OFAKD.vs"))
  vs_path_inverse(get_classification_path("OFAKN.vs"))
  vs_path_inverse(get_classification_path("PXVSAarmd.vs"))
  vs_path_inverse(get_classification_path("PXVSAge.vs"))
})
