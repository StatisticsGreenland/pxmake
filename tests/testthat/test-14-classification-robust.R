# Check classification on many .vs and .agg files

test_that(" ", {
  vs_path_inverse <- function(vs_path) {
    px_classification(vs_path = vs_path) %>%
      expect_save_read_preserves_classification()
  }

  ## Keep
  vs_path_inverse(get_classification_path(file.path("statgl", "BEXSTALLlcode.vs"))) # keep, aggregation order
  vs_path_inverse(get_classification_path(file.path("statgl", "BEXSTALLlkode.vs"))) # keep, fil findes ikke

  vs_path_inverse(get_classification_path(file.path("statgl", "OFAKD.vs"))) # keep, aggregation as codes

  vs_path_inverse(get_classification_path(file.path("statgl", "OFAKN.vs")))  # # keep, head line

  ## Prospects
  vs_path_inverse(get_classification_path(file.path("statgl", "BEXSTALLlkodi.vs")))
  vs_path_inverse(get_classification_path(file.path("statgl", "OFFMD.vs"))) # head line
  vs_path_inverse(get_classification_path(file.path("statgl", "OFFMN.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAarmd.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAge.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAgec.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAgeMother.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAlder.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAlderc.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSAlderMor.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSCausesdeath.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSCausesdeathB.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSda_ie.vs"))) # fil findes ikke
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSDISTIKT.vs")))  # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSDodsarsag.vs")))  # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSDodsarsagB.vs")))  # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSen_ie.vs"))) # numerical expression
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSFSKD.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSFSKE.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSFSKN.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC11D.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC11E.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC11N.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC97D.vs"))) # compatible size 2
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC97E.vs"))) # compatible size 2
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSISC97N.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSkl_ie.vs"))) # file not exist
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSKOMD.vs"))) # compatible size 2
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSKOME.vs"))) # compatible size 2
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSKOMK.vs"))) # compatible size 2
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSLOC.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSLOK.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSToqussutaasut.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSToqussutaasutB.vs"))) # head line
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSTSKD.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSTSKE.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSTSKN.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUDGD.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUDGE.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUDGK.vs"))) # slow?
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUkiuianaanaasup.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUkiut.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUkiutc.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSUtoqqaassuseq.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSVerden.vs"))) # compatible size
  # vs_path_inverse(get_classification_path(file.path("statgl", "PXVSWorld.vs"))) # compatible size
})


#list.files(get_classification_path("statgl"), pattern = "*.\\.vs")



# c1 <- px_classification(vs_path = get_classification_path(file.path("statgl", "BEXSTALLlcode.vs")))
#
# c2 <- return_c2(c1)
