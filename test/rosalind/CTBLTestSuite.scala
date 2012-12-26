package rosalind

import org.scalatest.FunSuite

import rosalind.NWCK._

class CTBLTestSuite extends FunSuite with RosalindProblem {
  
  test("character table with data from problem") {
    val data = "(dog,((elephant,mouse),robot),cat);"
    val characters = Tree.parseNewick(data).characterTable

    assert( characters.length == 2)
    assert( ( characters.contains("00110") && characters.contains("00111") )
    		|| ( characters.contains("11001") && characters.contains("11000") ) )
  }

  test("character table with real data") {
    val data = "((((((((((((((((((Acanthoceros_emilia,(Cynops_porphyrio,Micropalama_plumipes)),(Larus_weliczkowskii,Pandinus_laticauda)),((Coregonus_melanostictus,Net_zagrosensis),Uroplatus_modestus)),Panthera_scincoides),Rosalia_macularius),Pterinochilus_chinensis),Vipera_geniculata),(((Alauda_lesueurii,Kinixys_mlokosiewiczi),Rosalia_miliaris),Cyclemys_ornata)),(Chrysopelea_temminskii,Neophron_gecko)),Circus_collaris),((((Anas_aestivus,(Bubulcus_nippon,Limnaeus_linaria)),(Aplopeltura_boschas,(((((Bufo_kraepelini,Underwoodisaurus_hendricksoni),Melanocoryhpa_deremensis),Mergus_perdix),(((Corytophanes_cavirostris,((Ethmostigmus_deminutus,Hottentotta_guangxiensis),(Mesoplodon_pugnax,(Ovis_schreibersi,Streptopelia_vegans)))),Phylloscopus_iguana),Phrynomerus_perdix)),Burhinus_maldivarum))),Brachyramphus_coelestinus),Phalaropus_lesueurii)),(Desnana_marmorata,Nemachilus_smaragdina)),Citharacanthus_collybitus),(Gypaetus_torquatus,Tylototriton_cyanea)),Prunella_merganser),((((Chelydra_glottis,Oligodon_heliaca),(Nipponia_adamsii,Spalerosophis_campestris)),Cyclemys_ocellatus),Gonyosoma_dendrophila)),(((((((((((((((Aegialites_isabellina,Tupinambus_guangxiensis),Platalea_mlokosiewiczi),(((((Babycurus_hassanica,Pseudemys_salamandra),(Leptobrachium_tadorna,Rissa_sp)),Spalerosophis_serricollis),((((Colaeus_argali,Trionyx_mutabilis),Pelecanus_subcinctus),Kinosternon_mugodjaricus),(((Glareola_hosii,Scaphiophryne_stejnegeri),Passer_leucophtalmos),Marmota_carinata))),Phrynosoma_mnemosyne)),Passer_avinivi),((((Bombycilla_citreola,Psammophis_himalayensis),Pelomedusa_bairdii),Bufo_peregusna),Buthacus_bonasus)),(Chondropython_variabilis,Tetrao_naumanni)),Ceratophrys_indica),Tiliqua_vitticeps),(Ptychozoon_terrestris,Sorex_leucomelas)),((Lamprolepis_cherrug,(Otis_means,Vormela_ameiva)),Tryngites_lepturus)),(((Alectoris_blakistoni,Chamaeleo_alba),(Anthropoides_shadini,Psalmopoeus_daurica)),(Madagascarophis_albirostris,Sorex_variegatus))),((((((((((Arenaria_taxus,Chlidonias_dentata),Pelusios_cristatus),Phelsuma_stylifer),(Sitta_mandarina,Tamias_hassanica)),((Damon_moschata,(((Dryobates_catenifer,Mylopharyngodon_percnopterus),Oxyura_fusca),Gavia_piceus)),Monticola_corone)),Thamnophis_cristatella),Panthera_nebularia),Corallus_iguana),Saxicola_caucasicus),Chelus_japonensis)),Xenopeltis_standingii),(Remiz_hardwickii,Spermophilus_vittatus)),Capeila_chinensis)),(((Acanthosaura_albopillosum,((((((((((((((Allobates_caudatus,(Chlamydotis_czerskii,Leptopelis_dennysii)),(Chen_livia,Madagascarophis_caeruleus)),(((((Chelus_corsac,Chettussia_cinaedus),Sphenurus_infrafrenata),Margaritifera_middendorffi),Falcipennis_aceras),(Ingerophrynus_totanus,Rhodostethia_pedo))),Hemitheconyx_subglobosa),Bombyx_heterolepidotus),Numenius_acutus),Ardea_virgo),Aquila_crassicauda),Onychodactylus_sibilans),Ingerophrynus_parreyssi),(Aphonopelma_leucomystax,Lobipes_sauromates)),Balaena_relictus),(((Desnana_salamandra,Picus_calamita),Monticola_wogura),(Ninox_guttata,(Turdus_scincoides,Upupa_personata)))),Ptychozoon_karelini)),Petrocincla_collaris),Nemorhaedus_lavaretus),(((((Acanthoscurria_paradisi,((((((((((((Allactaga_chamaeleontinus,Tamias_sp),Gazella_perdix),(Almo_pugatshuki,Capreolus_caeruleus)),(((((Calidris_catenifer,Notophthalmus_cyanogenys),Pagophila_duplus),Pedostibes_schneideri),(Mylopharyngodon_rutila,(Rhodostethia_perdix,Rhynchophis_ammon))),Lasiodora_celer)),Xenophrys_macrops),(Alopex_ichthyaetus,Melanocoryhpa_multituberculatus)),(Aythya_pulcher,Oceanodroma_fusca)),(((Bombina_iankowskii,Machetes_ferrumequinum),Dryobates_bengalensis),(Himantopus_nasuta,Pituophis_quadriocellata))),(Archispirostreptus_plumifrons,Balaena_sinensis)),Platalea_fasciolata),Lystrophis_ferruginea),(Aplopeltura_glareola,Buthacus_teguixin))),Syrrhaptes_milii),(Brachypelma_fluviatilis,(Dipsosaurus_peregrinus,Geochelone_flavirufa))),(Coregonus_melleri,Lagenorhynchus_torquatus)),Lampropeltis_fernandi));"
    val characters = Tree.parseNewick(data).characterTable

    characters foreach println
  }
}