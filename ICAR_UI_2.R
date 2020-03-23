# ICAR v0.01 ####
# Shiny User Interface v0.01
# Dr Vincent Looten
# Date de création : 2020-02-17
# Date de dernière modification : 2020-03-22
# Text encoding : UTF-8 (test : éèçà)


# # Premier passage : rachis dl, epaules, canal carpien
# # Load packages  ####
# listepackages<-c("shiny","shinydashboard","tableHTML","ggplot2","lubridate","spacyr","dplyr","tidyr")
# for (pack in listepackages) {
#   if (!is.element(pack, installed.packages()[,1])){
#     install.packages(pack, dep = TRUE)
#   }
#   eval(parse(text=paste0("library(",pack,")")))
# }
# rm(pack)

ui <- dashboardPage(
  
  dashboardHeader(title = "ICAR"),
  dashboardSidebar(id="", sidebarMenu(
    # Side bar list ####
    menuItem("General", tabName = "general", icon = icon("dashboard")),
    conditionalPanel(
      condition="input.exam_display.includes('Le rachis cervical')",
      menuItem("Rachis cervical", tabName = "rachis_c", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Amputation')",
      menuItem("Amputation", tabName = "amputation", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Le rachis dorsolombaire')",
      menuItem("Rachis dorsolombaire", tabName = "rachis_dl", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les épaules')",
      menuItem("Epaules", tabName = "epaule", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les coudes')",
      menuItem("Epicondylite", tabName = "epicondylite", icon = icon("table"))
    ),
    
    conditionalPanel(
      condition = "input.exam_display.includes('Les poignets (Ténosynovite)')",
      menuItem("Ténosynovite", tabName = "tenosynovite", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les canaux carpiens')",
      menuItem("Canal Carpien", tabName = "canalcarpien", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les mains et poignets (traumatique)')",
      menuItem("Mains et poignets", tabName = "mainspoignets", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les hanches et/ou le bassin')",
      menuItem("Hanche et bassin", tabName = "hanchesbassin", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les genoux')",
      menuItem("Genoux", tabName = "genoux", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('Les chevilles et/ou les pieds')",
      menuItem("Chevilles et pieds", tabName = "cheville_pieds", icon = icon("table"))
    ),
    conditionalPanel(
      condition = "input.exam_display.includes('La dépression')",
      menuItem("Dépression", tabName = "depression", icon = icon("table"))
    ),
    conditionalPanel(
      condition="input.cadreprestation.includes('Maladie simple')",
      menuItem("Invalidité", tabName = "invalidite", icon = icon("table"))
    ),
    menuItem("Synthèse clinique", tabName = "synthesis", icon = icon("list-alt")),
    menuItem("Aide à la décision", tabName = "decisionmaking", icon = icon("dashboard"))
  )
  ),
  dashboardBody(
    
    tabItems(
      # tabitem general ####
      tabItem(tabName = "general",
              tags$h1("Interface de Convocation Assistée par oRdinateur (ICAR) - Dr Vincent Looten"),
              tags$section("Avertissement : ICAR est un outil d'aide à l'examen clinique et à la décision dans le cadre de la médecine d'assurance. Il ne se substitue pas au médecin, soyez prudents lors son l'utilisation."),
              p("Il a été décidé de ne pas intégrer les atteintes des organes mous, les maladies professionnelles non fréquentes, et les lésions stomatologiques."),
              tags$h2("Informations générales de la convocation"),
              fluidRow(
                box(
                  title = "Situation socioprofessionnelle",
                  radioButtons("gender", "Sexe :", c("Femme","Homme"),inline = T),
                  numericInput("yearbirth", "Année de naissance", value="" ,min = 1900, max = as.numeric(format(Sys.Date(), "%Y")) ),
                  textInput("work", "Métier", ""),
                  radioButtons("dominance","Bras dominant :", choices = c("Droitier","Gaucher","Ambidextre (Travailleur du bois)"), inline = T),
                  textInput("boss", "Employeur", ""),
                  radioButtons("work_time", "Temps de travail", c("Temps plein", "Temps partiel", "Interimaire","Retraité","Sans emploi"),inline = T),
                  radioButtons("mdph", "RQTH", c("Non","Dossier en cours","Oui"),inline = T),
                  textAreaInput("admin_context", "Autres observations :", "", rows = 2)
                ),
                box(
                  title = "Cadre de prestation (nécessaire pour l'aide à la décision)",
                  textInput("medecinconseil", "Nom du médecin conseil : ", "Dr Vincent Looten"),
                  radioButtons("cadreprestation", "Cadre de prestation", c("Non renseigné","Accident du travail","Maladie professionnelle", "Maladie simple : Contrôle pertinence d'un arrêt de travail", "Maladie simple : évaluation de la capacité de travail (invalidité)"),inline = T),
                  conditionalPanel(
                    condition = "input.cadreprestation=='Accident du travail' || input.cadreprestation=='Maladie professionnelle' ",
                    radioButtons("objectif_convoc", "Objectif de la convocation", c("Consolidation et fixation de séquelles","Fixation de séquelles après CFD", "Demande de rechute"),inline = T)
                  ),
                    conditionalPanel(
                      condition = "input.cadreprestation=='Accident du travail'",
                      textAreaInput("date_AT", "Date de l'AT", "", rows = 1),
                      textAreaInput("date_AT_2", "Date de la rechute (le cas échéant)", "", rows = 1),
                      conditionalPanel(
                        condition="input.objectif_convoc=='Demande de rechute'",
                        textAreaInput("date_AT_3", "Date de la dernière consolidation", "", rows = 1) 
                      ),
                      conditionalPanel(
                        condition="input.objectif_convoc=='Fixation de séquelles après CFD'",
                        textAreaInput("date_AT_4", "Date du CFD", "", rows = 1) 
                      )
                    ),
                    conditionalPanel(
                      condition = "input.cadreprestation=='Maladie professionnelle'",
                      textAreaInput("date_MP", "Date de la reconnaissance MP", "", rows = 1),
                      textAreaInput("date_MP_2", "Date de la rechute (le cas échéant)", "", rows = 1),
                      conditionalPanel(
                        condition="input.objectif_convoc=='Demande de rechute'",
                        textAreaInput("date_MP_3", "Date de la dernière consolidation", "", rows = 1) 
                      ),
                      conditionalPanel(
                        condition="input.objectif_convoc=='Fixation de séquelles après CFD'",
                        textAreaInput("date_MP_4", "Date du CFD", "", rows = 1) 
                      )
                    ),
                  conditionalPanel(
                    condition = "input.cadreprestation.includes('Maladie simple')",
                    textAreaInput("date_AS1", "Date du PIT", "", rows = 1),
                    textAreaInput("date_AS2", "Date de forclusion", "", rows = 1),
                    conditionalPanel(
                      condition = "input.cadreprestation=='Maladie simple : évaluation de la capacité de travail (invalidité)'",
                      textAreaInput("date_Invalidite1", "Date de l'invalidité catégorie 1", "", rows = 1),
                      textAreaInput("date_Invalidite1", "Date de l'invalidité catégorie 2", "", rows = 1)
                    )
                  ),
                  htmlOutput("decisionmaking_alertes1")
                )
                
              ),
              fluidRow(
                box(width=6,
                    title = "Interrogatoire",
                    textAreaInput("disease_context", "Histoire de la maladie/accident", "", rows=5),
                    textAreaInput("traitement_context", "Traitement actuel :", "", rows=3),
                    textAreaInput("doleances_context", "Doléances :", "", rows=2)                
                    ),
                box(width=6,
                    title = "",
                    textAreaInput("antecedents", "Antécédents", "", rows=4),
                    textAreaInput("documents_hist", "Documents présentés :", "", rows=10)
                ),
                box(width=6,
                    title = "Informations pour l'aide à la décision",
                    radioButtons("traitement_prevu", "Traitements prévus :", c("Aucun ou soins d'entretien","Traitements actifs susceptibles d'améliorer la situation"),inline = T),
                    radioButtons("traitement_travail", "Les soins empêchent-ils une reprise du travail ?", c("Non","Oui pour un temps plein ou un travail non adapté","Oui même pour un temps partiel ou un travail adapté"),inline = T),
                    textAreaInput("traitement_date_last_1", "Date du dernier acte thérapeutique signifiant", "", rows = 1),
                    textAreaInput("traitement_date_last_2", "Date du dernier acte diagnostique signifiant", "", rows = 1),
                    textAreaInput("traitement_date_next_1", "Date du prochain acte thérapeutique signifiant", "", rows = 1),
                    textAreaInput("traitement_date_next_2", "Date du prochain acte diagnostique signifiant", "", rows = 1)
                ),
                box(width=6,
                    title = "Que souhaitez-vous examiner ? (Affichage des modules cliniques correspondants)",
                    checkboxGroupInput("exam_display", "Rubrique à afficher :", 
                                       choices = c("Le rachis cervical","Le rachis dorsolombaire","Les épaules",
                                                   "Les coudes","Les poignets (Ténosynovite)","Les canaux carpiens",
                                                   "Les mains et poignets (traumatique)","Les hanches et/ou le bassin",
                                                   "Les genoux","Les chevilles et/ou les pieds","La dépression","Amputation"))
                )
                )
              # ,
              # fluidRow(
              #   box(title = "Synthèse intermédiaire :",width = 12,
              #       htmlOutput("intermediaire_synthese")
              #   )
              # )
              
      ),
      tabItem(tabName = "rachis_c",
              #TabItem rachis cervical ####
              tags$h1("Examen du rachis cervical"),
              radioButtons("cervical_activate", "Examen clinique du rachis cervical réalisé :", c("Non","Oui"),inline = T),
              fluidRow(
                box(title = "Mobilité sagittale et gêne fonctionnelle",width=4,
                    radioButtons("cervical_fonction", "Persistance de douleurs et gêne fonctionnelle", c("Minime","Discrète", "Modérée","Importante","Très importante"),inline = T),
                    numericInput("cervical_flexion_cm", "Flexion (Distance menton-sternum en cm) :", value=-1 ,min = -1, max =70 ),
                    numericInput("cervical_extension_cm", "Extension (Distance menton-sternum en cm) :", value=-1 ,min = -1, max =70 )
                ),
                box(title = "Mobilité cervicale à gauche",width=4,
                    numericInput("cervical_rotation_G", "Rotation à gauche en degré (N=70):", value=70 ,min = 0, max =70 ),
                    numericInput("cervical_Inclinaison_G", "Inclinaison à gauche en degré (N=45):", value=45 ,min = 0, max =70 )
                ),
                box(title = "Mobilité cervicale à droite",width=4,
                    numericInput("cervical_rotation_D", "Rotation à droite en degré (N=70):", value=70 ,min = 0, max =70 ),
                    numericInput("cervical_Inclinaison_D", "Inclinaison à droite en degré (N=45):", value=45 ,min = 0, max =70 )
                )),
              fluidRow(
                box(title = "Examen général :",
                    textAreaInput("cervical_cic", "Cicatrices :", "Aucune", rows = 2),
                    textAreaInput("cervical_obs", "Autres observations :", "", rows = 2)
                      ),
                box(title = "Examen neurologique", width = 6,
                    radioButtons("cervicoceph", "Syndrome cervico-céphalique",c("Non","Oui"),inline = T),
                    checkboxGroupInput("cervicoceph_a", "Signes accompagnateurs", choices = c("Vertiges de position avec obnubilation visuelle", "Arnoldalgie", "Point d'Erb", "Contracture du trapèze","redressement de la lordose cervicale physiologique"),inline=T)
                )
              ),
              fluidRow(
                box(title = "Atteintes neurologiques périphériques :", width=6,
                    h4("Déficit sensitif"),
                    h5("Bras Gauche :"),
                    checkboxGroupInput("deficit_neuro_sensitif_ms_g", "Déficit sensitif à Gauche", choices = c("Aucun","Non systématisé", "C3-C4", "C5-C6", "C5-C8","C6-C8","C8-T1","T1-T2","T2"),inline=T,selected="Aucun"),
                    h5("Bras Droit :"),
                    checkboxGroupInput("deficit_neuro_sensitif_ms_D", "Déficit sensitif à Droite", choices = c("Aucun","Non systématisé", "C3-C4", "C5-C6", "C5-C8","C6-C8","C8-T1","T1-T2","T2"),inline=T,selected="Aucun"),
                    h4("Déficit moteur"),
                    h5("Bras Gauche :"),
                    checkboxGroupInput("deficit_neuro_moteur_ms_g", "Déficit moteur à Gauche", choices = c("Aucun","Non systématisé", "C5", "C6", "C7","C8-T1"),inline=T,selected="Aucun"),
                    h5("Bras Droit :"),
                    checkboxGroupInput("deficit_neuro_moteur_ms_D", "Déficit moteur à Droite", choices = c("Aucun","Non systématisé", "C5", "C6", "C7","C8-T1"),inline=T,selected="Aucun")
                    ),
                box(title = "Territoires sensitifs et moteur du membre supérieur :", width=6,
                    htmlOutput("picture_cervical"),
                    p("NCBC5 / REFLEXE BICIPITAL
                    DEFICIT DE L ABDUCTION DU BRAS ET DES ROTATEURS DE L EPAULE 
                    TERRITOIRE SENSITIF : MOIGNON DE L EPAULE ET FACE EXT DU BRAS"), 
                    p("NCBC6 : REFLEXE STYLORADIAL
                    DEFICIT DE LA FLEXION DU COUDE DE LA SUPINATION ET DE LA FLEXION DU POUCE 
                    FACE ANTERIEURE DU BRAS FACE EXT DE L AVT BRAS  ET POUCE"),
                    p("NCBC7 : REFLEXE TRICIPITAL
                    DEFICIT DE L EXTENSION DU COUDE DU POIGNET ET DES DOIGTS AINSI QUE DE LA PRONATION 
                    FACE POST DU MS JUSQU AUX 2 ° ET 3 ° DOIGTS"),
                    p("NCB C8 T1 / CUBITOPRONATEUR 
                    FLEXION ECARTEMENT DES DOIGTS 
                    FACE INTERNE DU MS JUSQU AUX 4 ET 5 ° DOIGTS")
                )
              )
      ),
      tabItem(tabName = "rachis_dl",
              # tabitem rachis dorsolombaire ####
              h1("Examen du rachis dorsolombaire et de la mobilité"),
              radioButtons("rachisdl_activate", "Examen clinique du rachis dorsolombaire et de la mobilité réalisé :", c("Non","Oui"),inline = T),
              fluidRow(
                box(
                  title = "Analyse de la mobilité",
                  radioButtons("marche_1", "Marche spontanée", c("Sans boiterie","Boiterie sans aide","Avec 1 canne","Avec 2 cannes", "Fauteuil"),selected = "Sans boiterie",inline = T),
                  radioButtons("marche_2", "Marche sur les talons", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("marche_3", "Marche sur les pointes", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("accroupissement", "Accroupissement", c("Possible","Précautionneux","A mi-hauteur","Impossible","Non réalisé"),selected = "Possible",inline = T),
                  radioButtons("appuimono_D", "Appui monopodal à Droite", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  radioButtons("appuimono_G", "Appui monopodal à Gauche", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  textAreaInput("rachisdl_obs", "Autres observations :", "", rows = 1)
                ),
                box(
                  title = "Examen clinique",
                  textAreaInput("rachisdl_cic", "Cicatrices :", "Aucune", rows = 1),
                  radioButtons("allonge", "S'allonge sur la table d'examen", c("Oui","Difficilement","Avec aide","Non réalisé"),selected = "Oui",inline = T),
                  radioButtons("equerre", "Tient l'équerre", c("Oui","Difficilement","Impossible","Non réalisé"),selected = "Oui",inline = T),
                  numericInput("schober", "Indice de Schober = 15/",value = -1,min=-1,max=30),
                  checkboxInput("schober_none", "Indice de Schober non mesurable", value = FALSE, width = NULL),
                  numericInput("dms", "Distance mains-sol:",value = -1,min=-1,max=300),
                  checkboxInput("dms_none", "DMS non mesurable", value = FALSE, width = NULL)
                )
                
              ),
              fluidRow(
                box(width=6,
                    title = "Examen neurologique (Jambe gauche)",
                    checkboxGroupInput("sciatalgies_g", "Sciatalgies gauche (territoires)", choices = c("Aucune","Non systématisées", "L1", "L2", "L3","L4","L5"),inline=T,selected="Aucune"),
                    checkboxGroupInput("deficitsens_g", "Déficits sensitifs gauche (territoires)", choices = c("Aucune","Non systématisées", "L1", "L2", "L3","L4","L5"),inline=T,selected="Aucune"),
                    radioButtons("deficit_pied_g1","Déficit des releveurs du pied gauche",choices = c("Non","Oui 4/5", "Oui 3/5", "Oui 2/5", "Oui 1/5", "Oui 0/5"), inline = T,selected="Non"),
                    radioButtons("deficit_pied_g2","Déficit des extenseurs du pied gauche",choices = c("Non","Oui 4/5", "Oui 3/5", "Oui 2/5", "Oui 1/5", "Oui 0/5"), inline = T,selected="Non"),
                    textAreaInput("obs_neuro_g", "Autres observations (Gauche) :", "", rows = 5)
                    # verbatimTextOutput("synthese")
                ),
                box(width=6,
                    title = "Examen neurologique (Jambe droite)",
                    checkboxGroupInput("sciatalgies_d", "Sciatalgies droite (territoires)", choices = c("Aucune","Non systématisées", "L1", "L2", "L3","L4","L5"),inline=T,selected="Aucune"),
                    checkboxGroupInput("deficitsens_d", "Déficits sensitifs droit (territoires)", choices = c("Aucune","Non systématisées", "L1", "L2", "L3","L4","L5"),inline=T,selected="Aucune"),
                    radioButtons("deficit_pied_d1","Déficit des releveurs du pied droit",choices = c("Non","Oui 4/5", "Oui 3/5","Oui 2/5", "Oui 1/5", "Oui 0/5"), inline = T,selected="Non"),
                    radioButtons("deficit_pied_d2","Déficit des extenseurs du pied droit",choices = c("Non","Oui 4/5", "Oui 3/5","Oui 2/5", "Oui 1/5", "Oui 0/5"), inline = T,selected="Non"),
                    textAreaInput("obs_neuro_g", "Autres observations (Gauche) :", "", rows = 5)
                    # verbatimTextOutput("synthese")
                )
              ),
              fluidRow(
                box(width=12,
                    title = "Examen neurologique Global",
                    checkboxGroupInput("tb_sphincter", "Troubles spinctériens", choices = c("Aucun","Anesthésies en selle", "Troubles de l'érection", "Troubles urinaires", "Incontinence fécale"),inline=T,selected="Aucun"),
                    radioButtons("rotulien", "Réflexes rotulien", choices = c("Vifs et symétrique","Non retrouvés", "Diminué à G", "Diminué à D"),inline=T),
                    radioButtons("achileen", "Réflexes achiléen", choices = c("Vifs et symétrique","Non retrouvés", "Diminué à G", "Diminué à D"),inline=T),
                    textAreaInput("obs_neuro", "Autres observations (Global) :", "", rows = 5)
                    # verbatimTextOutput("synthese")
                ),
                box(width=6,
                    title = "Examen neurologique périphérique - Membre Inférieur Gauche",
                    radioButtons("snp_mi_g_glob", "Examen neurologique périphérique gauche", choices = c("Normal","altéré"),inline=T,selected="Normal"),
                    conditionalPanel(
                      condition ="input.snp_mi_g_glob == 'altéré'",
                      checkboxGroupInput("snp_mi_g1", "Atteinte du SNP MI Gauche (force <4/5)", 
                                         choices = c("Paralysie totale d'un membre inférieur flasque",
                                                     "Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)",
                                                     "Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)",
                                                     "Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)",
                                                     "Paralysie du nerf crural (quadriceps)",
                                                     "Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)"),inline=T),
                      checkboxGroupInput("snp_mi_g2", "Atteinte du SNP MI Gauche (force =4/5)", 
                                         choices = c("Paralysie totale d'un membre inférieur flasque",
                                                     "Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)",
                                                     "Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)",
                                                     "Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)",
                                                     "Paralysie du nerf crural (quadriceps)",
                                                     "Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)"),inline=T),
                      textAreaInput("snp_mi_obs_g", "Autres observations (SNP MI Gauche) :", "", rows = 5)  
                    )
                    # verbatimTextOutput("synthese")
                ),
                box(width=6,
                    title = "Examen neurologique périphérique - Membre Inférieur Droit",
                    radioButtons("snp_mi_d_glob", "Examen neurologique périphérique droit", choices = c("Normal","altéré"),inline=T,selected="Normal"),
                    conditionalPanel(
                      condition ="input.snp_mi_d_glob == 'altéré'",
                      checkboxGroupInput("snp_mi_d1", "Atteinte du SNP MI Droit (force <4/5)", 
                                         choices = c("Paralysie totale d'un membre inférieur flasque",
                                                     "Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)",
                                                     "Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)",
                                                     "Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)",
                                                     "Paralysie du nerf crural (quadriceps)",
                                                     "Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)"),inline=T),
                      checkboxGroupInput("snp_mi_d2", "Atteinte du SNP MI Droit (force =4/5)", 
                                         choices = c("Paralysie totale d'un membre inférieur flasque",
                                                     "Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)",
                                                     "Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)",
                                                     "Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)",
                                                     "Paralysie du nerf crural (quadriceps)",
                                                     "Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)"),inline=T),
                      textAreaInput("snp_mi_obs_d", "Autres observations (SNP MI Droit) :", "", rows = 5)
                      
                    )

                )
                
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "epaule",
              #tabItem examen des épaules ####
              h1("Examen des épaules"),
              radioButtons("epaule_activate", "Examen clinique des épaules réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(width=6,
                    title = "Examen de l'épaule gauche",
                    checkboxGroupInput("signes_epaule_g", "Signes observés ou allégués de l'épaule gauche", 
                                       choices =c("Impotence fonctionnelle",
                                                  "Douleurs avec réveil nocturne",
                                                  "Impossibilité de réaliser les gestes de la vie quotidienne",
                                                  "Impossibilité de réaliser les gestes de la vie professionnelle",
                                                  "Abaissement (ou d’élévation) du moignon de l’épaule",
                                                  "Amyotrophie des masses sus et sous-épineuses"),inline=T),
                    radioButtons("cal_epaule_g","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    textAreaInput("epaule_g_obs", "Autres observations :", "", rows = 3)
                ),
                box(width=6,
                    title = "Examen de l'épaule droite",
                    checkboxGroupInput("signes_epaule_d", "Signes observés ou allégués de l'épaule droite", 
                                       choices =c("Impotence fonctionnelle",
                                                  "Douleurs avec réveil nocturne",
                                                  "Impossibilité de réaliser les gestes de la vie quotidienne",
                                                  "Impossibilité de réaliser les gestes de la vie professionnelle",
                                                  "Abaissement (ou d’élévation) du moignon de l’épaule",
                                                  "Amyotrophie des masses sus et sous-épineuses"),inline=T),
                    radioButtons("cal_epaule_d","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    textAreaInput("epaule_d_obs", "Autres observations :", "", rows = 3)
                    
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Mouvements complexes de l'épaule gauche",
                    numericInput("mvt_cplx_g1", "Main G - Epaule D en cm :", value = -1, max = 100, min = -1),
                    numericInput("mvt_cplx_g2", "Main G - tête en cm :", value = -1, max = 100, min = -1),
                    numericInput("mvt_cplx_g3", "Main G - omoplate :", value = -1, max = 100, min = -1)
                ),
                box(width=6,
                    title = "Mouvements complexes de l'épaule droite",
                    numericInput("mvt_cplx_d1", "Main D - Epaule G en cm :", value = -1, max = 100, min = -1),
                    numericInput("mvt_cplx_d2", "Main D - tête en cm :", value = -1, max = 100, min = -1),
                    numericInput("mvt_cplx_d3", "Main D - omoplate :", value = -1, max = 100, min = -1)
                )
                
              ),
              fluidRow(
                box(width=6,
                    title = "Mobilités de l'épaule gauche",
                    numericInput("mob_act_g1", "Antepulsion gauche active :", value = -1, max = 180, min = -1),
                    numericInput("mob_pas_g2", "Antepulsion gauche passive:", value = -1, max = 180, min = -1),
                    numericInput("mob_act_g3", "Abduction gauche active :", value = -1, max = 170, min = -1),
                    numericInput("mob_pas_g4", "Abduction gauche passive :", value = -1, max = 170, min = -1)
                ),
                box(width=6,
                    title = "Mobilités de l'épaule droite",
                    numericInput("mob_act_d1", "Antepulsion droite active :", value = -1, max = 180, min = -1),
                    numericInput("mob_pas_d2", "Antepulsion droite passive:", value = -1, max = 180, min = -1),
                    numericInput("mob_act_d3", "Abduction droite active :", value = -1, max = 170, min = -1),
                    numericInput("mob_pas_d4", "Abduction droite passive :", value = -1, max = 170, min = -1)
                )
                
              ),
              fluidRow(
                box(width=6,
                    title = "Mensurations de l'épaule gauche",
                    numericInput("mens_g1", "Creux axillaire vertical", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g2", "Biceps (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g3", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1,min = -1, max =70 ),
                    h5("Force de préhension (Jamar) :"),
                    numericInput("jamar_g1", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g2", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g3", "Troisième essai", value=-1 ,min = -1, max =100 )
                ),
                box(width=6,
                    title = "Mensurations de l'épaule droite",
                    numericInput("mens_d1", "Creux axillaire vertical", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d2", "Biceps (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d3", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    h5("Force de préhension (Jamar) :"),
                    numericInput("jamar_d1", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d2", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d3", "Troisième essai", value=-1 ,min = -1, max =100 )                )
                
              )
      ),
      
      tabItem(tabName = "canalcarpien",
              #TabItem canal carpien ####
              h1("Examen du canal carpien"),
              radioButtons("cc_activate", "Examen clinique du canal carpien réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(width=6,
                    title = "Examen du canal carpien gauche",
                    checkboxGroupInput("cc_douleurs_g", "Sensibilité locale face antérieure poignet : ", choices = c("Non douloureux","Douloureux à la palpation", "Douloureux spontanément"),inline=T),
                    checkboxGroupInput("cc_fonctionnel_g", "Signes sensitifs : ", choices = c("Minime","acroparesthésies occasionnelles", "acroparesthésies à la mobilisation","Acroparesthésies permanentes non insomniantes","Acroparesthésies permanentes et insonmiantes","engourdissement matinal de la main"),inline=T),
                    checkboxGroupInput("cc_sensitif_g", "Gène fonctionnelle : ", choices = c("Minime","Diminution force musculaire globale de la main", "Gène à la préhension (maladresse)"),inline=T),
                    textAreaInput("cc_g_cic", "Cicatrice(s) à gauche (taille, aspect, douleur) :", "Sans particularité", rows = 3),
                    radioButtons("cc_g_amyo_the", "Amyotrophie thénardienne :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cc_g_tbvaso", "Troubles vasomoteurs :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    checkboxGroupInput("cc_neuro_g", "Troubles de la sensibilité superficielle  : ", choices = c("Pas de trouble de la sensibilité superficielle","Hypoesthésie ou une anesthésie des doigts dans le territoire du médian", "Troubles de la sensibilité épicritique (test du thrombone)"), selected = "Pas de trouble de la sensibilité superficielle"),
                    checkboxGroupInput("cc_pincepp_g", "Pince pouce-index  : ", choices = c("Normale en forme et en force","Altérée en forme", "Altérée en force 4/5", "Altérée en force 3/5"), selected = "Normale en forme et en force"),
                    checkboxGroupInput("cc_pincepd_g", "Pince pollicidigitale  : ", choices = c("Normale en forme et en force","Altérée en forme", "Altérée en force 4/5", "Altérée en force 3/5"), selected = "Normale en forme et en force"),
                    radioButtons("cc_g_force_abd", "Diminution de la force d’abduction du pouce :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    numericInput("cc_g_opp_p", "Distance entre le pouce et la  base du 5eme doigts", value=-1 ,min = -1, max =30 ),   
                    textAreaInput("cc_g_fct_art", "Altérations des fonctions articulaires (si relation causale) :", "Pas d'altération des fonctions articulaires en lien avec la maladie professionnelle", rows = 3),
                    h5("Force de préhension à gauche (Jamar) :"),
                    numericInput("jamar_g1_cc", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g2_cc", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g3_cc", "Troisième essai", value=-1 ,min = -1, max =100 )
                ),
                box(width=6,
                    title = "Examen du canal carpien droit",
                    checkboxGroupInput("cc_douleurs_d", "Sensibilité locale face antérieure poignet : ", choices = c("Non douloureux","Douloureux à la palpation", "Douloureux spontanément"),inline=T),
                    checkboxGroupInput("cc_fonctionnel_d", "Signes sensitifs : ", choices = c("Minime","acroparesthésies occasionnelles", "acroparesthésies à la mobilisation","Acroparesthésies permanentes non insomniantes","Acroparesthésies permanentes et insonmiantes","engourdissement matinal de la main"),inline=T),
                    checkboxGroupInput("cc_sensitif_d", "Gène fonctionnelle : ", choices = c("Minime","Diminution force musculaire globale de la main", "Gène à la préhension (maladresse)"),inline=T),
                    textAreaInput("cc_d_cic", "Cicatrice(s) à gauche (taille, aspect, douleur) :", "Sans particularité", rows = 3),
                    radioButtons("cc_d_amyo_the", "Amyotrophie thénardienne :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cc_d_tbvaso", "Troubles vasomoteurs :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    checkboxGroupInput("cc_neuro_d", "Troubles de la sensibilité superficielle  : ", choices = c("Pas de trouble de la sensibilité superficielle","Hypoesthésie ou une anesthésie des doigts dans le territoire du médian", "Troubles de la sensibilité épicritique (test du thrombone)"),selected = "Pas de trouble de la sensibilité superficielle"),
                    checkboxGroupInput("cc_pincepp_d", "Pince pouce-index  : ", choices = c("Normale en forme et en force","Altérée en forme", "Altérée en force 4/5", "Altérée en force 3/5"),selected = "Normale en forme et en force"),
                    checkboxGroupInput("cc_pincepd_d", "Pince pollicidigitale  : ", choices = c("Normale en forme et en force","Altérée en forme", "Altérée en force 4/5", "Altérée en force 3/5"), selected = "Normale en forme et en force"),
                    radioButtons("cc_d_force_abd", "Diminution de la force d’abduction du pouce :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    numericInput("cc_d_opp_p", "Distance entre le pouce et la  base du 5eme doigts", value=-1 ,min = -1, max =30 ),   
                    textAreaInput("cc_d_fct_art", "Altérations des fonctions articulaires (si relation causale) :", "Pas d'altération des fonctions articulaires en lien avec la maladie professionnelle", rows = 3),
                    h5("Force de préhension à droite (Jamar) :"),
                    numericInput("jamar_d1_cc", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d2_cc", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d3_cc", "Troisième essai", value=-1 ,min = -1, max =100 )
                )
              )
      ),
      
      tabItem(tabName = "epicondylite",
              #TabItem Epicondylite ####
              h1("Examen des coudes (épicondylites et épitrochléite notamment)"),
              radioButtons("epicondylite_activate", "Examen clinique des coudes réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(width=6,
                    title = "Examen du coude gauche",
                    textAreaInput("coude_g_signessubj_all","Signes subjectifs allégués :",value = "Aucun",rows = 2),
                    textAreaInput("coude_g_genefonct_all","Gêne fonctionnelle alléguée :",value = "Aucun",rows = 2),
                    numericInput("coude_g_portcharge","Port de charge maximum en kg :",value = -1,min = 0,max=40),
                    textAreaInput("coude_g_morpho","Morphologie du coude  :",value = "Conservée à la flexion et à l'extension",rows = 2),
                    textAreaInput("coude_g_amyo","Amyotrophie locale  :",value = "Pas d'amyotrophie cliniquement visible",rows = 2),
                    textAreaInput("coude_g_tum","Tuméfaction musculaire :",value = "Pas de tuméfaction cliniquement visible",rows = 2),
                    textAreaInput("coude_g_cic","Cicatrice :",value = "Pas de cicatrice visible",rows = 2),
                    textAreaInput("coude_g_douleur_int","Douleur provoquée à l'épicondyle interne",value = "Non",rows = 2),
                    textAreaInput("coude_g_douleur_ext","Douleur provoquée à l'épicondyle externe :",value = "Non",rows = 2),
                    h5("Douleurs épitrochléennes provoquées par les mouvements contrariés : "),
                    radioButtons("coude_g_mvt1","...de flexion du poignet et des doigts :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    radioButtons("coude_g_mvt2","...de pronation du poignet :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    h5("Douleurs épicondyliennes provoquées par les mouvements contrariés : "),
                    radioButtons("coude_g_mvt3","...d’extension des doigts longs :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    radioButtons("coude_g_mvt4","...de supination du poignet :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    h5("Mobilité articulaire : "),
                    numericInput("coude_g_flessum","Flessum :",value = -1,min = -1,max=180),
                    radioButtons("coude_g_mob1","Mobilité de l'articulation :", choices = c("Normale", "Limitation des mouvements de flexion-extension","Blocage de la flexion-extension"), selected ="Normale" ,inline = T),
                    conditionalPanel(
                      condition = "input.coude_g_mob1 == 'Limitation des mouvements de flexion-extension'",
                      radioButtons("coude_g_mob2","Angle de limitation :",choices = c("Mouvements conservés de 70 à 145 degrés","Mouvements conservés autour de l'angle favorable (60-100 degrés)","Mouvements conservés de 0 à 70 degrés"), selected = "Mouvements conservés autour de l'angle favorable (60-100 degrés)", inline=T)
                      ),
                    conditionalPanel(
                      condition = "input.coude_g_mob1 == 'Blocage de la flexion-extension'",
                      radioButtons("coude_g_mob3","Blocage articulaire :",choices = c("Angle favorable (60-100)","Angle défavorable (de 100 à 145 ou de 0 à 60)"), selected = "Angle favorable (60-100)", inline=T)
                    )
                ),
                box(width=6,
                    title = "Examen du coude droit",
                    textAreaInput("coude_d_signessubj_all","Signes subjectifs allégués :",value = "Aucun",rows = 2),
                    textAreaInput("coude_d_genefonct_all","Gêne fonctionnelle alléguée :",value = "Aucun",rows = 2),
                    numericInput("coude_d_portcharge","Port de charge maximum en kg :",value = -1,min = 0,max=40),
                    textAreaInput("coude_d_morpho","Morphologie du coude  :",value = "Conservée à la flexion et à l'extension",rows = 2),
                    textAreaInput("coude_d_amyo","Amyotrophie locale  :",value = "Pas d'amyotrophie cliniquement visible",rows = 2),
                    textAreaInput("coude_d_tum","Tuméfaction musculaire :",value = "Pas de tuméfaction cliniquement visible",rows = 2),
                    textAreaInput("coude_d_cic","Cicatrice :",value = "Pas de cicatrice visible",rows = 2),
                    textAreaInput("coude_d_douleur_int","Douleur provoquée à l'épicondyle interne",value = "Non",rows = 2),
                    textAreaInput("coude_d_douleur_ext","Douleur provoquée à l'épicondyle externe :",value = "Non",rows = 2),
                    h5("Douleurs épitrochléennes provoquées par les mouvements contrariés : "),
                    radioButtons("coude_d_mvt1","...de flexion du poignet et des doigts :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    radioButtons("coude_d_mvt2","...de pronation du poignet :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    h5("Douleurs épicondyliennes provoquées par les mouvements contrariés : "),
                    radioButtons("coude_d_mvt3","...d’extension des doigts longs :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    radioButtons("coude_d_mvt4","...de supination du poignet :",choices = c("Aucune","Minime","Modérée","Importante"), selected ="Aucune" ,inline = T),
                    h5("Mobilité articulaire : "),
                    numericInput("coude_d_flessum","Flessum :",value = -1,min = -1,max=180),
                    radioButtons("coude_d_mob1","Mobilité de l'articulation :", choices = c("Normale", "Limitation des mouvements de flexion-extension","Blocage de la flexion-extension"), selected ="Normale" ,inline = T),
                    conditionalPanel(
                      condition = "input.coude_d_mob1 == 'Limitation des mouvements de flexion-extension'",
                      radioButtons("coude_d_mob2","Angle de limitation :",choices = c("Mouvements conservés de 70 à 145 degrés","Mouvements conservés autour de l'angle favorable (60-100 degrés)","Mouvements conservés de 0 à 70 degrés"), selected = "Mouvements conservés autour de l'angle favorable (60-100 degrés)", inline=T)
                    ),
                    conditionalPanel(
                      condition = "input.coude_d_mob1 == 'Blocage de la flexion-extension'",
                      radioButtons("coude_d_mob3","Blocage articulaire :",choices = c("Angle favorable (60-100)","Angle défavorable (de 100 à 145 ou de 0 à 60)"), selected = "Angle favorable (60-100)", inline=T)
                    )
                )
              )
      ),
      
      tabItem(tabName = "tenosynovite",
              #TabItem Tenosynovite ####
              h1("Examen des avant-bras (Ténosynovite de De Quervain)"),
              radioButtons("tenosynovite_activate", "Examen clinique des avant-bras réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(width=6,
                    title = "Examen du côté gauche",
                    textAreaInput("poignet_g_douleurstyloide","Douleur de la styloïde radiale :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_g_genefct","Gêne fonctionnelle :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_g_tumext","Tuméfaction bord externe poignet :",value = "Non",rows = 2),
                    textAreaInput("poignet_g_cic","Cicatrice :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_g_dlpalp","Douleur provoquée à la palpation (si oui précisez l'intensité) :",value = "Aucune",rows = 2),
                    h5("Douleurs styloïdiennes déclenchées par les mouvements contrariés :"),
                    textAreaInput("poignet_g_lgabd","Long abducteur et court extenseur du pouce :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_g_finkelstein","Test de Finkelstein :",value = "Négatif",rows = 2),
                    h5("Mensurations de l'épaule gauche"),
                    numericInput("mens_g1_querv", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g2_querv", "Poignet", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g3_querv", "Gantier", value=-1 ,min = -1, max =70 ),
                    h5("Force de préhension à gauche(Jamar) :"),
                    numericInput("jamar_g1_querv", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g2_querv", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g3_querv", "Troisième essai", value=-1 ,min = -1, max =100 )
                ),
                box(width=6,
                    title = "Examen du côté droit",
                    textAreaInput("poignet_d_douleurstyloide","Douleur de la styloïde radiale :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_d_genefct","Gêne fonctionnelle :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_d_tumext","Tuméfaction bord externe poignet :",value = "Non",rows = 2),
                    textAreaInput("poignet_d_cic","Cicatrice :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_d_dlpalp","Douleur provoquée à la palpation (si oui précisez l'intensité) :",value = "Aucune",rows = 2),
                    h5("Douleurs styloïdiennes déclenchées par les mouvements contrariés :"),
                    textAreaInput("poignet_d_lgabd","Long abducteur et court extenseur du pouce :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_d_finkelstein","Test de Finkelstein :",value = "Négatif",rows = 2),
                    h5("Mensurations de l'épaule droite"),
                    numericInput("mens_d1_querv", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d2_querv", "Poignet", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d3_querv", "Gantier", value=-1 ,min = -1, max =70 ),
                    h5("Force de préhension à droite (Jamar) :"),
                    numericInput("jamar_d1_querv", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d2_querv", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d3_querv", "Troisième essai", value=-1 ,min = -1, max =100 )
                )
              )
      ),
      tabItem(tabName = "mainspoignets",
              #TabItem Mains et poignets ####
              h1("Examen des mains et poignets"),
              radioButtons("mainspoignets_activate", "Examen clinique des mains et poignets réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(width=6,
                    title = "Examen du poignet gauche",
                    textAreaInput("poignet_g_genefct_2","Gêne fonctionnelle :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_g_signesubj_2","Signes subjectifs  :",value = "Aucun",rows = 2),
                    textAreaInput("poignet_g_aspect_2","Aspect (déformation, désaxation) :",value = "Normal",rows = 2),
                    textAreaInput("poignet_g_cic_2","Cicatrice :",value = "Aucune",rows = 2),
                    checkboxGroupInput("poignet_g_vasomoteur_2","Troubles vasomoteurs", 
                                       choices = c("Aucun","Cyanose de la peau","Rougeur de la peau",
                                                   "Chaud à la palpation","Froid à la palpation",
                                                   "Oedème"),inline = T,selected = "Aucun"),
                    radioButtons("cal_poignet_g","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    h3("Mobilité du poignet gauche :"),
                    numericInput("poignet_g_flexion_act", "Flexion active (N=80)", value=80 ,min = 0, max =100 ),
                    numericInput("poignet_g_extension_act", "Extension active (N=45)", value=45 ,min = 0, max =100 ),
                    numericInput("poignet_g_abd_act", "Abduction active (inclinaison radiale) (N=15)", value=15 ,min = 0, max =100 ),
                    numericInput("poignet_g_add_act", "Adduction active (inclinaison cubitale) (N=40)", value=40 ,min = 0, max =100 ),
                    
                    radioButtons("poignet_g_activate_passive", "Examen des mobilités passives réalisé :",choices = c("Non","Oui"), inline = T),
                    conditionalPanel(
                      condition="input.poignet_g_activate_passive=='Oui'",
                      numericInput("poignet_g_flexion_pas", "Flexion passive (N=80)", value=80 ,min = 0, max =100 ),
                      numericInput("poignet_g_extension_pas", "Extension passive (N=45)", value=45 ,min = 0, max =100 ),
                      numericInput("poignet_g_abd_pas", "Abduction passive (inclinaison radiale) (N=15)", value=15 ,min = 0, max =100 ),
                      numericInput("poignet_g_add_pas", "Adduction passive (inclinaison cubitale) (N=40)", value=40 ,min = 0, max =100 )
                      
                    ),
                    radioButtons("poignet_g_pronation_amplitude", "Pronation (amplitude):", choices = c("Normale","Diminuée de moitiée","Diminuée des 3/4","Impossible"),inline = T),
                    radioButtons("poignet_g_pronation_force", "Pronation (force):", choices = c("Normale (5/5)","Diminuée (4/5)","Diminuée (<3/5)"),inline = T),
                    radioButtons("poignet_g_suppination_amplitude", "Suppination (amplitude):", choices = c("Normale","Diminuée de moitiée","Diminuée des 3/4","Impossible"),inline = T),
                    radioButtons("poignet_g_suppination_force", "Suppination (force):", choices = c("Normale (5/5)","Diminuée (4/5)","Diminuée (<3/5)"),inline = T),
                    h5("Mensurations à gauche en cm"),
                    numericInput("mens_g0_pm", "Biceps (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g1_pm", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g2_pm", "Poignet", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g3_pm", "Gantier", value=-1 ,min = -1, max =70 ),
                    h5("Force de préhension à gauche (Jamar) :"),
                    numericInput("jamar_g1_pm", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g2_pm", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_g3_pm", "Troisième essai", value=-1 ,min = -1, max =100 )
                ),
                box(width=6,
                    title = "Examen du poignet droit",
                    textAreaInput("poignet_d_genefct_2","Gêne fonctionnelle :",value = "Aucune",rows = 2),
                    textAreaInput("poignet_d_signesubj_2","Signes subjectifs  :",value = "Aucun",rows = 2),
                    textAreaInput("poignet_d_aspect_2","Aspect (déformation, désaxation) :",value = "Normal",rows = 2),
                    textAreaInput("poignet_d_cic_2","Cicatrice :",value = "Aucune",rows = 2),
                    checkboxGroupInput("poignet_d_vasomoteur_2","Troubles vasomoteurs", 
                                       choices = c("Aucun","Cyanose de la peau","Rougeur de la peau",
                                                   "Chaud à la palpation","Froid à la palpation",
                                                   "Oedème"),inline = T,selected = "Aucun"),
                    radioButtons("cal_poignet_d","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    h3("Mobilité du poignet droit :"),
                    numericInput("poignet_d_flexion_act", "Flexion active (N=80)", value=80 ,min = 0, max =100 ),
                    numericInput("poignet_d_extension_act", "Extension active (N=45)", value=45 ,min = 0, max =100 ),
                    numericInput("poignet_d_abd_act", "Abduction active (inclinaison radiale) (N=15)", value=15 ,min = 0, max =100 ),
                    numericInput("poignet_d_add_act", "Adduction active (inclinaison cubitale) (N=40)", value=40 ,min = 0, max =100 ),
                    radioButtons("poignet_d_activate_passive", "Examen des mobilités passives réalisé :",choices = c("Non","Oui"), inline = T),
                    conditionalPanel(
                      condition="input.poignet_d_activate_passive=='Oui'",
                      numericInput("poignet_d_flexion_pas", "Flexion passive (N=80)", value=80 ,min = 0, max =100 ),
                      numericInput("poignet_d_extension_pas", "Extension passive (N=45)", value=45 ,min = 0, max =100 ),
                      numericInput("poignet_d_abd_act", "Abduction passive (inclinaison radiale) (N=15)", value=15 ,min = 0, max =100 ),
                      numericInput("poignet_d_add_act", "Adduction passive (inclinaison cubitale) (N=40)", value=40 ,min = 0, max =100 )
                      
                    ),
                    radioButtons("poignet_d_pronation_amplitude", "Pronation (amplitude):", choices = c("Normale","Diminuée de moitiée","Diminuée des 3/4","Impossible"),inline = T),
                    radioButtons("poignet_d_pronation_force", "Pronation (force):", choices = c("Normale (5/5)","Diminuée (4/5)","Diminuée (<3/5)"),inline = T),
                    radioButtons("poignet_d_suppination_amplitude", "Suppination (amplitude):", choices = c("Normale","Diminuée de moitiée","Diminuée des 3/4","Impossible"),inline = T),
                    radioButtons("poignet_d_suppination_force", "Suppination (force):", choices = c("Normale (5/5)","Diminuée (4/5)","Diminuée (<3/5)"),inline = T),
                    h5("Mensurations à droite en cm"),
                    numericInput("mens_d0_pm", "Biceps (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d1_pm", "Cône antébrachial (mesuré à 10cm du pli du coude)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d2_pm", "Poignet", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d3_pm", "Gantier", value=-1 ,min = -1, max =70 ),
                    h5("Force de préhension à gauche (Jamar) :"),
                    numericInput("jamar_d1_pm", "Premier essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d2_pm", "Deuxième essai", value=-1 ,min = -1, max =100 ),
                    numericInput("jamar_d3_pm", "Troisième essai", value=-1 ,min = -1, max =100 )
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Examen de la main gauche",
                    h4("Examen global de la main gauche :"),
                    radioButtons("main_g_pince_pi_forme","Pince pouce-index en forme",  choices = c("Normale en forme","Altérée en forme","Impossible"),selected = "Normale en forme", inline = T),
                    radioButtons("main_g_pince_pi_force","Pince pouce-index en force",  choices = c("Normale en force","Altérée en force (4/5)","Très altérée en force (<3/5)"),selected = "Normale en force", inline = T),
                    radioButtons("main_g_pince_ppd_forme","Pince pollicidigitale en forme",  choices = c("Normale en forme","Altérée en forme","Impossible"),selected = "Normale en forme", inline = T),
                    radioButtons("main_g_pince_ppd_force","Pince pollicidigitale force",  choices = c("Normale en force","Altérée en force (4/5)","Très altérée en force (<3/5)"),selected = "Normale en force", inline = T),
                    textAreaInput("main_g_pince_ppd_obs","Pince pollicidigitale (précisions)  :",value = "",rows = 2),
                    h4("Examen fonctionnel de la main gauche :"),
                    radioButtons("main_g_activate_examfunct","Examen fonctionnel de la main gauche :", choices = c("Normal","Altéré"), selected = "Normal", inline = T),
                    conditionalPanel(
                      condition="input.main_g_activate_examfunct=='Altéré'",
                    radioButtons("main_fonct_g_item1","Pince unguéale (ramassage d'une allumette ou d'une épingle)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item2","Pince pulpo-pulpaire (plaquette de plastique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item3","Pince pulpo-latérale (plaquette de plastique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item4","Pince tripode (haut de la boîte cylindrique, manche d'outil, pinceau)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item5","Empaumement (boîte de conserves, manche, pinceau)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item6","Crochet (poignée)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                    radioButtons("main_fonct_g_item7","Prise sphérique (haut de la boîte cylindrique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE")
                    ),
                    h4("Examen des articulations de la main gauche :"),
                    radioButtons("main_g_activate_articulations","Examen des articulations de la main gauche :",  choices = c("Normal","Altéré","Altéré avec un équivalent-amputation"), selected = "Normal",inline = T),
                    conditionalPanel(
                      condition="input.main_g_activate_articulations!='Normal'",
                      radioButtons("main_g_carpometacarp_1","Blocage de la colonne du pouce articulaire ou extra-articulaire :",
                                   choices = c("Pas de blocage","En position de fonction (anté-pulsion et opposition)","Luxation carpo-métacarpienne ancienne, non réduite, à l'exclusion du pouce",
                                               "En position défavorable (adduction, rétropulsion)", "Equivalent d'une amputation"),selected =  "Pas de blocage", inline = T),
                      radioButtons("main_g_metacarpophal_1","Articulation métacarpo-phalangienne du pouce :", choices=c("Pas de blocage","Blocage en semi-flexion ou en extension", "Blocage en flexion complète","Laxité articulaire par rupture ou luxation ancienne du pouce non réduite", "Equivalent d'une amputation"),selected =  "Pas de blocage", inline = T),
                      radioButtons("main_g_interphalangienne_1","Articulation inter-phalangienne du pouce:", choices=c("Normale","Blocage en flexion complète", "Blocage en semi-flexion ou en extension ou luxation ancienne non réduite","Diminuée en amplitude", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_g_index_1","Mobilité de l'index :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_g_medius_1","Mobilité du medius :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_g_annulaire_1","Mobilité de l'annulaire :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T)
                      
                    ),
                    conditionalPanel(
                      condition="input.main_g_activate_articulations=='Altéré avec un équivalent-amputation'",
                      radioButtons("amputation_pouce_g_12","Amputation d'une partie du pouce :",choices = c("Pas d'amputation","Pouce entier avec le premier métacarpien","Les deux phalanges du pouce","Phalange unguéale"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_index_g_12","Amputation d'une partie de l'index :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale seule"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_medius_g_12","Amputation d'une partie du medius :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale seule"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_annulaire_g_12","Amputation d'une partie de l'annulaire :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_auriculaire_g_12","Amputation d'une partie de l'auriculaire :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale"),selected = "Pas d'amputation",inline = T)
                    )
                    
                ),
                box(width=6,
                    title = "Examen de la main droite",
                    h4("Examen global de la main droite :"),
                    radioButtons("main_d_pince_pi_forme","Pince pouce-index en forme",  choices = c("Normale en forme","Altérée en forme","Impossible"),selected = "Normale en forme", inline = T),
                    radioButtons("main_d_pince_pi_force","Pince pouce-index en force",  choices = c("Normale en force","Altérée en force (4/5)","Très altérée en force (<3/5)"),selected = "Normale en force", inline = T),
                    radioButtons("main_d_pince_ppd_forme","Pince pollicidigitale en forme",  choices = c("Normale en forme","Altérée en forme","Impossible"),selected = "Normale en forme", inline = T),
                    radioButtons("main_d_pince_ppd_force","Pince pollicidigitale force",  choices = c("Normale en force","Altérée en force (4/5)","Très altérée en force (<3/5)"),selected = "Normale en force", inline = T),
                    textAreaInput("main_d_pince_ppd_obs","Pince pollicidigitale (précisions)  :",value = "",rows = 2),
                    h4("Examen fonctionnel de la main droite :"),
                    radioButtons("main_d_activate_examfunct","Examen fonctionnel de la main droite :", choices = c("Normal","Altéré"), selected = "Normal", inline = T),
                    conditionalPanel(
                      condition="input.main_d_activate_examfunct=='Altéré'",
                      radioButtons("main_fonct_d_item1","Pince unguéale (ramassage d'une allumette ou d'une épingle)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item2","Pince pulpo-pulpaire (plaquette de plastique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item3","Pince pulpo-latérale (plaquette de plastique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item4","Pince tripode (haut de la boîte cylindrique, manche d'outil, pinceau)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item5","Empaumement (boîte de conserves, manche, pinceau)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item6","Crochet (poignée)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE"),
                      radioButtons("main_fonct_d_item7","Prise sphérique (haut de la boîte cylindrique)", choices = c("NORMALE","INTERMEDIAIRE","NULLE"),inline = T, selected = "NORMALE")
                    ),
                    h4("Examen des articulations de la main droite :"),
                    radioButtons("main_d_activate_articulations","Examen des articulations de la main droite :",  choices = c("Normal","Altéré","Altéré avec un équivalent-amputation"), selected = "Normal",inline = T),
                    conditionalPanel(
                      condition="input.main_d_activate_articulations!='Normal'",
                      radioButtons("main_d_carpometacarp_1","Blocage de la colonne du pouce articulaire ou extra-articulaire :",
                                   choices = c("Pas de blocage","En position de fonction (anté-pulsion et opposition)","Luxation carpo-métacarpienne ancienne, non réduite, à l'exclusion du pouce",
                                               "En position défavorable (adduction, rétropulsion)", "Equivalent d'une amputation"),selected =  "Pas de blocage", inline = T),
                      radioButtons("main_d_metacarpophal_1","Articulation métacarpo-phalangienne du pouce :", choices=c("Pas de blocage","Blocage en semi-flexion ou en extension", "Blocage en flexion complète","Laxité articulaire par rupture ou luxation ancienne du pouce non réduite", "Equivalent d'une amputation"),selected =  "Pas de blocage", inline = T),
                      radioButtons("main_d_interphalangienne_1","Articulation inter-phalangienne du pouce:", choices=c("Normale","Blocage en flexion complète", "Blocage en semi-flexion ou en extension ou luxation ancienne non réduite","Diminuée en amplitude", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_d_index_1","Mobilité de l'index :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_d_medius_1","Mobilité du medius :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T),
                      radioButtons("main_d_annulaire_1","Mobilité de l'annulaire :",choices=c("Normale","Diminuée", "Equivalent d'une amputation"),selected =  "Normale", inline = T)
                      
                    ),
                    conditionalPanel(
                      condition="input.main_d_activate_articulations=='Altéré avec un équivalent-amputation'",
                      radioButtons("amputation_pouce_d_12","Amputation d'une partie du pouce :",choices = c("Pas d'amputation","Pouce entier avec le premier métacarpien","Les deux phalanges du pouce","Phalange unguéale"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_index_d_12","Amputation d'une partie de l'index :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale seule"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_medius_d_12","Amputation d'une partie du medius :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale seule"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_annulaire_d_12","Amputation d'une partie de l'annulaire :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale"),selected = "Pas d'amputation",inline = T),
                      radioButtons("amputation_auriculaire_d_12","Amputation d'une partie de l'auriculaire :",choices = c("Pas d'amputation","Trois phalanges (avec ou sans la tête du métacarpien)","Deux phalanges ou la phalange unguéale"),selected = "Pas d'amputation",inline = T)
                    )
                )
              )
              
      ),
      tabItem(tabName = "genoux",
              #TabItem Genoux ####
              h1("Examen des genoux"),
              radioButtons("genoux_activate", "Examen clinique des genoux réalisé :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(title = "Analyse de la mobilité", width = 12,
                  radioButtons("genoux_marche_1", "Marche spontanée", c("Sans boiterie","Boiterie sans aide","Avec 1 canne","Avec 2 cannes", "Fauteuil"),selected = "Sans boiterie",inline = T),
                  radioButtons("genoux_marche_2", "Marche sur les talons", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("genoux_marche_3", "Marche sur les pointes", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("genoux_accroupissement", "Accroupissement", c("Possible","Précautionneux","A mi-hauteur","Impossible","Non réalisé"),selected = "Possible",inline = T),
                  radioButtons("genoux_appuimono_D", "Appui monopodal à Droite", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  radioButtons("genoux_appuimono_G", "Appui monopodal à Gauche", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  radioButtons("genoux_sautmono_D", "Sautillement unipodal à Droite", c("Non réalisé","Possible","Impossible ou douloureux"),selected = "Non réalisé",inline = T),
                  radioButtons("genoux_sautmono_G", "Sautillement unipodal à Gauche", c("Non réalisé","Possible","Impossible ou douloureux"),selected = "Non réalisé",inline = T)
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Examen du genou gauche",
                    h4("Inspection du genou gauche"),
                    textAreaInput("genou_g_cic","Cicatrice :",value = "Aucune",rows = 2),
                    checkboxGroupInput("genou_g_tbcirculatoire", "Troubles circulatoires et vaso-moteurs :", choices = c("Aucun","Modification de la couleur de la peau", "Modification de la chaleur de la peau", "oedème"),selected = "Aucun", inline = T),
                    numericInput("genou_g_genuvarum","Genu varum :",value = 0, min=0, max=200),
                    numericInput("genou_g_genuvalgum","Genu valgum :",value = 0, min=0, max=200),
                    sliderInput("genou_g_raccourcissement","Raccourcissement du membre inférieur en cm",min = 0,max = 50,value = 0),
                    numericInput("genou_g_flessum","Flessum :",value = 0, min=0, max=200),
                    h4("Palpation du genou gauche :"),
                    radioButtons("genou_g_cal","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    checkboxGroupInput("genou_g_tiroir", "Tiroir :", choices = c("Aucun","Antérieur", "Posterieur"),selected = "Aucun", inline = T),
                    checkboxGroupInput("genou_g_grinding", "Grinding test :", choices = c("Négatifs","Positif en interne", "Positif en externe"),selected = "Négatifs", inline = T),
                    radioButtons("genou_g_chocrotulien","Choc rotulien :",choices = c("Non","Oui"),inline = T),
                    radioButtons("genou_g_rabot","Signe du rabot :",choices = c("Non","Oui"),inline = T),
                    h4("Mobilité du genou gauche :"),
                    radioButtons("genou_g_mobilite_activate","Mobilité articulaire du genou gauche :", choices = c("Normale","Blocage","Limitation"),inline = T),
                    conditionalPanel(
                      condition="input.genou_g_mobilite_activate=='Blocage'",
                      radioButtons("genou_g_blocage_angle","Angle de blocage",choices = c("Rectitude (position favorable)", "De 5° à 25°",
                                                                          "De 25° à 50°", "De 50° à 80°","Au-delà de 80°"),inline = T),
                      textAreaInput("genou_g_blocage_obs","Autres observations :",value = "",rows = 2)
                    ),
                    conditionalPanel(
                      condition="input.genou_g_mobilite_activate=='Limitation'",
                      radioButtons("genou_g_limitationext_angle","Déficit de l'extension",choices = c("Aucune", " déficitaire de 5° à 25°",
                                                                          "déficitaire de 25°", "déficitaire de 45°"),inline = T),
                      radioButtons("genou_g_limitationflex_angle","Déficit de la flexion",choices = c("Aucune", "ne peut s'effectuer au-delà de 110°",
                                                                                                      "ne peut se faire au-delà de 90°", "ne peut se faire au-delà de 45°"),inline = T),
                      textAreaInput("genou_g_limitation_obs","Autres observations :",value = "",rows = 2)
                      
                    ),
                    radioButtons("genou_g_rotule_activate","Examen de la rotule :", choices = c("Normal","Altéré"), inline = T),
                    conditionalPanel(
                      condition="input.genou_g_rotule_activate=='Altéré'",
                      checkboxGroupInput("genou_g_rotule_exam","", choices = c("Rotule anormalement mobile (par rupture d'ailerons rotuliens)",
                                                                               "Luxation récidivante","Patellectomie"),inline = T),
                      textAreaInput("genou_g_rotul_obs","Autres observations :",value = "",rows = 2)

                    )
                    ),
                box(width=6,
                    title = "Examen du genou droit",
                    h4("Inspection du genou droit"),
                    textAreaInput("genou_d_cic","Cicatrice :",value = "Aucune",rows = 2),
                    checkboxGroupInput("genou_d_tbcirculatoire", "Troubles circulatoires et vaso-moteurs :", choices = c("Aucun","Modification de la couleur de la peau", "Modification de la chaleur de la peau", "oedème"),selected = "Aucun", inline = T),
                    numericInput("genou_d_genuvarum","Genu varum :",value = 0, min=0, max=200),
                    numericInput("genou_d_genuvalgum","Genu valgum :",value = 0, min=0, max=200),
                    sliderInput("genou_d_raccourcissement","Raccourcissement du membre inférieur en cm",min = 0,max = 50,value = 0),
                    numericInput("genou_d_flessum","Flessum :",value = 0, min=0, max=200),
                    h4("Palpation du genou droit :"),
                    radioButtons("genou_d_cal","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    checkboxGroupInput("genou_d_tiroir", "Tiroir :", choices = c("Aucun","Antérieur", "Posterieur"),selected = "Aucun", inline = T),
                    checkboxGroupInput("genou_d_grinding", "Grinding test :", choices = c("Négatifs","Positif en interne", "Positif en externe"),selected = "Négatifs", inline = T),
                    radioButtons("genou_d_chocrotulien","Choc rotulien :",choices = c("Non","Oui"),inline = T),
                    radioButtons("genou_d_rabot","Signe du rabot :",choices = c("Non","Oui"),inline = T),
                    h4("Mobilité du genou droit :"),
                    radioButtons("genou_d_mobilite_activate","Mobilité articulaire du genou gauche :", choices = c("Normale","Blocage","Limitation"),inline = T),
                    conditionalPanel(
                      condition="input.genou_d_mobilite_activate=='Blocage'",
                      radioButtons("genou_d_blocage_angle","Angle de blocage",choices = c("Rectitude (position favorable)", "De 5° à 25°",
                                                                                          "De 25° à 50°", "De 50° à 80°","Au-delà de 80°"),inline = T),
                      textAreaInput("genou_d_blocage_obs","Autres observations :",value = "",rows = 2)
                    ),
                    conditionalPanel(
                      condition="input.genou_d_mobilite_activate=='Limitation'",
                      radioButtons("genou_d_limitationext_angle","Déficit de l'extension",choices = c("Aucune", " déficitaire de 5° à 25°",
                                                                                                      "déficitaire de 25°", "déficitaire de 45°"),inline = T),
                      radioButtons("genou_d_limitationflex_angle","Déficit de la flexion",choices = c("Aucune", "ne peut s'effectuer au-delà de 110°",
                                                                                                      "ne peut se faire au-delà de 90°", "ne peut se faire au-delà de 45°"),inline = T),
                      textAreaInput("genou_d_limitation_obs","Autres observations :",value = "",rows = 2)
                      
                    ),
                    radioButtons("genou_d_rotule_activate","Examen de la rotule :", choices = c("Normal","Altéré"), inline = T),
                    conditionalPanel(
                      condition="input.genou_d_rotule_activate=='Altéré'",
                      checkboxGroupInput("genou_d_rotule_exam","", choices = c("Rotule anormalement mobile (par rupture d'ailerons rotuliens)",
                                                                               "Luxation récidivante","Patellectomie"),inline = T),
                      textAreaInput("genou_d_rotul_obs","Autres observations :",value = "",rows = 2)
                      
                    )
                )
              )
              
      ),
      tabItem(tabName = "hanchesbassin",
              #TabItem Hanches et bassin ####
              h1("Examen du bassin et des hanches"),
              checkboxGroupInput("hanchesbassin_activate", "Examen clinique des genoux réalisé :", choices = c("Non","Bassin","Hanche droite","Hanche gauche"),inline = T,selected = "Non"),
              fluidRow(
                box(
                  title = "Analyse de la mobilité", width = 6,
                  radioButtons("hanches_marche_1", "Marche spontanée", c("Sans boiterie","Boiterie sans aide","Avec 1 canne","Avec 2 cannes", "Fauteuil"),selected = "Sans boiterie",inline = T),
                  radioButtons("hanches_marche_2", "Marche sur les talons", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("hanches_marche_3", "Marche sur les pointes", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("hanches_accroupissement", "Accroupissement", c("Possible","Précautionneux","A mi-hauteur","Impossible","Non réalisé"),selected = "Possible",inline = T),
                  radioButtons("hanches_appuimono_D", "Appui monopodal à Droite", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  radioButtons("hanches_appuimono_G", "Appui monopodal à Gauche", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T)
                ),
                box(
                  title="Aide à l'examen clinique :",
                  p("Le malade sera examiné couché sur le dos, le bassin fixé, genou fléchi, pour l'étude de la flexion, de l'abduction et de l'adduction."), 
                  p("Couché sur le ventre, genou fléchi à 90°, pour l'étude de l'extension et des rotations (la jambe, portée en dehors, provoque la rotation interne, portée en dedans, la rotation externe)"),
                  p("On recherchera les mouvements anormaux, la position du trochanter par rapport à la normale (la ligne bi-trochantérienne effleure le bord supérieur de la symphyse pubienne), l'amyotrophie des quadriceps ou celle des fessiers (effacement du pli fessier). L'accroupissement et la flexion en avant seront observés avec attention."),
                  HTML("<p>Mobilités normales de l'articulation coxo-fémorale :</p><ul><li>Extension : 0°</li><li>Flexion : 140° (variable selon l'adiposité du sujet)</li><li>Hyperextension : 15° à 30°</li><li>Abduction : 50°</li><li>Adduction : 15° à 30°</li><li>Rotation interne : 30°</li><li>Rotation externe : 60°</li></um>")
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Examen de la hanche gauche",
                    h4("Inpsection de la hanche gauche :"),
                    textAreaInput("hanche_g_cicatrice","Cicatrice :",value = "Aucune",rows = 2),
                    radioButtons("hanche_g_attitude", "Attitude vicieuse :",choices = c("Aucune", "En rotation interne","En rotation externe"), inline = T),
                    numericInput("hanche_g_raccourcissement", "Raccourcissement du membre inférieur :", value = 0, min=0, max=100),
                    radioButtons("hanche_g_cal","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    h4("Mobilités de la hanche gauche :"),
                    numericInput("hanche_g_flexion", "Flexion (N=140) :", value = -1, min=-1, max=200),
                    numericInput("hanche_g_extension", "Extension (N=0) :", value = -1, min=-1, max=100),
                    numericInput("hanche_g_hyperextension", "Hyperxtension (N=15 à 30) :", value = -1, min=-1, max=100),
                    numericInput("hanche_g_rot_int", "Rotation interne (N=30) :", value = -1, min=-1, max=100),
                    numericInput("hanche_g_rot_ext", "Rotation externe (N=60) :", value = -1, min=-1, max=100),
                    numericInput("hanche_g_abduction", "Abduction (N=50) :", value = -1, min=-1, max=100),
                    numericInput("hanche_g_adduction", "Adduction (N =15 à 30) :", value = -1, min=-1, max=100),
                    h4("Mensurations du membre inférieur gauche"),
                    numericInput("hanche_mens_g1", "Cuisse (mesuré à 15 cm du sommet rotulien)", value=-1 ,min = 0, max =200 ),
                    numericInput("hanche_mens_g2", "Mollet (mesuré à 10 cm de la pointe de la rotule)", value=-1 ,min = -1, max =70 ),
                    textAreaInput("hanche_g_obs","Autres observations :",value = "",rows = 5)
                ),
                box(width=6,
                    title = "Examen de la hanche droit",
                    h4("Inpsection de la hanche droite :"),
                    textAreaInput("hanche_d_cicatrice","Cicatrice :",value = "Aucune",rows = 2),
                    radioButtons("hanche_d_attitude", "Attitude vicieuse :",choices = c("Aucune", "En rotation interne","En rotation externe"), inline = T),
                    numericInput("hanche_d_raccourcissement", "Raccourcissement du membre inférieur :", value = 0, min=0, max=100),
                    radioButtons("hanche_d_cal","Cal hypertrophique",c("Non", "douloureux à la palpation","indolore à la palpation"),inline = T),
                    h4("Mobilités de la hanche droite :"),
                    numericInput("hanche_d_flexion", "Flexion (N=140) :", value = -1, min=-1, max=200),
                    numericInput("hanche_d_extension", "Extension (N=0) :", value = -1, min=-1, max=100),
                    numericInput("hanche_d_hyperextension", "Hyperxtension (N=15 à 30) :", value = -1, min=-1, max=100),
                    numericInput("hanche_d_rot_int", "Rotation interne (N=30) :", value = -1, min=-1, max=100),
                    numericInput("hanche_d_rot_ext", "Rotation externe (N=60) :", value = -1, min=-1, max=100),
                    numericInput("hanche_d_abduction", "Abduction (N=50) :", value = -1, min=-1, max=100),
                    numericInput("hanche_d_adduction", "Adduction (N =15 à 30) :", value = -1, min=-1, max=100),
                    h4("Mensurations du membre inférieur droite"),
                    numericInput("hanche_mens_d1", "Cuisse (mesuré à 15 cm du sommet rotulien)", value=-1 ,min = 0, max =200 ),
                    numericInput("hanche_mens_d2", "Mollet (mesuré à 10 cm de la pointe de la rotule)", value=-1 ,min = -1, max =70 ),
                    textAreaInput("hanche_d_obs","Autres observations :",value = "",rows = 5)
                )
              ),
              fluidRow(
                box(width = 12,
                    title="Examen du bassin",
                    radioButtons("bassin_activate","Examen du bassin :",choices = c("Normal","Altéré"),selected = "Normal",inline = T),
                    conditionalPanel(
                      condition="input.bassin_activate=='Altéré'",
                    radioButtons("bassin_disjoncion_symphyse","Disjonction de la symphyse pubienne :",choices = c("Non","Oui"),selected = "Non",inline = T),
                    h4("Atteinte des sacro-iliaques"),
                    radioButtons("bassin_diastasis_sacroi","Diastasis (entraînant une mobilité anormale du sacrum, avec retentissement sur la marche, accroupissement impossible, sacralgies) :",choices = c("Non","Oui"),selected = "Non",inline = T),
                    radioButtons("bassin_arthro_sacroi","Arthropathie sacro-iliaque douloureuse chronique d'origine traumatique :",choices = c("Non","Oui"),selected = "Non",inline = T),
                    h4("Atteinte du sacrum et du coccyx"),
                    radioButtons("bassin_fx_sacrum","Fracture du sacrum : Gêne aux mouvements du tronc, des douleurs à la station assise, une gêne plus ou moins importante à l'usage de la bicyclette",
                                 choices = c("Aucune gêne ou minime","Gêne dans certains moments de la semaine ou du mois","Gêne quotidienne"),selected = "Aucune gêne ou minime", inline = T),
                    radioButtons("bassin_coccygodynie","Coccygodynie : avec tiraillements à l'accroupissement, douleurs en position assise",
                                 choices = c("Aucune gêne ou minime","Gêne dans certains moments de la semaine ou du mois","Gêne quotidienne"),selected = "Aucune gêne ou minime", inline = T)
                    )
                    )
              )
              
      ),
      tabItem(tabName = "cheville_pieds",
              #TabItem Chevilles et pieds ####
              h1("Examen des chevilles et des pieds"),
              radioButtons("cheville_pieds_activate", "Examen clinique des chevilles et pieds :", choices = c("Non","Oui les deux","Oui à droite","Oui à gauche"),inline = T),
              fluidRow(
                box(
                  title = "Analyse de la mobilité", width = 6,
                  radioButtons("cheville_marche_1", "Marche spontanée", c("Sans boiterie","Boiterie sans aide","Avec 1 canne","Avec 2 cannes", "Fauteuil"),selected = "Sans boiterie",inline = T),
                  radioButtons("cheville_marche_2", "Marche sur les talons", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  radioButtons("cheville_marche_3", "Marche sur les pointes", c("Possible","Précautionneuse","Impossible","Non réalisée"),selected = "Possible",inline = T),
                  textAreaInput("cheville_attitude_obs", "Attitude globale :", "", rows = 1),
                  sliderInput("cheville_g_raccourcissement","Raccourcissement du membre inférieur gauche en cm",min = 0,max = 50,value = 0)
                ),
                box(
                  title = "", width = 6,
                  radioButtons("cheville_accroupissement", "Accroupissement", c("Possible","Précautionneux","A mi-hauteur","Impossible","Non réalisé"),selected = "Possible",inline = T),
                  radioButtons("cheville_appuimono_D", "Appui monopodal à Droite", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  radioButtons("cheville_appuimono_G", "Appui monopodal à Gauche", c("Tenu 10s","Tenu 5s","Impossible","Non réalisé"),selected = "Tenu 10s",inline = T),
                  textAreaInput("cheville_mobilite_obs", "Autres observations :", "", rows = 1),
                  sliderInput("cheville_d_raccourcissement","Raccourcissement du membre inférieur droit en cm",min = 0,max = 50,value = 0)
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Examen de la cheville gauche",
                    h4("Mensurations à gauche en cm"),
                    numericInput("mens_g0_cheville", "Cuisse (mesuré à 15 cm du sommet rotulien)", value=-1 ,min = -1, max =200 ),
                    numericInput("mens_g1_cheville", "Genou (au centre rotulien)", value=-1 ,min = -1, max =100 ),
                    numericInput("mens_g2_cheville", "Mollet (à 10 cm de la pointe de la rotule)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g3_cheville", "Malléolaire", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_g4_cheville", "Etrier", value=-1 ,min = -1, max =70 ),
                    h4("Inspection et palpation"),
                    radioButtons("cheville_g_tbvaso", "Troubles vasomoteurs :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cheville_g_empatsous", "Empâtement sous-malléolaire :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cheville_g_empatretro", "Empâtement rétro-malléolaire :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    textAreaInput("cheville_g_cicatrices_obs", "Cicatrices à gauche :", "Aucunes", rows = 1),
                    h4("Mobilisations"),
                    radioButtons("activate_tibiotarsienne_g","Articulation tibio-tarsienne :", choices = c("Normale","Limitation","Blocage"),inline = T),
                    h5("Cette articulation est principalement responsable des flexions plantaire et dorsale"),
                    conditionalPanel(
                      condition="input.activate_tibiotarsienne_g=='Blocage'",
                      h5("L'angle de mobilité d'un blocage corresponds à un angle inférieur à 15° de part et d'autre de l'angle droit, sinon il s'agit d'une limitation"),
                      radioButtons("cheville_g_tibiotarsienne_blocage","Blocage de la cheville :",c("Avec pied en bonne position (angle droit)", "Avec pied en talus","Avec pied en équin prononcé"), inline=T),
                      radioButtons("cheville_g_tibiotarsienne_blocage2","Déviation de la cheville :",c("Pas de déviation", "Déviation en varus","Déviation en valgus"), inline=T)
                    ),
                    conditionalPanel(
                      condition="input.activate_tibiotarsienne_g=='Limitation'",
                      h5("L'angle de mobilité d'une limitation corresponds à 15° de part et d'autre de l'angle droit, sinon il s'agit d'un blocage"),
                      numericInput("cheville_g_flexionp", "Flexion plantaire (N=40)", value = 40, min=0, max=50),
                      numericInput("cheville_g_flexiond", "Flexion dorsale (N=25)", value = 25, min=0, max=35),
                      radioButtons("cheville_g_tibiotarsienne_limitation","Limitation de la cheville :",c("Angles supérieurs à 15°", "Angle <15° de part et d'autre de l'angle droit","Diastasis tibio-péronier important, en lui-même"), inline=T),
                      radioButtons("cheville_g_tibiotarsienne_limitation2","Déviation de la cheville :",c("Pas de déviation", "Déviation en varus","Déviation en valgus"), inline=T)
                      ),
                    radioButtons("activate_tarsometatarsiennes_g","Articulations sous-astragaliennes et tarso-métatarsiennes :", choices = c("Normale","Limitation","Blocage"),inline = T),
                    h5("Elles sont responsables de l'abduction (latéralité externe jusqu'à 20°), et de l'adduction (latéralité interne, jusqu'à 30°), de la pronation (plante du pied regardant en dehors), et de la supination (plante du pied regardant en dedans)."),
                    conditionalPanel(
                      condition="input.activate_tarsometatarsiennes_g!='Normale'",
                      numericInput("cheville_g_tarsometatarsiennes_adduction", "Adduction (N=30)", value = 30, min=0, max=40),
                      numericInput("cheville_g_tarsometatarsiennes_adduction", "Abduction (N=20)", value = 20, min=0, max=30)
                    )
                    
                ),
                box(width=6,
                    title = "Examen de la cheville droite",
                    h4("Mensurations à droite en cm"),
                    numericInput("mens_d0_cheville", "Cuisse (mesuré à 15 cm du sommet rotulien)", value=-1 ,min = -1, max =200 ),
                    numericInput("mens_d1_cheville", "Genou (au centre rotulien)", value=-1 ,min = -1, max =100 ),
                    numericInput("mens_d2_cheville", "Mollet (à 10 cm de la pointe de la rotule)", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d3_cheville", "Malléolaire", value=-1 ,min = -1, max =70 ),
                    numericInput("mens_d4_cheville", "Etrier", value=-1 ,min = -1, max =70 ),
                    h4("Inspection et palpation"),
                    radioButtons("cheville_d_tbvaso", "Troubles vasomoteurs :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cheville_d_empatsous", "Empâtement sous-malléolaire :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    radioButtons("cheville_d_empatretro", "Empâtement rétro-malléolaire :", choices = c("Non","Oui modérée", "Oui majeure"), inline = T),
                    textAreaInput("cheville_d_cicatrices_obs", "Cicatrices à droite :", "Aucunes", rows = 1),
                    h4("Mobilisations"),
                    radioButtons("activate_tibiotarsienne_d","Articulation tibio-tarsienne :", choices = c("Normale","Limitation","Blocage"),inline = T),
                    h5("Cette articulation est principalement responsable des flexions plantaire et dorsale"),
                    conditionalPanel(
                      condition="input.activate_tibiotarsienne_d=='Blocage'",
                      h5("L'angle de mobilité d'un blocage corresponds à un angle inférieur à 15° de part et d'autre de l'angle droit, sinon il s'agit d'une limitation"),
                      radioButtons("cheville_d_tibiotarsienne_blocage","Blocage de la cheville :",c("Avec pied en bonne position (angle droit)", "Avec pied en talus","Avec pied en équin prononcé"), inline=T),
                      radioButtons("cheville_d_tibiotarsienne_blocage2","Déviation de la cheville :",c("Pas de déviation", "Déviation en varus","Déviation en valgus"), inline=T)
                    ),
                    conditionalPanel(
                      condition="input.activate_tibiotarsienne_d=='Limitation'",
                      h5("L'angle de mobilité d'une limitation corresponds à 15° de part et d'autre de l'angle droit, sinon il s'agit d'un blocage"),
                      numericInput("cheville_d_flexionp", "Flexion plantaire (N=40)", value = 40, min=0, max=50),
                      numericInput("cheville_d_flexiond", "Flexion dorsale (N=25)", value = 25, min=0, max=35),
                      radioButtons("cheville_d_tibiotarsienne_limitation","Limitation de la cheville :",c("Angles supérieurs à 15°", "Angle <15° de part et d'autre de l'angle droit","Diastasis tibio-péronier important, en lui-même"), inline=T),
                      radioButtons("cheville_d_tibiotarsienne_limitation2","Déviation de la cheville :",c("Pas de déviation", "Déviation en varus","Déviation en valgus"), inline=T)
                    ),
                    radioButtons("activate_tarsometatarsiennes_d","Articulations sous-astragaliennes et tarso-métatarsiennes :", choices = c("Normale","Limitation","Blocage"),inline = T),
                    h5("Elles sont responsables de l'abduction (latéralité externe jusqu'à 20°), et de l'adduction (latéralité interne, jusqu'à 30°), de la pronation (plante du pied regardant en dehors), et de la supination (plante du pied regardant en dedans)."),
                    conditionalPanel(
                      condition="input.activate_tarsometatarsiennes_d!='Normale'",
                      numericInput("cheville_d_tarsometatarsiennes_adduction", "Adduction (N=30)", value = 30, min=0, max=40),
                      numericInput("cheville_d_tarsometatarsiennes_adduction", "Abduction (N=20)", value = 20, min=0, max=30)
                    )
                )
              ),
              fluidRow(
                box(width=6,
                    title = "Examen du pied gauche",
                    h4("Aspect général du pied gauche :"),
                    radioButtons("pied_g_calvicieux","Cal vicieux, exubérant",c("Non","Oui avec répercussion sur la marche minime","Oui avec répercussion sur la marche modéré","Oui avec répercussion sur la marche important"), inline=T),
                    radioButtons("pied_g_exostose","Exostose sous-calcanéenne", c("Non","Oui"), inline = T),
                    radioButtons("pied_g_piedcreux","Pied creux post-traumatique", c("Non","Oui modéré","Oui sévère"), inline = T),
                    radioButtons("pied_g_vouteplantaire","Affaissement de la voûte plantaire", c("Non","Oui minime","Oui modéré","Oui sévère"), inline = T),
                    h4("Examen du gros orteil"),
                    radioButtons("activate_grosorteil_g","Articulations du gros orteils intactes",c("Oui","Non"), inline = T),
                    conditionalPanel(
                      condition="input.activate_grosorteil_g=='Non'",
                      h5("Articulations métatarso-phalangiennes du gros orteil : Elles permettent aux orteils un angle flexion-extension de 90° environ. La plus importante est la première, étant donnée l'importance du gros orteil dans la fonction d'appui dans la locomotion."),
                      radioButtons("pied_g_grosorteil1","Blocage du gros orteil",c("Non","En rectitude (bonne position)", "En mauvaise position"), inline = T),
                      radioButtons("pied_g_grosorteil2","Limitation de la mobilité du gros orteil",c("Non","Oui minime","Oui modérée"), inline = T),
                      h5("Articulations interphalangiennes : Seule a une importance, dans la fonction de locomotion, l'interphalangienne du gros orteil"),
                      radioButtons("pied_g_grosorteil3","Interphalangienne du gros orteil",c("Non", "Limitation de ses mouvements","Blocage de l'interphalangienne"), inline = T)
                    ),
                    radioButtons("activate_autresorteil_g","Articulations du autres orteils intactes",c("Oui","Non"), inline = T),
                    conditionalPanel(
                      condition="input.activate_autresorteil_g=='Non'",
                      radioButtons("pied_g_autresorteilblocage","Existe-t-il un ou plusieurs autres orteil bloqué ?", c("Non","Oui en rectitude","Oui en mauvaise position"), inline=T),
                      conditionalPanel(
                        condition="input.pied_g_autresorteilblocage!='Non'",
                        textAreaInput("pied_g_autresorteilblocage_obs", "Précisez lesquels :", "", rows = 1)
                      ),
                      radioButtons("pied_g_autresorteillimitation","Existe-t-il un ou plusieurs autres orteil limités en mouvement ?", c("Non","Oui"), inline=T),
                      conditionalPanel(
                        condition="input.pied_g_autresorteillimitation=='Oui'",
                        textAreaInput("pied_g_autresorteillimitation_obs", "Précisez lesquels :", "", rows = 1)
                      )
                      
                    )
                    
                    
                ),
                box(width=6,
                    title = "Examen du pied droit",
                    
                    h4("Aspect général du pied droit :"),
                    radioButtons("pied_d_calvicieux","Cal vicieux, exubérant",c("Non","Oui avec répercussion sur la marche minime","Oui avec répercussion sur la marche modéré","Oui avec répercussion sur la marche important"), inline=T),
                    radioButtons("pied_d_exostose","Exostose sous-calcanéenne", c("Non","Oui"), inline = T),
                    radioButtons("pied_d_piedcreux","Pied creux post-traumatique", c("Non","Oui modéré","Oui sévère"), inline = T),
                    radioButtons("pied_d_vouteplantaire","Affaissement de la voûte plantaire", c("Non","Oui minime","Oui modéré","Oui sévère"), inline = T),
                    h4("Examen du gros orteil"),
                    radioButtons("activate_grosorteil_d","Articulations du gros orteils intactes",c("Oui","Non"), inline = T),
                    conditionalPanel(
                      condition="input.activate_grosorteil_d=='Non'",
                      h5("Articulations métatarso-phalangiennes du gros orteil : Elles permettent aux orteils un angle flexion-extension de 90° environ. La plus importante est la première, étant donnée l'importance du gros orteil dans la fonction d'appui dans la locomotion."),
                      radioButtons("pied_d_grosorteil1","Blocage du gros orteil",c("Non","En rectitude (bonne position)", "En mauvaise position"), inline = T),
                      radioButtons("pied_d_grosorteil2","Limitation de la mobilité du gros orteil",c("Non","Oui minime","Oui modérée"), inline = T),
                      h5("Articulations interphalangiennes : Seule a une importance, dans la fonction de locomotion, l'interphalangienne du gros orteil"),
                      radioButtons("pied_d_grosorteil3","Interphalangienne du gros orteil",c("Non", "Limitation de ses mouvements","Blocage de l'interphalangienne"), inline = T)
                    ),
                    radioButtons("activate_autresorteil_d","Articulations du autres orteils intactes",c("Oui","Non"), inline = T),
                    conditionalPanel(
                      condition="input.activate_autresorteil_d=='Non'",
                      radioButtons("pied_d_autresorteilblocage","Existe-t-il un ou plusieurs autres orteil bloqué ?", c("Non","Oui en rectitude","Oui en mauvaise position"), inline=T),
                      conditionalPanel(
                        condition="input.pied_d_autresorteilblocage!='Non'",
                        textAreaInput("pied_d_autresorteilblocage_obs", "Précisez lesquels :", "", rows = 1)
                      ),
                      radioButtons("pied_d_autresorteillimitation","Existe-t-il un ou plusieurs autres orteil limités en mouvement ?", c("Non","Oui"), inline=T),
                      conditionalPanel(
                        condition="input.pied_d_autresorteillimitation=='Oui'",
                        textAreaInput("pied_d_autresorteillimitation_obs", "Précisez lesquels :", "", rows = 1)
                      )
                      
                    )
                )
              )
      ),
      tabItem(tabName = "depression",
              #TabItem Depression ####
              h1("Examen psychiatrique (dépression uniquement)"),
              radioButtons("depression_activate", "Examen psychiatrique de l'état dépressif réalisé ?", choices = c("Non","Oui"),inline = T),
              
              fluidRow(
                box(title="Eléments objectifs de l'examen :", width = 6,
                    radioButtons("psy_obj_item1","Existe-t-il un suivi spécialisé ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_obj_item2","Existe-t-il un traitement de fond ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"), 
                    radioButtons("psy_obj_item3","Existe-t-il un ralentissement psychomoteur ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_obj_item4","Existe-t-il une incurie ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_obj_item5","Existe-t-il des signes de mutilation ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non")
              ),
              box(title="Eléments de l'interrogatoire :", width = 6,
                  textAreaInput("psy_obs", "Notes libre de l'examen clinique :", "", rows = 5),
                  radioButtons("psy_mod_1","Existe-t-il des troubles du comportement ?", choices = c("Non ou minimes","Oui modérés","Oui sévères","Oui très sévères"),inline = T, selected = "Non ou minimes"),
                  radioButtons("psy_mod_2","Existe-t-il une grande dépression mélancolique ?", choices = c("Non","Oui"),inline = T, selected = "Non"),
                  radioButtons("psy_mod_3","Existe-t-il des troubles anxieux ?", choices = c("Non ou minimes","Oui avec évitement ciblés","Oui avec pantophobie"),inline = T, selected = "Non ou minimes")
              )
              ),
              fluidRow(
                box(title="Facteurs de gravités :", width = 6,
                    radioButtons("psy_grav_item1","Isolement social", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_grav_item2","Contexte de deuil", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_grav_item3","Antécédents de tentatives de suicide", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_grav_item4","Existence d’une planification de suicide", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_grav_item5","Maladie chronique douloureuse ou handicap", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
                    radioButtons("psy_grav_item6","Utilisation de substances toxiques (alcool, drogue)", choices = c("Non","Oui","NSP"),inline = T, selected = "Non")
                ),
                box(title="Evaluation de la sévérité de la dépression :", width = 6,
                    h4("PHQ questionnaire"), p("Kroenke K, Spitzer RL, Williams JB. The PHQ-9: validity of a brief depression severity measure. J Gen Intern Med 2001;16(9):606-13 (41)"),
                    radioButtons("PHQ9_item0","Question PHQ réalisé ?", choices = c("Non","Oui"),inline = T, selected = "Non"),
                    conditionalPanel(
                      condition="input.PHQ9_item0=='Oui'",
                    radioButtons("PHQ9_item1","1. Une diminution marquée d’intérêt ou de plaisir dans vos activités ?", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item2","2. Un sentiment d’abattement, de dépression ou de perte d’espoir ?", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item3","3. Difficultés à vous endormir, à rester endormi(e), ou au contraire une tendance à trop dormir ?", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item4","4. Une sensation de fatigue ou de manque d’énergie ?", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item5","5. Un manque ou un excès d’appétit ?", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item6","6. Une mauvaise opinion de vous-même : l’impression que vous êtes un(e) raté(e) ou que vous vous êtes laissé(e) aller ou que vous avez négligé votre famille ? ", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item7","7. De la peine à vous concentrer, par exemple pour lire le journal ou regarder la télévision. ", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item8","8. L’impression de parler ou de vous déplacer si lentement que cela se remarquait – ou, au contraire, une fébrilité ou agitation telle que vous ne teniez pas en place ? ", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" ),
                    radioButtons("PHQ9_item9","9. Penser vous que vous préféreriez être mort ou penser à vous faire du mal ? ", choices = c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours"),inline = T, selected ="Jamais" )
                    )
                    )
              )
              
      ),
      tabItem(tabName = "invalidite",
              # TabItem Invalidite ####
      h1("Evaluation de la capacité de travail"),
      radioButtons("invalidite_activate", "Examen de la capacité de gain réalisé :", choices = c("Non","Oui"),inline = T),
      radioButtons("invalidite_appreciation_MC", "Au vue de l'examen des capacités restantes, vous estimez que l'assuré relève de :",c("Aptitude à un travail quelconque","Invalidité catégorie 1 (UN)","Invalidité catégorie 2 (DEUX)"), inline=T),
      fluidRow(
      box(title="Capacités actuelles :", width = 6,
          sliderInput("inval_AMI_item1","Regarder", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item1_com1"),
          sliderInput("inval_AMI_item2","Ecouter", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item2_com1"),
          sliderInput("inval_AMI_item3","Acquérir des compétences", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item3_com1"),
          sliderInput("inval_AMI_item4","Fixer son attention", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item4_com1"),
          sliderInput("inval_AMI_item5","Exécuter une tâche unique", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item5_com1"),
          sliderInput("inval_AMI_item6","Exécuter des tâches multiples", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item6_com1"),
          sliderInput("inval_AMI_item7","Gérer le stress et autres exigences psychologiques", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item7_com1"),
          sliderInput("inval_AMI_item8","Comprendre et s'exprimer par la parole et l'écrit", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item8_com1"),
          sliderInput("inval_AMI_item9","Changer la position corporelle de base", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item9_com1"),
          sliderInput("inval_AMI_item10","Garder la position du corps", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item10_com1"),
          sliderInput("inval_AMI_item11","Soulever et porter des objets", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item11_com1"),
          sliderInput("inval_AMI_item12","Activités de motricité fine", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item12_com1"),
          sliderInput("inval_AMI_item13","Utilisation des mains et des bras", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item13_com1"),
          sliderInput("inval_AMI_item14","Marcher", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item14_com1"),
          sliderInput("inval_AMI_item15","Déplacements à l'extérieur du domicile", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item15_com1"),
          sliderInput("inval_AMI_item16","Relations et interactions avec autrui", min = 0,max = 4, value=0),
          htmlOutput("inval_AMI_item16_com1")
      ),
      box(title = "Questionnaire ciblé :", width = 6,
          radioButtons("inval_travail_item1","L'assuré exerce t-il une activité à temps partiel ?",choices = c("Non","Oui"), inline = T),
          radioButtons("inval_travail_item2","L'assuré a-t-il la capacité de maintenir une activité à temps partiel (>=2j par semaine) ?",choices = c("Non","Oui"), inline = T),
          radioButtons("inval_travail_item3","L'assuré veut-il de maintenir une activité à temps partiel (>=2j par semaine) ?",choices = c("Non","Oui"), inline = T),
          textAreaInput("inval_travail_obs","Autres observations :",value = "",rows = 2),
          radioButtons("inval_activate_cancer","Existe-t-il un cancer non guéri ?",choices = c("Non","Oui"), inline = T),
          conditionalPanel(
            condition="input.inval_activate_cancer=='Oui'",
            radioButtons("inval_cancer_grav_item1","Existe-t-il un traitement en cours ? (chimiothérapie, radiothérapie)", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_cancer_grav_item2","Existe-t-il une chirurgie récente ? (<2 ans)", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_cancer_grav_item3","Existe-t-il un traitement palliatif ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_cancer_grav_item4","Existe-t-il des métastases ou une masse non résécable ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_cancer_grav_item5","Existe-t-il une surveillance active ? (Examens tous les 6 mois)", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            textAreaInput("inval_cancer_grav_obs","Autres observations :",value = "",rows = 2)
          ),
          radioButtons("inval_activate_psy","Existe-t-il des troubles psychiatriques non contrôlés ?",choices = c("Non","Oui"), inline = T),
          conditionalPanel(
            condition="input.inval_activate_psy=='Oui'",
            radioButtons("inval_psy_grav_item1","Existe-t-il un suivi psychiatrique mensuel ou inframensuel ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_psy_grav_item2","Existe-t-il une hospitalisation en cours (HDJ incluse) ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_psy_grav_item3","Existe-t-il une hospitalisation récente en psychiatrie (< 2 ans) ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_psy_grav_item4","L'assuré présente-t-il un danger quotidien ou hebdomadaire pour lui même ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_psy_grav_item5","L'assuré est-il dépendant d'une ou de plusieurs personnes ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            textAreaInput("inval_psy_grav_obs","Autres observations :",value = "",rows = 2)
          ),
          radioButtons("inval_activate_tms","Existe-t-il des troubles musculosquelettiques faisant obstacle à la reprise du travail ?",choices = c("Non","Oui"), inline = T),
          conditionalPanel(
            condition="input.inval_activate_tms=='Oui'",
            radioButtons("inval_tms_grav_item1","L'assuré est-il empécher d'effectuer des gestes quotidiens essentiels ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_tms_grav_item2","L'assuré peut-il effectuer des tâches non manuelles ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_tms_grav_item3","L'assuré est-il capable de lire et d'écrire ? (ou d'apprendre dans l'année)", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_tms_grav_item4","La poursuite d'une activité manuelle présent-elle une menace pour sa santé ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            textAreaInput("inval_tms_grav_obs","Autres observations :",value = "",rows = 2)
          ),
          radioButtons("inval_activate_degeneratif","Existe-t-il une maladie chronique dégénérative menacant l'autonomie actuelle ou future ?",choices = c("Non","Oui"), inline = T),
          conditionalPanel(
            condition="input.inval_activate_degeneratif=='Oui'",
            radioButtons("inval_degen_grav_item1","Existe-t-il un traitement curatif ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_degen_grav_item2","Existe-t-il un traitement pallatif en cours ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_degen_grav_item3","L'autonomie de l'assuré est-elle menacée à court terme (<2 ans)?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            radioButtons("inval_degen_grav_item4","L'assuré est-il dépendant d'une ou de plusieurs personnes ?", choices = c("Non","Oui","NSP"),inline = T, selected = "Non"),
            textAreaInput("inval_degen_grav_obs","Autres observations :",value = "",rows = 2)
          )
          
          )
      )
              ),
      tabItem(tabName = "decisionmaking",
              #TabItem decision making ####
              h1("Module d'aide à la décision"),
              tags$section("Avertissement : Ce module n'a pas fait l'objet d'une validation institutionnelle, vous restez responsable de votre décision."),
              actionButton("btn", "Analyse de mes données"),
              htmlOutput("decisionmaking_inval1"),
              conditionalPanel(
                condition = "(input.cadreprestation=='Accident du travail' || input.cadreprestation=='Maladie professionnelle') && input.objectif_convoc.includes('séquelles')",
                h3("ARGUMENTS POUR LA FIXATION DU TAUX D'IP"),
                div(DT::dataTableOutput("AT_bareme"),style = "font-size: 75%; width: 50%" ) 
                )
              
      ),
      tabItem(tabName = "synthesis",
              #TabItem Synthese clinique ####
              h1("Module de synthèse clinique"),
              tags$section("Avertissement : Ce module présente la synthèse de votre examen, vérifiez bien les informations afin de les entrer dans nos outils institutionnel. Cela vous engage."),
              fluidRow(
                box(title = "Synthèse clinique :",width = 12,
                    htmlOutput("intermediaire_synthese"),
                    htmlOutput("epaule"),
                    htmlOutput("synthese_coude"),
                    htmlOutput("synthese_poignet"),
                    htmlOutput("mains_synthese"),
                    htmlOutput("synthese_canalcarpiens"),
                    htmlOutput("cervical_synthese"),
                    htmlOutput("rachisdl_synthese"),
                    htmlOutput("hanchesbasssin_synthese"),
                    htmlOutput("genoux_synthese"),
                    htmlOutput("piedschevilles_synthese"),
                    htmlOutput("amputation_synthese"),
                    htmlOutput("depression_synthese"),
                    htmlOutput("inval_synthese")
                )
              )
              
      ),
      tabItem(tabName = "amputation",
              #TabItem Amputation ####
              h1("Module Amputation"),
              h2("Amputation du membre supérieur"),
              radioButtons("activate_amputation","Existe-t-il tout ou partie d'un membre amputé dans la liste ci-dessous ?", choices = c("Non","Oui"),inline = T, selected = "Non"),
              p("Les taux indiqués le sont sans tenir compte des possibilités d'appareillage ou de correction chirurgicale à visée fonctionnelle."),
              p("Lorsque cet appareillage ou cette intervention aboutit à un résultat excellent, l'expert peut tenir compte du gain de capacité ainsi obtenu, mais ne pourra appliquer une réduction du taux supérieur à 5 %"),
              fluidRow(
                box(title = "Membre supérieur Gauche", width = 6,
                    h3("Epaule"),
                    radioButtons("amputation_epaule_g_1","Amputation interscapulothoracique avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os",c("Non","Oui"),inline = T),
                    radioButtons("amputation_epaule_g_2","Désarticulation de l'épaule",c("Non","Oui"),inline = T),
                    h3("Bras"),
                    radioButtons("amputation_bras_g_1","Amputation du bras au tiers supérieur",c("Non","Oui"),inline = T),
                    radioButtons("amputation_bras_g_2","Amputation du bras au tiers moyen ou inférieur",c("Non","Oui"),inline = T),
                    radioButtons("amputation_bras_g_3","Désarticulation du coude, avant-bras au tiers supérieur",c("Non","Oui"),inline = T),
                    h3("Mains et doigts"),
                    radioButtons("amputation_main_g_1","Amputation métacarpienne conservant une palette",c("Non","Oui"),inline = T),
                    h5("S'il y a une amputation phalangienne, se référer au module mains et poignets car l'ensemble de la main est à examiner")
                    ),
                box(title = "Membre supérieur Droit", width = 6,
                    h3("Epaule"),
                    radioButtons("amputation_epaule_d_1","Amputation interscapulothoracique avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os",c("Non","Oui"),inline = T),
                    radioButtons("amputation_epaule_d_2","Désarticulation de l'épaule",c("Non","Oui"),inline = T),
                    h3("Bras"),
                    radioButtons("amputation_bras_d_1","Amputation du bras au tiers supérieur",c("Non","Oui"),inline = T),
                    radioButtons("amputation_bras_d_2","Amputation du bras au tiers moyen ou inférieur",c("Non","Oui"),inline = T),
                    radioButtons("amputation_bras_d_3","Désarticulation du coude, avant-bras au tiers supérieur",c("Non","Oui"),inline = T),
                    h3("Mains et doigts"),
                    radioButtons("amputation_main_d_1","Amputation métacarpienne conservant une palette",c("Non","Oui"),inline = T),
                    h5("S'il y a une amputation phalangienne, se référer au module mains et poignets car l'ensemble de la main est à examiner")
                    
                )
              ),
              h2("Amputation du membre inférieur"),
              p("Dans le calcul des incapacités permanentes, les deux membres inférieurs sont considérés comme ayant une valeur fonctionnelle égale."),
              p("Les taux indiqués le sont sans tenir compte des possibilités d'appareillage ou de correction chirurgicale à visée fonctionnelle."),
              p("Lorsqu'un appareil ou une intervention aboutit à un résultat excellent, l'expert peut tenir compte du gain obtenu mais ne pourra appliquer une réduction du taux supérieur à 15 %"),
              p("Perte de fonction des deux membres inférieurs, quelle que soit la cause 100"),
              fluidRow(
                box(title = "Membre inférieur Gauche", width = 6,
                    radioButtons("amputation_jambe_g_1","Amputation inter-ilio-abdominale",c("Non","Oui"),inline = T),
                    h3("Cuisse"),
                    radioButtons("amputation_jambe_g_2","Désarticulation de la hanche",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_3","Amputation inter-trochantérienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_4","Amputation sous-trochantérienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_5","Amputation au tiers moyen ou au tiers inférieur",c("Non","Oui"),inline = T),
                    h3("Genou et jambe"),
                    radioButtons("amputation_jambe_g_6","Désarticulation du genou",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_7","Amputation au tiers supérieur de la jambe",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_8","Amputation au tiers moyen ou inférieur",c("Non","Oui"),inline = T),
                    h3("Cheville et pied"),
                    radioButtons("amputation_jambe_g_9","Désarticulation tibio-tarsienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_10","Amputation du pied, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier (avec mouvement du pied restant satisfaisant et sans bascule en varus)",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_11","Désarticulation médio-tarsienne de Chopart",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_12","Amputation transmétatarsienne de l'avant-pied",c("Non","Oui"),inline = T),
                    h4("Orteils"),
                    radioButtons("amputation_jambe_g_13","Perte de cinq orteils",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_g_14","Amputation du premier orteil",c("Non","Les deux phalanges avec le métatarsien","Les deux phalanges","Phalange distale"),inline = T),
                    checkboxGroupInput("amputation_jambe_g_15","Amputation d'un orteil",c("Non","Amputation d'un orteil","Deuxième ou cinquième orteil avec leur métatarsien", "Troisième ou quatrième orteil avec leur métatarsien"), selected = "Non",inline = T)
                ),
                box(title = "Membre inférieur Droit", width = 6,
                    radioButtons("amputation_jambe_d_1","Amputation inter-ilio-abdominale",c("Non","Oui"),inline = T),
                    h3("Cuisse"),
                    radioButtons("amputation_jambe_d_2","Désarticulation de la hanche",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_3","Amputation inter-trochantérienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_4","Amputation sous-trochantérienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_5","Amputation au tiers moyen ou au tiers inférieur",c("Non","Oui"),inline = T),
                    h3("Genou et jambe"),
                    radioButtons("amputation_jambe_d_6","Désarticulation du genou",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_7","Amputation au tiers supérieur de la jambe",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_8","Amputation au tiers moyen ou inférieur",c("Non","Oui"),inline = T),
                    h3("Cheville et pied"),
                    radioButtons("amputation_jambe_d_9","Désarticulation tibio-tarsienne",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_10","Amputation du pied, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier (avec mouvement du pied restant satisfaisant et sans bascule en varus)",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_11","Désarticulation médio-tarsienne de Chopart",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_12","Amputation transmétatarsienne de l'avant-pied",c("Non","Oui"),inline = T),
                    h4("Orteils"),
                    radioButtons("amputation_jambe_d_13","Perte de cinq orteils",c("Non","Oui"),inline = T),
                    radioButtons("amputation_jambe_d_14","Amputation du premier orteil",c("Non","Les deux phalanges avec le métatarsien","Les deux phalanges","Phalange distale"),inline = T),
                    checkboxGroupInput("amputation_jambe_d_15","Amputation d'un orteil",c("Non","Amputation d'un orteil","Deuxième ou cinquième orteil avec leur métatarsien", "Troisième ou quatrième orteil avec leur métatarsien"), selected = "Non",inline = T)
                )
              )
      )
      
      
      
    )
  )
  
)

