# ICAR v0.01 ####
# Shiny server v0.01
# Dr Vincent Looten
# Date de création : 2020-02-17
# Date de dernière modification : 2020-03-27
# Text encoding : UTF-8 (test : éèçà)

server <- function(input, output) {
  
  # UI - Load pictures ####
  output$picture_cervical <-renderUI({
    HTML(paste0('<img src="','https://p6.storage.canalblog.com/69/18/1650833/123851931.jpg','" height="680" width="440">'))
  })
  
  # Synthese - Examen des Epaules ####
  output$epaule <- renderUI({
    # Conditionnal panel setup
    
    # Text functions
    if(!(input$epaule_activate=="Non")){
      epaule_text_0 <- "<h3>Examen des épaules : </h3>"  
      epaule_text_1 <- ifelse(input$epaule_activate=="Oui les deux","<em>Examen bilatéral comparatif</em>","")
      epaule_text_1 <- ifelse(input$epaule_activate=="Oui à droite","<em>Examen restreint à l'épaule droite</em>",epaule_text_1)
      epaule_text_1 <- ifelse(input$epaule_activate=="Oui à gauche","<em>Examen restreint à l'épaule gauche</em>",epaule_text_1)
      epaule_text_g0 <- ""
      epaule_text_d0 <- ""
      # Examen de l'épaule gauche
      if(input$epaule_activate %in% c("Oui à gauche","Oui les deux") ){
        # Signes observés ou allégués de l'épaule gauche
        epaule_text_g0 <- "<h4>Examen de l'épaule gauche :</h4><h5>Signes observés ou allégués de l'épaule gauche : </h5>"
        s1 <- ifelse("Impotence fonctionnelle" %in% input$signes_epaule_g, "<li>Impotence fonctionnelle</li>","<li>Pas d'impotence fonctionnelle</li>" )
        s2 <- ifelse("Douleurs avec réveil nocturne" %in% input$signes_epaule_g, "<li>Douleurs avec réveil nocturne</li>","<li>Pas de douleurs avec réveil nocturne</li>" )
        s3 <- ifelse("Impossibilité de réaliser les gestes de la vie quotidienne" %in% input$signes_epaule_g, "<li>Impossibilité de réaliser les gestes de la vie quotidienne</li>","<li>Pas de limitation significative pour réaliser les gestes de la vie quotidienne</li>" )
        s4 <- ifelse("Impossibilité de réaliser les gestes de la vie professionnelle" %in% input$signes_epaule_g, "<li>Impossibilité de réaliser les gestes de la vie professionnelle</li>","<li>Pas de limitation significative pour réaliser les gestes de la vie professionnelle</li>" )
        s5 <- ifelse("Abaissement (ou d’élévation) du moignon de l’épaule" %in% input$signes_epaule_g, "<li>Abaissement (ou d’élévation) du moignon de l’épaule</li>","<li>Pas d'abaissement ou d’élévation du moignon de l’épaule</li>" )
        s6 <- ifelse("Amyotrophie des masses sus et sous-épineuses" %in% input$signes_epaule_g, "<li>Amyotrophie des masses sus et sous-épineuses</li>","<li>Pas d'amyotrophie des masses sus et sous-épineuses</li>" )
        s7 <- ifelse(input$cal_epaule_d=="Non","<li>Pas de cal osseux à la palpation</li>","")
        s7 <- ifelse(input$cal_epaule_d=="douloureux à la palpation","<li>Cal osseux douloureux à la palpation</li>",s7)
        s7 <- ifelse(input$cal_epaule_d=="indolore à la palpation","<li>Cal osseux indolore à la palpation</li>",s7)
        # Mvt complexes à gauche
        s8 <- "<h5>Mouvements complexes de l'épaule gauche : </h5>"
        s9 <- ifelse(input$mvt_cplx_g1==-1,"<li>Main Gauche - Epaule Droite : non testé</li>","")
        s9 <- ifelse(input$mvt_cplx_g1==0,"<li>Main Gauche - Epaule Droite : complet</li>",s9)
        s9 <- ifelse(input$mvt_cplx_g1>0,paste0("<li>Main Gauche - Epaule Droite : incomplet à ",input$mvt_cplx_g1,"cm </li>"),s9)
        
        s10 <- "<li>Main G - tête : non testé</li>"
        s10 <- ifelse(input$mvt_cplx_g2==0,"<li>Main Gauche - tête : complet</li>",s10)
        s10 <- ifelse(input$mvt_cplx_g2>0,paste0("<li>Main Gauche - tête : incomplet à ",input$mvt_cplx_g2,"cm </li>"),s10)
        
        s11 <- ifelse(input$mvt_cplx_g3==-1,"<li>Main Gauche - omoplate : non testé</li>","")
        s11 <- ifelse(input$mvt_cplx_g3==0,"<li>Main Gauche - omoplate : complet</li>",s11)
        s11 <- ifelse(input$mvt_cplx_g3>0,paste0("<li>Main Gauche - omoplate : incomplet à ",input$mvt_cplx_g3,"cm </li>"),s11)
        
        if(input$epaule_activate=="Oui à gauche"){
          # Mobilité de l'épaule gauche
          s12 <- "<h4>Examen de l'épaule gauche :</h4><h5>Mobilité de l'épaule gauche : </h5>"
          s13 <- ifelse(input$mob_act_g1>0,paste0("<li>Antépulsion gauche active : ",input$mob_act_g1," degrés</li>"),"<li>Antépulsion gauche active : non mesurée</li>")
          s14 <- ifelse(input$mob_pas_g2>0,paste0("<li>Antépulsion gauche passive : ",input$mob_pas_g2," degrés</li>"),"<li>Antépulsion gauche passive : non mesurée</li>")
          s15 <- ifelse(input$mob_act_g3>0,paste0("<li>Abduction gauche active : ",input$mob_act_g3," degrés</li>"),"<li>Abduction gauche active : non mesurée</li>")
          s16 <- ifelse(input$mob_pas_g4>0,paste0("<li>Abduction gauche passive : ",input$mob_pas_g4," degrés</li>"),"<li>Abduction gauche passive : non mesurée</li>")
          
          # Mensuration de l'épaule gauche
          s17 <- "<h5>Mensurations de l'épaule gauche : </h5>"
          s18 <- ifelse(input$mens_g1>0,paste0("<li>Creux axillaire vertical : ",input$mens_g1," cm</li>"),"<li>Creux axillaire vertical : non mesuré</li>")
          s19 <- ifelse(input$mens_g2>0,paste0("<li>Biceps (mesuré à 10cm du pli du coude) : ",input$mens_g2," cm</li>"),"<li>Biceps : non mesuré</li>")
          s20 <- ifelse(input$mens_g3>0,paste0("<li>Cône antébrachial (mesuré à 10cm du pli du coude) : ",input$mens_g3," cm</li>"),"<li>Cône antébrachial : non mesuré</li>")
          
          # Force de préhension à gauche (Jamar)
          s21 <- "<h5>Force de préhension à gauche (Jamar) : </h5>"
          s22 <- ifelse(input$jamar_g1>0,paste0("<li>Premier essai : ",input$jamar_g1,"</li>"),"<li>non contributif</li>")
          s23 <- ifelse(input$jamar_g2>0,paste0("<li>Deuxième essai : ",input$jamar_g2,"</li>"),"")
          s24 <- ifelse(input$jamar_g3>0,paste0("<li>Troisième essai : ",input$jamar_g3,"</li>"),"")
          
          epaule_text_g0 <- paste0(epaule_text_g0,"<ul>",s1,s2,s3,s4,s5,s6,s7,"</ul>",s8,"<ul>",s9,s10,s11,"</ul>",s12,"<ul>",s13,s14,s15,s16,"</ul>",s17,"<ul>",s18,s19,s20,"</ul>",s21,"<ul>",s22,s23,s24,"</ul>")
          
        }else{
          epaule_text_g0 <- paste0(epaule_text_g0,"<ul>",s1,s2,s3,s4,s5,s6,s7,"</ul>",s8,"<ul>",s9,s10,s11,"</ul>")
        }
        
        
        # concaténation des chaînes de caractères pour la synthèse de l'épaule gauche
        # epaule_text_g0 <- paste0(epaule_text_g0,"<ul>",s1,s2,s3,s4,s5,s6,s7,"</ul>",s8,"<ul>",s9,s10,s11,"</ul>",s12,"<ul>",s13,s14,s15,s16,"</ul>",s17,"<ul>",s18,s19,s20,"</ul>",s21,"<ul>",s22,s23,s24,"</ul>")
      }
      
      # Examen de l'épaule droite
      if(input$epaule_activate %in% c("Oui à droite","Oui les deux") ){
        # Signes observés ou allégués de l'épaule droite
        epaule_text_d0 <- "<h5>Signes observés ou allégués de l'épaule droite : </h5>"
        s1d <- ifelse("Impotence fonctionnelle" %in% input$signes_epaule_d, "<li>Impotence fonctionnelle</li>","<li>Pas d'impotence fonctionnelle</li>" )
        s2d <- ifelse("Douleurs avec réveil nocturne" %in% input$signes_epaule_d, "<li>Douleurs avec réveil nocturne</li>","<li>Pas de douleurs avec réveil nocturne</li>" )
        s3d <- ifelse("Impossibilité de réaliser les gestes de la vie quotidienne" %in% input$signes_epaule_d, "<li>Impossibilité de réaliser les gestes de la vie quotidienne</li>","<li>Pas de limitation significative pour réaliser les gestes de la vie quotidienne</li>" )
        s4d <- ifelse("Impossibilité de réaliser les gestes de la vie professionnelle" %in% input$signes_epaule_d, "<li>Impossibilité de réaliser les gestes de la vie professionnelle</li>","<li>Pas de limitation significative pour réaliser les gestes de la vie professionnelle</li>" )
        s5d <- ifelse("Abaissement (ou d’élévation) du moignon de l’épaule" %in% input$signes_epaule_d, "<li>Abaissement (ou d’élévation) du moignon de l’épaule</li>","<li>Pas d'abaissement ou d’élévation du moignon de l’épaule</li>" )
        s6d <- ifelse("Amyotrophie des masses sus et sous-épineuses" %in% input$signes_epaule_d, "<li>Amyotrophie des masses sus et sous-épineuses</li>","<li>Pas d'amyotrophie des masses sus et sous-épineuses</li>" )
        s7d <- ifelse(input$cal_epaule_d=="Non","<li>Pas de cal osseux à la palpation</li>","")
        s7d <- ifelse(input$cal_epaule_d=="douloureux à la palpation","<li>Cal osseux douloureux à la palpation</li>",s7d)
        s7d <- ifelse(input$cal_epaule_d=="indolore à la palpation","<li>Cal osseux indolore à la palpation</li>",s7d)
        # Mvt complexes à droite
        s8d <- "<h5>Mouvements complexes de l'épaule droite : </h5>"
        s9d <- ifelse(input$mvt_cplx_d1==-1,"<li>Main Droite - Epaule Gauche : non testé</li>","")
        s9d <- ifelse(input$mvt_cplx_d1==0,"<li>Main Droite - Epaule Gauche : complet</li>",s9d)
        s9d <- ifelse(input$mvt_cplx_d1>0,paste0("<li>Main Droite - Epaule Gauche : incomplet à ",input$mvt_cplx_d1,"cm </li>"),s9d)
        
        s10d <- "<li>Main D - tête : non testé</li>"
        s10d <- ifelse(input$mvt_cplx_d2==0,"<li>Main Droite - tête : complet</li>",s10d)
        s10d <- ifelse(input$mvt_cplx_d2>0,paste0("<li>Main Droite - tête : incomplet à ",input$mvt_cplx_d2,"cm </li>"),s10d)
        
        s11d <- ifelse(input$mvt_cplx_d3==-1,"<li>Main Droite - omoplate : non testé</li>","")
        s11d <- ifelse(input$mvt_cplx_d3==0,"<li>Main Droite - omoplate : complet</li>",s11d)
        s11d <- ifelse(input$mvt_cplx_d3>0,paste0("<li>Main Droite - omoplate : incomplet à ",input$mvt_cplx_d3,"cm </li>"),s11d)
        
        if(input$epaule_activate=="Oui à droite"){
          # Mobilité de l'épaule droite
          s12d <- "<h4>Examen de l'épaule droite :</h4><h5>Mobilité de l'épaule droite : </h5>"
          s13d <- ifelse(input$mob_act_d1>0,paste0("<li>Antépulsion droite active : ",input$mob_act_d1," degrés</li>"),"<li>Antépulsion droite active : non mesurée</li>")
          s14d <- ifelse(input$mob_pas_d2>0,paste0("<li>Antépulsion droite passive : ",input$mob_pas_d2," degrés</li>"),"<li>Antépulsion droite passive : non mesurée</li>")
          s15d <- ifelse(input$mob_act_d3>0,paste0("<li>Abduction droite active : ",input$mob_act_d3," degrés</li>"),"<li>Abduction droite active : non mesurée</li>")
          s16d <- ifelse(input$mob_pas_d4>0,paste0("<li>Abduction droite passive : ",input$mob_pas_d4," degrés</li>"),"<li>Abduction droite passive : non mesurée</li>")
          
          # Mensuration de l'épaule droite
          s17d <- "<h5>Mensurations de l'épaule droite : </h5>"
          s18d <- ifelse(input$mens_d1>0,paste0("<li>Creux axillaire vertical : ",input$mens_d1," cm</li>"),"<li>Creux axillaire vertical : non mesuré</li>")
          s19d <- ifelse(input$mens_d2>0,paste0("<li>Biceps (mesuré à 10cm du pli du coude) : ",input$mens_d2," cm</li>"),"<li>Biceps : non mesuré</li>")
          s20d <- ifelse(input$mens_d3>0,paste0("<li>Cône antébrachial (mesuré à 10cm du pli du coude) : ",input$mens_d3," cm</li>"),"<li>Cône antébrachial : non mesuré</li>")
          
          # Force de préhension à droite (Jamar)
          s21d <- "<h5>Force de préhension à droite (Jamar) : </h5>"
          s22d <- ifelse(input$jamar_d1>0,paste0("<li>Premier essai : ",input$jamar_d1,"</li>"),"<li>non contributif</li>")
          s23d <- ifelse(input$jamar_d2>0,paste0("<li>Deuxième essai : ",input$jamar_d2,"</li>"),"")
          s24d <- ifelse(input$jamar_d3>0,paste0("<li>Troisième essai : ",input$jamar_d3,"</li>"),"")
          
          epaule_text_d0 <- paste0(epaule_text_d0,"<ul>",s1d,s2d,s3d,s4d,s5d,s6d,s7d,"</ul>",s8d,"<ul>",s9d,s10d,s11d,"</ul>",s12d,"<ul>",s13d,s14d,s15d,s16d,"</ul>",s17d,"<ul>",s18d,s19d,s20d,"</ul>",s21d,"<ul>",s22d,s23d,s24d,"</ul>")
          
        }else{
          epaule_text_d0 <- paste0(epaule_text_d0,"<ul>",s1d,s2d,s3d,s4d,s5d,s6d,s7d,"</ul>",s8d,"<ul>",s9d,s10d,s11d,"</ul>")
        }
        
        
        # concaténation des chaînes de caractères pour la synthèse de l'épaule gauche
        # epaule_text_d0 <- paste0(epaule_text_d0,"<ul>",s1d,s2d,s3d,s4d,s5d,s6d,s7d,"</ul>",s8d,"<ul>",s9d,s10d,s11d,"</ul>",s12d,"<ul>",s13d,s14d,s15d,s16d,"</ul>",s17d,"<ul>",s18d,s19d,s20d,"</ul>",s21d,"<ul>",s22d,s23d,s24d,"</ul>")
      }
      
      # Texte final de l'épaule
      if(input$epaule_activate %in% c("Oui à droite", "Oui à gauche") ){
        epaule_synthese <- paste(paste0(epaule_text_0,epaule_text_1,epaule_text_g0,epaule_text_d0),sep="<br/>")
      }
      if(input$epaule_activate=="Oui les deux"){
        
        mensuration <- as.data.frame(matrix(NA,ncol=2,nrow=3), row.names = c("Creux axillaire vertical","Biceps (mesuré à 10cm du pli du coude)","Cône antébrachial (mesuré à 10cm du pli du coude)"))
        colnames(mensuration) <- c("Gauche","Droit")
        mensuration[1,] <- c(ifelse(input$mens_g1>0,input$mens_g1,""),ifelse(input$mens_d1>0,input$mens_d1,"") )
        mensuration[2,] <- c(ifelse(input$mens_g2>0,input$mens_g2,""),ifelse(input$mens_d2>0,input$mens_d2,"") )
        mensuration[3,] <- c(ifelse(input$mens_g3>0,input$mens_g3,""),ifelse(input$mens_d3>0,input$mens_d3,"") )
        tab_mensuration <- tableHTML(mensuration,widths = rep(100, 3)) %>%  add_css_column(css = list('text-align', 'center'), columns = names(mensuration)) %>% add_css_header(css =list('text-align', 'center'),headers = c(2:3))
        
        #
        mobilisation <- as.data.frame(matrix(NA,ncol=2,nrow=4), row.names = c("Antépulsion active","Antépulsion passive","Abduction active","Abduction passive"))
        colnames(mobilisation) <- c("Gauche","Droit")
        mobilisation[1,] <- c(ifelse(input$mob_act_g1>0,input$mob_act_g1,""),ifelse(input$mob_act_d1>0,input$mob_act_d1,"") )
        mobilisation[2,] <- c(ifelse(input$mob_pas_g2>0,input$mob_pas_g2,""),ifelse(input$mob_pas_d2>0,input$mob_pas_d2,"") )
        mobilisation[3,] <- c(ifelse(input$mob_act_g3>0,input$mob_act_g3,""),ifelse(input$mob_act_d3>0,input$mob_act_d3,"") )
        mobilisation[4,] <- c(ifelse(input$mob_pas_g4>0,input$mob_pas_g4,""),ifelse(input$mob_pas_d4>0,input$mob_pas_d4,"") )
        tab_mobilisation <- tableHTML(mobilisation,widths = rep(100, 3)) %>%  add_css_column(css = list('text-align', 'center'), columns = names(mobilisation)) %>% add_css_header(css =list('text-align', 'center'),headers = c(2:3))
        #
        jamar <- as.data.frame(matrix(NA,ncol=2,nrow=3), row.names = c("Jamar (1re essai)","Jamar (2eme essai)","Jamar (3eme essai)"))
        jamar[,1] <- c(ifelse(input$jamar_g1>0,input$jamar_g1,""),ifelse(input$jamar_g2>0,input$jamar_g2,""),ifelse(input$jamar_g3>0,input$jamar_g3,""))
        jamar[,2] <- c(ifelse(input$jamar_d1>0,input$jamar_d1,""),ifelse(input$jamar_d2>0,input$jamar_d2,""),ifelse(input$jamar_d3>0,input$jamar_d3,""))
        colnames(jamar) <- c("Gauche","Droit")
        tab_jamar <- tableHTML(jamar,widths = rep(100, 3)) %>%  add_css_column(css = list('text-align', 'center'), columns = names(mensuration)) %>% add_css_header(css =list('text-align', 'center'),headers = c(2:3))
        
        epaule_synthese <- paste(paste0(epaule_text_0,epaule_text_1,epaule_text_g0,epaule_text_d0,"<h4>Examen de l'épaule :</h4><h5>Mobilité de l'épaule : </h5>",tab_mobilisation,"<h5>Mensurations en cm : </h5>",tab_mensuration,"<h5>Force de préhension à droite (Jamar) : </h5>",tab_jamar)) #,sep="<br/>"
      }
      
    }else{
      epaule_synthese <- "" 
    }
    HTML(epaule_synthese)
  })
  
  # Synthese - Examen des coudes ####
  
  output$synthese_coude <- renderUI({
    
    coude_synthese <- ""
    
    if(input$epicondylite_activate!="Non"){
      
      coude_synthese <- "<h3>Examen des coudes : </h3>"
      
      if(input$epicondylite_activate %in% c("Oui les deux","Oui à gauche")){
        
        coude_text_g_0 <- "<h4>Examen du coude gauche</h4>"
        coude_text_g_1 <- paste0("<h5>Signes subjectifs allégués : ",input$coude_g_signessubj_all,"</h5>")
        coude_text_g_2 <- paste0("<h5>Gêne fonctionnelle alléguée : ",input$coude_g_genefonct_all,"</h5>")
        coude_text_g_3 <- ifelse(input$coude_g_portcharge>0,paste0("<h5>","Port de charge maximum en kg : ",input$coude_g_portcharge,"kg</h5>"), "<h5>Pas de limitation de port de charge rapportée</h5>")
        
        coude_text_g_4 <- paste0("<h5>Morphologie du coude : ",input$coude_g_morpho,"</h5>")
        coude_text_g_5 <- paste0("<h5>Amyotrophie locale : ",input$coude_g_amyo,"</h5>")
        coude_text_g_6 <- paste0("<h5>Tuméfaction musculaire : ",input$coude_g_tum,"</h5>")
        coude_text_g_7 <- paste0("<h5>Cicatrice : ",input$coude_g_cic,"</h5>")
        coude_text_g_8 <- paste0("<h5>Douleur provoquée à l'épicondyle interne : ",input$coude_g_douleur_int,"</h5>")
        coude_text_g_9 <- paste0("<h5>Douleur provoquée à l'épicondyle externe : ",input$coude_g_douleur_ext,"</h5>")
        
        coude_text_g_10 <- "<h5>Douleurs épitrochléennes provoquées par les mouvements contrariés : ... <h5>"
        coude_text_g_11 <- paste0("<h5>...de flexion du poignet et des doigts : ",input$coude_g_mvt1,"</h5>")
        coude_text_g_12 <- paste0("<h5>...de pronation du poignet : ",input$coude_g_mvt2,"</h5>")
        
        coude_text_g_13 <- "<h5>Douleurs épicondyliennes provoquées par les mouvements contrariés : ... <h5>"
        coude_text_g_14 <- paste0("<h5>...d’extension des doigts longs : ",input$coude_g_mvt3,"</h5>")
        coude_text_g_15 <- paste0("<h5>...de supination du poignet : ",input$coude_g_mvt4,"</h5>")
        
        coude_text_g_16 <- "<h5>Mobilité articulaire : <h5>"
        coude_text_g_17 <- ifelse(input$coude_g_flessum>=0,paste0("<h5>","Flessum : ",input$coude_g_flessum," degrés</h5>"),"<h5>Pas de Flessum <h5>")
        coude_text_g_18 <- paste0("<h5>Mobilité de l'articulation : ",input$coude_g_mob1,"</h5>")
        coude_text_g_19 <- ""
        if(input$coude_g_mob1 == 'Limitation des mouvements de flexion-extension'){
          coude_text_g_19 <- paste0("<h5>Angle de limitation : ",input$coude_g_mob2,"</h5>")
        }
        if(input$coude_g_mob1 == 'Blocage de la flexion-extension'){
          coude_text_g_19 <- paste0("<h5>Blocage articulaire : ",input$coude_g_mob3,"</h5>")
        }
        
        coude_synthese <- paste0(coude_synthese,coude_text_g_0,coude_text_g_1,coude_text_g_2,coude_text_g_3,coude_text_g_4,coude_text_g_5,coude_text_g_6,coude_text_g_7,coude_text_g_8,coude_text_g_9,coude_text_g_10,coude_text_g_11,coude_text_g_12,coude_text_g_13,coude_text_g_14,coude_text_g_15,coude_text_g_16,coude_text_g_17,coude_text_g_18,coude_text_g_19)
        
      }
      
      if(input$epicondylite_activate %in% c("Oui les deux","Oui à droite")){
        
        coude_text_d_0 <- "<h4>Examen du coude droit</h4>"
        coude_text_d_1 <- paste0("<h5>Signes subjectifs allégués : ",input$coude_d_signessubj_all,"</h5>")
        coude_text_d_2 <- paste0("<h5>Gêne fonctionnelle alléguée : ",input$coude_d_genefonct_all,"</h5>")
        coude_text_d_3 <- ifelse(input$coude_g_portcharge>0,paste0("<h5>","Port de charge maximum en kg : ",input$coude_d_portcharge,"kg</h5>"), "<h5>Pas de limitation de port de charge rapportée</h5>")
        
        coude_text_d_4 <- paste0("<h5>Morphologie du coude : ",input$coude_d_morpho,"</h5>")
        coude_text_d_5 <- paste0("<h5>Amyotrophie locale : ",input$coude_d_amyo,"</h5>")
        coude_text_d_6 <- paste0("<h5>Tuméfaction musculaire : ",input$coude_d_tum,"</h5>")
        coude_text_d_7 <- paste0("<h5>Cicatrice : ",input$coude_d_cic,"</h5>")
        coude_text_d_8 <- paste0("<h5>Douleur provoquée à l'épicondyle interne : ",input$coude_d_douleur_int,"</h5>")
        coude_text_d_9 <- paste0("<h5>Douleur provoquée à l'épicondyle externe : ",input$coude_d_douleur_ext,"</h5>")
        
        coude_text_d_10 <- "<h5>Douleurs épitrochléennes provoquées par les mouvements contrariés : ... <h5>"
        coude_text_d_11 <- paste0("<h5>...de flexion du poignet et des doigts : ",input$coude_d_mvt1,"</h5>")
        coude_text_d_12 <- paste0("<h5>...de pronation du poignet : ",input$coude_d_mvt2,"</h5>")
        
        coude_text_d_13 <- "<h5>Douleurs épicondyliennes provoquées par les mouvements contrariés : ... <h5>"
        coude_text_d_14 <- paste0("<h5>...d’extension des doigts longs : ",input$coude_d_mvt3,"</h5>")
        coude_text_d_15 <- paste0("<h5>...de supination du poignet : ",input$coude_d_mvt4,"</h5>")
        
        coude_text_d_16 <- "<h5>Mobilité articulaire : <h5>"
        coude_text_d_17 <- ifelse(input$coude_d_flessum>=0,paste0("<h5>","Flessum : ",input$coude_d_flessum," degrés</h5>"),"<h5>Pas de Flessum <h5>")
        coude_text_d_18 <- paste0("<h5>Mobilité de l'articulation : ",input$coude_d_mob1,"</h5>")
        coude_text_d_19 <- ""
        if(input$coude_d_mob1 == 'Limitation des mouvements de flexion-extension'){
          coude_text_d_19 <- paste0("<h5>Angle de limitation : ",input$coude_d_mob2,"</h5>")
        }
        if(input$coude_d_mob1 == 'Blocage de la flexion-extension'){
          coude_text_d_19 <- paste0("<h5>Blocage articulaire : ",input$coude_d_mob3,"</h5>")
        }
        
        coude_synthese <- paste0(coude_synthese,coude_text_d_0,coude_text_d_1,coude_text_d_2,coude_text_d_3,coude_text_d_4,coude_text_d_5,coude_text_d_6,coude_text_d_7,coude_text_d_8,coude_text_d_9,coude_text_d_10,coude_text_d_11,coude_text_d_12,coude_text_d_13,coude_text_d_14,coude_text_d_15,coude_text_d_16,coude_text_d_17,coude_text_d_18,coude_text_d_19)
        
      }
      
    }
    
    HTML(coude_synthese)
  })
  
  # Synthese - Examen des poignets ####
  
  output$synthese_poignet <- renderUI({
    
    poignet_synthese <- ""
    
    if(input$tenosynovite_activate!="Non"){
      
      poignet_synthese <- "<h3>Examen des poignets : </h3>"
      
      if(input$tenosynovite_activate %in% c("Oui les deux","Oui à gauche")){
        
        poignet_text_g_0 <- "<h4>Examen du poignet gauche</h4>"
        poignet_text_g_1 <- paste0("<h5>Douleur de la styloïde radiale : ",input$poignet_g_douleurstyloide,"</h5>")
        poignet_text_g_2 <- paste0("<h5>Gêne fonctionnelle : ",input$poignet_g_genefct,"</h5>")
        poignet_text_g_3 <- paste0("<h5>Tuméfaction bord externe poignet : ",input$poignet_g_tumext,"</h5>")
        poignet_text_g_4 <- paste0("<h5>Cicatrice : ",input$poignet_g_cic,"</h5>")
        poignet_text_g_5 <- paste0("<h5>Douleur provoquée à la palpation : ",input$poignet_g_dlpalp,"</h5>")
        poignet_text_g_6 <- "<h5>Douleurs styloïdiennes déclenchées par les mouvements contrariés : ...</h5>"
        poignet_text_g_7 <- paste0("<h5>Long abducteur et court extenseur du pouce : ",input$poignet_g_lgabd,"</h5>")
        poignet_text_g_8 <- paste0("<h5>Test de Finkelstein : ",input$poignet_g_finkelstein,"</h5>")
        poignet_text_g_9 <- "<h5>Mensurations de l'épaule gauche : ...</h5>"
        poignet_text_g_10 <- ifelse(input$mens_g1_querv>=0,paste0("<h5>Cône antébrachial (mesuré à 10cm du pli du coude) :",input$mens_g1_querv, "</h5>"),"<h5>Cône antébrachial (mesuré à 10cm du pli du coude) : non mesuré</h5>")
        poignet_text_g_11 <- ifelse(input$mens_g2_querv>=0,paste0("<h5>Poignet :",input$mens_g2_querv, "</h5>"),"<h5>Poignet : non mesuré</h5>")
        poignet_text_g_12 <- ifelse(input$mens_g3_querv>=0,paste0("<h5>Gantier :",input$mens_g3_querv, "</h5>"),"<h5>Gantier : non mesuré</h5>")
        poignet_text_g_13 <- "<h5>Force de préhension à gauche(Jamar) : ...</h5>"
        poignet_text_g_14 <- ifelse(input$jamar_g1_querv>=0,paste0("<h5>1re essai :",input$jamar_g1_querv, "</h5>"),"<h5>1re essai : non mesuré</h5>")
        poignet_text_g_15 <- ifelse(input$jamar_g2_querv>=0,paste0("<h5>2nd essai :",input$jamar_g2_querv, "</h5>"),"<h5>2nd essai : non mesuré</h5>")
        poignet_text_g_16 <- ifelse(input$jamar_g3_querv>=0,paste0("<h5>3eme essai :",input$jamar_g3_querv, "</h5>"),"<h5>3eme essai : non mesuré</h5>")
        
        poignet_text_g <- paste0(poignet_text_g_0,poignet_text_g_1,poignet_text_g_2,poignet_text_g_3,poignet_text_g_4,poignet_text_g_5,poignet_text_g_5,poignet_text_g_6,poignet_text_g_7,poignet_text_g_8,poignet_text_g_9,poignet_text_g_10,poignet_text_g_11,poignet_text_g_12,poignet_text_g_13,poignet_text_g_14,poignet_text_g_15,poignet_text_g_16)
        poignet_synthese <- paste0(poignet_synthese,poignet_text_g)
        
        }
      if(input$tenosynovite_activate %in% c("Oui les deux","Oui à droite")){
        
        poignet_text_d_0 <- "<h4>Examen du poignet droit</h4>"
        poignet_text_d_1 <- paste0("<h5>Douleur de la styloïde radiale : ",input$poignet_d_douleurstyloide,"</h5>")
        poignet_text_d_2 <- paste0("<h5>Gêne fonctionnelle : ",input$poignet_d_genefct,"</h5>")
        poignet_text_d_3 <- paste0("<h5>Tuméfaction bord externe poignet : ",input$poignet_d_tumext,"</h5>")
        poignet_text_d_4 <- paste0("<h5>Cicatrice : ",input$poignet_d_cic,"</h5>")
        poignet_text_d_5 <- paste0("<h5>Douleur provoquée à la palpation : ",input$poignet_d_dlpalp,"</h5>")
        poignet_text_d_6 <- "<h5>Douleurs styloïdiennes déclenchées par les mouvements contrariés : ...</h5>"
        poignet_text_d_7 <- paste0("<h5>Long abducteur et court extenseur du pouce : ",input$poignet_d_lgabd,"</h5>")
        poignet_text_d_8 <- paste0("<h5>Test de Finkelstein : ",input$poignet_d_finkelstein,"</h5>")
        poignet_text_d_9 <- "<h5>Mensurations de l'épaule droite : ...</h5>"
        poignet_text_d_10 <- ifelse(input$mens_d1_querv>=0,paste0("<h5>Cône antébrachial (mesuré à 10cm du pli du coude) :",input$mens_d1_querv, "</h5>"),"<h5>Cône antébrachial (mesuré à 10cm du pli du coude) : non mesuré</h5>")
        poignet_text_d_11 <- ifelse(input$mens_d2_querv>=0,paste0("<h5>Poignet :",input$mens_d2_querv, "</h5>"),"<h5>Poignet : non mesuré</h5>")
        poignet_text_d_12 <- ifelse(input$mens_d3_querv>=0,paste0("<h5>Gantier :",input$mens_d3_querv, "</h5>"),"<h5>Gantier : non mesuré</h5>")
        poignet_text_d_13 <- "<h5>Force de préhension à droite (Jamar) : ...</h5>"
        poignet_text_d_14 <- ifelse(input$jamar_d1_querv>=0,paste0("<h5>1re essai :",input$jamar_d1_querv, "</h5>"),"<h5>1re essai : non mesuré</h5>")
        poignet_text_d_15 <- ifelse(input$jamar_d2_querv>=0,paste0("<h5>2nd essai :",input$jamar_d2_querv, "</h5>"),"<h5>2nd essai : non mesuré</h5>")
        poignet_text_d_16 <- ifelse(input$jamar_d3_querv>=0,paste0("<h5>3eme essai :",input$jamar_d3_querv, "</h5>"),"<h5>3eme essai : non mesuré</h5>")
        
        poignet_text_d <- paste0(poignet_text_d_0,poignet_text_d_1,poignet_text_d_2,poignet_text_d_3,poignet_text_d_4,poignet_text_d_5,poignet_text_d_5,poignet_text_d_6,poignet_text_d_7,poignet_text_d_8,poignet_text_d_9,poignet_text_d_10,poignet_text_d_11,poignet_text_d_12,poignet_text_d_13,poignet_text_d_14,poignet_text_d_15,poignet_text_d_16)
        
        poignet_synthese <- paste0(poignet_synthese,poignet_text_d)
      }
      
    }
    
    HTML(poignet_synthese)
    
  })
  
  # Synthese - Examen des canaux carpiens ####
  
  output$synthese_canalcarpiens <- renderUI({
    if(!(input$cc_activate=="Non")){
      cc_text_0 <- "<h3>Examen des poignets et des mains : </h3>"  
      cc_text_1 <- ifelse(input$cc_activate=="Oui les deux","<em>Examen bilatéral comparatif des poignets et des mains</em>","")
      cc_text_1 <- ifelse(input$cc_activate=="Oui à droite","<em>Examen restreint du poignet droit</em>",cc_text_1)
      cc_text_1 <- ifelse(input$cc_activate=="Oui à gauche","<em>Examen restreint du poignet gauche</em>",cc_text_1)
      cc_text_g0 <- ""
      cc_text_d0 <- ""
      # Examen du canal carpien gauche 
      if(input$cc_activate %in% c("Oui à gauche","Oui les deux") ){
        # Signes observés ou allégués canal carpien gauche
        cc_text_g0 <- "<h3>Examen du poignet et de la main gauche :</h3><h4>Doléances : </h4>"
        cc_text_g0 <- paste0(cc_text_g0,"<h5>Sensibilité locale face antérieure poignet : </h5><ul>")
        cc1 <- ifelse("Non douloureux" %in% input$cc_douleurs_g, "<li>Face antérieure du poignet non douloureuse ni spontanément, ni à la palpation</li>","")
        cc2 <- ifelse("Douloureux à la palpation" %in% input$cc_douleurs_g, "<li>Face antérieure du poignet douloureuse à la palpation</li>","<li>Face antérieure du poignet non douloureuse à la palpation</li>")
        cc3 <- ifelse("Douloureux spontanément" %in% input$cc_douleurs_g, "<li>Face antérieure du poignet douloureuse spontanément</li>","")
        cc_text_g0b <- "<h5>Signes sensitifs : </h5><ul>"
        cc4 <- ifelse("Minime" %in% input$cc_fonctionnel_g,"<li>Signes sensitifs minimes, pas d'acroparesthésie permanente ou insomniante</li>","")
        cc5 <- ifelse("acroparesthésies occasionnelles" %in% input$cc_fonctionnel_g,"<li>Acroparesthésies occasionnelles</li>","")
        cc6 <- ifelse("Acroparesthésies permanentes non insomniantes" %in% input$cc_fonctionnel_g,"<li>Acroparesthésies permanentes non insomniantes</li>","")
        cc7 <- ifelse("Acroparesthésies permanentes et insonmiantes" %in% input$cc_fonctionnel_g,"<li>Acroparesthésies permanentes et insonmiantes</li>","")
        cc8 <- ifelse("engourdissement matinal de la main" %in% input$cc_fonctionnel_g,"<li>Engourdissement matinal de la main</li>","")
        cc_text_g0c <- "<h5>Gène fonctionnelle : </h5><ul>"
        cc9 <- ifelse("Minime" %in% input$cc_sensitif_g,"<li>Gène fonctionnelle minimes</li>","")
        cc10 <- ifelse("Diminution force musculaire globale de la main" %in% input$cc_sensitif_g,"<li>Diminution force musculaire globale de la main</li>","")
        cc11 <- ifelse("Gène à la préhension (maladresse)" %in% input$cc_sensitif_g,"<li>Gène à la préhension (maladresse)</li>","")
        cc_text_g0d <- paste0("<h4>Examen clinique : </h4><h5>Examen cutané : ",input$cc_g_cic,"</h5>")
        cc_text_g0e <- paste0("<h5>Amyotrophie thénardienne : ",input$cc_g_amyo_the,"</h5>")
        cc_text_g0f <- paste0("<h5>Troubles vasomoteurs : ",input$cc_g_tbvaso,"</h5>")
        cc12b <- "<h5>Etude des pinces :</h5><ul>"
        cc12 <- ifelse("Normale en forme et en force" %in% input$cc_pincepp_g,"<li>Pince pouce-index normale en forme et en force</li>","")
        cc13 <- ifelse("Altérée en forme" %in% input$cc_pincepp_g,"<li>Pince pouce-index altérée en forme</li>","")
        cc14 <- ifelse("Altérée en force 4/5" %in% input$cc_pincepp_g,"<li>Pince pouce-index altérée en force (4/5)</li>","")
        cc15 <- ifelse("Altérée en force 3/5" %in% input$cc_pincepp_g,"<li>Pince pouce-index altérée en force (3/5)</li>","")
        #
        cc16 <- ifelse("Normale en forme et en force" %in% input$cc_pincepd_g,"<li>Pince pollicidigitale normale en forme et en force</li>","")
        cc17 <- ifelse("Altérée en forme" %in% input$cc_pincepd_g,"<li>Pince pollicidigitale altérée en forme</li>","")
        cc18 <- ifelse("Altérée en force 4/5" %in% input$cc_pincepd_g,"<li>Pince pollicidigitale altérée en force (4/5)</li>","")
        cc19 <- ifelse("Altérée en force 3/5" %in% input$cc_pincepd_g,"<li>Pince pollicidigitale altérée en force (3/5)</li>","")
        #
        cc_text_g0g <- paste0("<h5>Diminution de la force d’abduction du pouce : ",input$cc_g_force_abd,"</h5>")
        cc_text_g0h <- paste0("<h5>Distance entre le pouce et la  base du 5eme doigts : ",input$cc_g_opp_p,"cm</h5>")
        cc_text_g0i <- paste0("<h5>Altérations des fonctions articulaires secondaires : ",input$cc_g_fct_art,"</h5>")
        #
        cc20b <- "<h5>Force de préhension à gauche (Jamar) : </h5><ul>"
        cc20 <- ifelse(input$jamar_g1_cc>0,paste0("<li>Premier essai : ",input$jamar_g1_cc,"</li>"),"<li>non contributif</li>")
        cc21 <- ifelse(input$jamar_g2_cc>0,paste0("<li>Deuxième essai : ",input$jamar_g2_cc,"</li>"),"")
        cc22 <- ifelse(input$jamar_g3_cc>0,paste0("<li>Troisième essai : ",input$jamar_g3_cc,"</li>"),"")
        
        # concaténation des chaînes de caractères pour la synthèse du canal carpien gauche
        cc_text_g0 <- paste0(cc_text_g0,cc1,cc2,cc3,"</ul>",cc_text_g0b,cc4,cc5,cc6,cc7,cc8,"</ul>",cc_text_g0c,cc9,cc10,cc11,"</ul>",cc_text_g0d,cc_text_g0e,cc_text_g0f,cc12b,cc12,cc13,cc14,cc15,cc16,cc17,cc18,cc19,"</ul>",cc_text_g0g,cc_text_g0h,cc_text_g0i,cc20b,cc20,cc21,cc22,"</ul>")
      }
      
      # Examen du canal carpien droit
      if(input$cc_activate %in% c("Oui à droite","Oui les deux") ){
        # Signes observés ou allégués du canal carpien droit
        cc_text_d0 <- "<h3>Examen du poignet et de la main droit :</h3><h4>Doléances : </h4>"
        cc_text_d0 <- paste0(cc_text_d0,"<h5>Sensibilité locale face antérieure poignet : </h5><ul>")
        cc1d <- ifelse("Non douloureux" %in% input$cc_douleurs_d, "<li>Face antérieure du poignet non douloureuse ni spontanément, ni à la palpation</li>","")
        cc2d <- ifelse("Douloureux à la palpation" %in% input$cc_douleurs_d, "<li>Face antérieure du poignet douloureuse à la palpation</li>","<li>Face antérieure du poignet non douloureuse à la palpation</li>")
        cc3d <- ifelse("Douloureux spontanément" %in% input$cc_douleurs_d, "<li>Face antérieure du poignet douloureuse spontanément</li>","")
        cc_text_d0b <- "<h5>Signes sensitifs : </h5><ul>"
        cc4d <- ifelse("Minime" %in% input$cc_fonctionnel_d,"<li>Signes sensitifs minimes, pas d'acroparesthésie permanente ou insomniante</li>","")
        cc5d <- ifelse("acroparesthésies occasionnelles" %in% input$cc_fonctionnel_d,"<li>Acroparesthésies occasionnelles</li>","")
        cc6d <- ifelse("Acroparesthésies permanentes non insomniantes" %in% input$cc_fonctionnel_d,"<li>Acroparesthésies permanentes non insomniantes</li>","")
        cc7d <- ifelse("Acroparesthésies permanentes et insonmiantes" %in% input$cc_fonctionnel_d,"<li>Acroparesthésies permanentes et insonmiantes</li>","")
        cc8d <- ifelse("engourdissement matinal de la main" %in% input$cc_fonctionnel_d,"<li>Engourdissement matinal de la main</li>","")
        cc_text_d0c <- "<h5>Gène fonctionnelle : </h5><ul>"
        cc9d <- ifelse("Minime" %in% input$cc_sensitif_d,"<li>Gène fonctionnelle minimes</li>","")
        cc10d <- ifelse("Diminution force musculaire globale de la main" %in% input$cc_sensitif_d,"<li>Diminution force musculaire globale de la main</li>","")
        cc11d <- ifelse("Gène à la préhension (maladresse)" %in% input$cc_sensitif_d,"<li>Gène à la préhension (maladresse)</li>","")
        cc_text_d0d <- paste0("<h4>Examen clinique : </h4><h5>Examen cutané : ",input$cc_d_cic,"</h5>")
        cc_text_d0e <- paste0("<h5>Amyotrophie thénardienne : ",input$cc_d_amyo_the,"</h5>")
        cc_text_d0f <- paste0("<h5>Troubles vasomoteurs : ",input$cc_d_tbvaso,"</h5>")
        cc12bd <- "<h5>Etude des pinces :</h5><ul>"
        cc12d <- ifelse("Normale en forme et en force" %in% input$cc_pincepp_d,"<li>Pince pouce-index normale en forme et en force</li>","")
        cc13d <- ifelse("Altérée en forme" %in% input$cc_pincepp_d,"<li>Pince pouce-index altérée en forme</li>","")
        cc14d <- ifelse("Altérée en force 4/5" %in% input$cc_pincepp_d,"<li>Pince pouce-index altérée en force (4/5)</li>","")
        cc15d <- ifelse("Altérée en force 3/5" %in% input$cc_pincepp_d,"<li>Pince pouce-index altérée en force (3/5)</li>","")
        #
        cc16d <- ifelse("Normale en forme et en force" %in% input$cc_pincepd_d,"<li>Pince pollicidigitale normale en forme et en force</li>","")
        cc17d <- ifelse("Altérée en forme" %in% input$cc_pincepd_d,"<li>Pince pollicidigitale altérée en forme</li>","")
        cc18d <- ifelse("Altérée en force 4/5" %in% input$cc_pincepd_d,"<li>Pince pollicidigitale altérée en force (4/5)</li>","")
        cc19d <- ifelse("Altérée en force 3/5" %in% input$cc_pincepd_d,"<li>Pince pollicidigitale altérée en force (3/5)</li>","")
        #
        cc_text_d0g <- paste0("<h5>Diminution de la force d’abduction du pouce : ",input$cc_d_force_abd,"</h5>")
        cc_text_d0h <- paste0("<h5>Distance entre le pouce et la  base du 5eme doigts : ",input$cc_d_opp_p,"cm</h5>")
        cc_text_d0i <- paste0("<h5>Altérations des fonctions articulaires secondaires : ",input$cc_d_fct_art,"</h5>")
        #
        cc20bd <- "<h5>Force de préhension à droite (Jamar) : </h5><ul>"
        cc20d <- ifelse(input$jamar_d1_cc>0,paste0("<li>Premier essai : ",input$jamar_d1_cc,"</li>"),"<li>non contributif</li>")
        cc21d <- ifelse(input$jamar_d2_cc>0,paste0("<li>Deuxième essai : ",input$jamar_d2_cc,"</li>"),"")
        cc22d <- ifelse(input$jamar_d3_cc>0,paste0("<li>Troisième essai : ",input$jamar_d3_cc,"</li>"),"")
        
        # concaténation des chaînes de caractères pour la synthèse du canal carpien droit
        cc_text_d0 <- paste0(cc_text_d0,cc1d,cc2d,cc3d,"</ul>",cc_text_d0b,cc4d,cc5d,cc6d,cc7d,cc8d,"</ul>",cc_text_d0c,cc9d,cc10d,cc11d,"</ul>",cc_text_d0d,cc_text_d0e,cc_text_d0f,cc12bd,cc12d,cc13d,cc14d,cc15d,cc16d,cc17d,cc18d,cc19d,"</ul>",cc_text_d0g,cc_text_d0h,cc_text_d0i,cc20bd,cc20d,cc21d,cc22d,"</ul>")
        
      }
      
      # Texte final du canal carpien
      cc_synthese <- paste(paste0(cc_text_0,cc_text_1,cc_text_g0,cc_text_d0)) # ,sep="<br/>"
      
    }else{
      cc_synthese <- "" 
    }
    HTML(cc_synthese)
  })
  
  # Synthese - Examen des mains ####
  
  output$mains_synthese <- renderUI({
    
    mains_synthese <- ""
    
    if(input$mainspoignets_activate %in% c("Oui à gauche","Oui les deux") ){
      main_g_0 <- "<h3>Examen du poignet gauche :</h3>"
      main_g_1 <- "<h4>Signes rapportés :</h4>"
      main_g_2 <- paste0("<h5>Gêne fonctionnelle : ",input$poignet_g_genefct_2,"</h5>")
      main_g_3 <- paste0("<h5>Signes subjectifs : ",input$poignet_g_signesubj_2,"</h5>")
      main_g_4 <- "<h4>Inspection :</h4>"
      main_g_5 <- paste0("<h5>Aspect (déformation, désaxation) : ",input$poignet_g_aspect_2,"</h5>")
      main_g_6 <- paste0("<h5>Cicatrice : ",input$poignet_g_cic_2,"</h5>")
      main_g_7 <- "<h4>Palpation :</h4>"
      main_g_8 <- paste0("<h5>Troubles vasomoteurs : ",paste0(input$poignet_g_vasomoteur_2,sep=", "),"</h5>")
      main_g_9 <- paste0("<h5>Cal hypertrophique : ",input$cal_poignet_g,"</h5>")
      main_g_10 <- "<h4>Mobilité du poignet gauche :</h4>"
      
      main_g_11 <- paste0("<h5>Flexion active : ",input$poignet_g_flexion_act," degrés</h5>")
      main_g_12 <- paste0("<h5>Extension active : ",input$poignet_g_extension_act," degrés</h5>")
      main_g_13 <- paste0("<h5>Abduction active (inclinaison radiale) : ",input$poignet_g_abd_act," degrés</h5>")
      main_g_14 <- paste0("<h5>Adduction active (inclinaison cubitale) : ",input$poignet_g_add_act," degrés</h5>")
      if(input$poignet_g_activate_passive=="Oui"){
        main_g_15 <- paste0("<h5>Flexion passive : ",input$poignet_g_flexion_pas," degrés</h5>")
        main_g_16 <- paste0("<h5>Extension passive : ",input$poignet_g_extension_pas," degrés</h5>")
        main_g_17 <- paste0("<h5>Abduction passive : ",input$poignet_g_abd_act," degrés</h5>")
        main_g_18 <- paste0("<h5>Adduction passive : ",input$poignet_g_add_act," degrés</h5>")
      }else{
        main_g_15 <- ""
        main_g_16 <- ""
        main_g_17 <- ""
        main_g_18 <- ""
      }
      main_g_19 <- paste0("<h5>Pronation (amplitude) : ",input$poignet_g_pronation_amplitude,"</h5>")
      main_g_20 <- paste0("<h5>Pronation (force) : ",input$poignet_g_pronation_force,"</h5>")
      main_g_21 <- paste0("<h5>Suppination (amplitude) : ",input$poignet_g_suppination_amplitude,"</h5>")
      main_g_22 <- paste0("<h5>Suppination (force) : ",input$poignet_g_suppination_force,"</h5>")
      main_g_23 <- "<h4>Mensurations à gauche en cm :</h4>"
      main_g_24 <- ifelse(input$mens_g0_pm>=0, paste0("<h5>Biceps (mesuré à 10cm du pli du coude) : ",input$mens_g0_pm," cm</h5>"),"")
      main_g_25 <- ifelse(input$mens_g1_pm>=0, paste0("<h5>Cône antébrachial (mesuré à 10cm du pli du coude) : ",input$mens_g1_pm," cm</h5>"),"")
      main_g_26 <- ifelse(input$mens_g2_pm>=0, paste0("<h5>Poignet : ",input$mens_g2_pm," cm</h5>"),"")
      main_g_27 <- ifelse(input$mens_g3_pm>=0, paste0("<h5>Gantier : ",input$mens_g3_pm," cm</h5>"),"")
      main_g_28 <- "<h4>Force de préhension à gauche (Jamar) :</h4>"
      main_g_29 <- ifelse(input$jamar_g1_pm>=0, paste0("<h5>Premier essai : ",input$jamar_g1_pm," cm</h5>"),"")
      main_g_30 <- ifelse(input$jamar_g2_pm>=0, paste0("<h5>Deuxième essai : ",input$jamar_g2_pm," cm</h5>"),"")
      main_g_31 <- ifelse(input$jamar_g3_pm>=0, paste0("<h5>Troisième essai : ",input$jamar_g3_pm," cm</h5>"),"")
      
      main_g_32 <- "<h3>Examen de la main gauche :</h3>"
      main_g_33 <- paste0("<h5>Pince pouce-index en forme : ",input$main_g_pince_pi_forme,"</h5>")
      main_g_34 <- paste0("<h5>Pince pouce-index en force : ",input$main_g_pince_pi_force,"</h5>")
      main_g_35 <- paste0("<h5>Pince pollicidigitale en forme : ",input$main_g_pince_ppd_forme,"</h5>")
      main_g_36 <- paste0("<h5>Pince pollicidigitale force : ",input$main_g_pince_ppd_force,"</h5>")
      main_g_37 <- paste0("<h5>",input$main_g_pince_ppd_obs,"</h5>")
      main_g_38 <- "<h4>Examen fonctionnel de la main gauche :</h4>"
      main_g_39 <- paste0("<h5>Pince unguéale (ramassage d'une allumette ou d'une épingle) : ",input$main_fonct_g_item1,"</h5>")
      main_g_40 <- paste0("<h5>Pince pulpo-pulpaire (plaquette de plastique) : ",input$main_fonct_g_item2,"</h5>")
      main_g_41 <- paste0("<h5>Pince pulpo-latérale (plaquette de plastique) : ",input$main_fonct_g_item3,"</h5>")
      main_g_42 <- paste0("<h5>Pince tripode (haut de la boîte cylindrique, manche d'outil, pinceau) : ",input$main_fonct_g_item4,"</h5>")
      main_g_43 <- paste0("<h5>Empaumement (boîte de conserves, manche, pinceau) : ",input$main_fonct_g_item5,"</h5>")
      main_g_44 <- paste0("<h5>Crochet (poignée) : ",input$main_fonct_g_item6,"</h5>")
      main_g_45 <- paste0("<h5>Prise sphérique (haut de la boîte cylindrique) : ",input$main_fonct_g_item7,"</h5>")
      main_g_46 <- "<h4>Examen des articulations de la main gauche :</h4>"
      main_g_47 <- ifelse(input$main_g_carpometacarp_1!="Pas de blocage", paste0("<h5>Blocage de la colonne du pouce articulaire ou extra-articulaire : ",input$main_g_carpometacarp_1,"</h5>"),"<h5>Blocage de la colonne du pouce articulaire ou extra-articulaire :</h5>")
      
      main_g_48 <- ifelse(input$main_g_metacarpophal_1!="Pas de blocage", paste0("<h5>Articulation métacarpo-phalangienne du pouce : ",input$main_g_metacarpophal_1,"</h5>"),"<h5>Articulation métacarpo-phalangienne du pouce conservée</h5>")
      main_g_49 <- ifelse(input$main_g_interphalangienne_1!="Normale", paste0("<h5>Articulation inter-phalangienne du pouce : ",input$main_g_interphalangienne_1,"</h5>"),"<h5>Articulation inter-phalangienne du pouce conservée</h5>")
      main_g_50 <- ifelse(input$main_g_index_1!="Normale", paste0("<h5>Mobilité de l'index : ",input$main_g_index_1,"</h5>"),"<h5>Mobilité de l'index normale</h5>")
      main_g_51 <- ifelse(input$main_g_medius_1!="Normale", paste0("<h5>Mobilité du medius : ",input$main_g_medius_1,"</h5>"),"<h5>Mobilité du medius normale</h5>")
      main_g_52 <- ifelse(input$main_g_annulaire_1!="Normale", paste0("<h5>Mobilité de l'annulaire : ",input$main_g_annulaire_1,"</h5>"),"<h5>Mobilité de l'annulaire normale</h5>")
      if(input$main_g_activate_articulations=='Altéré avec un équivalent-amputation'){
        main_g_53 <- paste0("<h5>Amputation d'une partie du pouce : ",input$amputation_pouce_g_12,"</h5>")
        main_g_54 <- paste0("<h5>Amputation d'une partie de l'index : ",input$amputation_index_g_12,"</h5>")
        main_g_55 <- paste0("<h5>Amputation d'une partie du medius : ",input$amputation_medius_g_12,"</h5>")
        main_g_56 <- paste0("<h5>Amputation d'une partie de l'annulaire : ",input$amputation_annulaire_g_12,"</h5>")
        main_g_57 <- paste0("<h5>Amputation d'une partie de l'auriculaire : ",input$amputation_auriculaire_g_12,"</h5>")
      }else{
        main_g_53 <- ""
        main_g_54 <- ""
        main_g_55 <- ""
        main_g_56 <- ""
        main_g_57 <- ""
      }
      synthese_g <- paste0(main_g_0,main_g_1,main_g_2,main_g_3,main_g_4,main_g_5,main_g_6,main_g_7,main_g_8,main_g_9,main_g_10,main_g_11,main_g_12,main_g_13,main_g_14,main_g_15,main_g_16,main_g_17,main_g_18,main_g_19,main_g_20,main_g_21,main_g_22,main_g_23,main_g_24,main_g_25,main_g_26,main_g_27,main_g_28,main_g_29,main_g_30,main_g_31,main_g_32,main_g_33,main_g_34,main_g_35,main_g_36,main_g_37,main_g_38,main_g_39,main_g_40,main_g_41,main_g_42,main_g_43,main_g_44,main_g_45,main_g_46,main_g_47,main_g_48,main_g_49,main_g_50,main_g_51,main_g_52,main_g_53,main_g_54,main_g_55,main_g_56,main_g_57)
      mains_synthese <- paste0(mains_synthese,synthese_g)
    }
    
    if(input$mainspoignets_activate %in% c("Oui à droite","Oui les deux") ){
      main_d_0 <- "<h3>Examen du poignet droit :</h3>"
      main_d_1 <- "<h4>Signes rapportés :</h4>"
      main_d_2 <- paste0("<h5>Gêne fonctionnelle : ",input$poignet_d_genefct_2,"</h5>")
      main_d_3 <- paste0("<h5>Signes subjectifs : ",input$poignet_d_signesubj_2,"</h5>")
      main_d_4 <- "<h4>Inspection :</h4>"
      main_d_5 <- paste0("<h5>Aspect (déformation, désaxation) : ",input$poignet_d_aspect_2,"</h5>")
      main_d_6 <- paste0("<h5>Cicatrice : ",input$poignet_d_cic_2,"</h5>")
      main_d_7 <- "<h4>Palpation :</h4>"
      main_d_8 <- paste0("<h5>Troubles vasomoteurs : ",paste0(input$poignet_d_vasomoteur_2,sep=", "),"</h5>")
      main_d_9 <- paste0("<h5>Cal hypertrophique : ",input$cal_poignet_d,"</h5>")
      main_d_10 <- "<h4>Mobilité du poignet droit :</h4>"
      
      main_d_11 <- paste0("<h5>Flexion active : ",input$poignet_d_flexion_act," degrés</h5>")
      main_d_12 <- paste0("<h5>Extension active : ",input$poignet_d_extension_act," degrés</h5>")
      main_d_13 <- paste0("<h5>Abduction active (inclinaison radiale) : ",input$poignet_d_abd_act," degrés</h5>")
      main_d_14 <- paste0("<h5>Adduction active (inclinaison cubitale) : ",input$poignet_d_add_act," degrés</h5>")
      if(input$poignet_d_activate_passive=="Oui"){
        main_d_15 <- paste0("<h5>Flexion passive : ",input$poignet_d_flexion_pas," degrés</h5>")
        main_d_16 <- paste0("<h5>Extension passive : ",input$poignet_d_extension_pas," degrés</h5>")
        main_d_17 <- paste0("<h5>Abduction passive : ",input$poignet_d_abd_act," degrés</h5>")
        main_d_18 <- paste0("<h5>Adduction passive : ",input$poignet_d_add_act," degrés</h5>")
      }else{
        main_d_15 <- ""
        main_d_16 <- ""
        main_d_17 <- ""
        main_d_18 <- ""
      }
      main_d_19 <- paste0("<h5>Pronation (amplitude) : ",input$poignet_d_pronation_amplitude,"</h5>")
      main_d_20 <- paste0("<h5>Pronation (force) : ",input$poignet_d_pronation_force,"</h5>")
      main_d_21 <- paste0("<h5>Suppination (amplitude) : ",input$poignet_d_suppination_amplitude,"</h5>")
      main_d_22 <- paste0("<h5>Suppination (force) : ",input$poignet_d_suppination_force,"</h5>")
      main_d_23 <- "<h4>Mensurations à droite en cm :</h4>"
      main_d_24 <- ifelse(input$mens_d0_pm>=0, paste0("<h5>Biceps (mesuré à 10cm du pli du coude) : ",input$mens_d0_pm," cm</h5>"),"")
      main_d_25 <- ifelse(input$mens_d1_pm>=0, paste0("<h5>Cône antébrachial (mesuré à 10cm du pli du coude) : ",input$mens_d1_pm," cm</h5>"),"")
      main_d_26 <- ifelse(input$mens_d2_pm>=0, paste0("<h5>Poignet : ",input$mens_d2_pm," cm</h5>"),"")
      main_d_27 <- ifelse(input$mens_d3_pm>=0, paste0("<h5>Gantier : ",input$mens_d3_pm," cm</h5>"),"")
      main_d_28 <- "<h4>Force de préhension à gauche (Jamar) :</h4>"
      main_d_29 <- ifelse(input$jamar_d1_pm>=0, paste0("<h5>Premier essai : ",input$jamar_d1_pm," cm</h5>"),"")
      main_d_30 <- ifelse(input$jamar_d2_pm>=0, paste0("<h5>Deuxième essai : ",input$jamar_d2_pm," cm</h5>"),"")
      main_d_31 <- ifelse(input$jamar_d3_pm>=0, paste0("<h5>Troisième essai : ",input$jamar_d3_pm," cm</h5>"),"")
      
      main_d_32 <- "<h3>Examen de la main droite :</h3>"
      main_d_33 <- paste0("<h5>Pince pouce-index en forme : ",input$main_d_pince_pi_forme,"</h5>")
      main_d_34 <- paste0("<h5>Pince pouce-index en force : ",input$main_d_pince_pi_force,"</h5>")
      main_d_35 <- paste0("<h5>Pince pollicidigitale en forme : ",input$main_d_pince_ppd_forme,"</h5>")
      main_d_36 <- paste0("<h5>Pince pollicidigitale force : ",input$main_d_pince_ppd_force,"</h5>")
      main_d_37 <- paste0("<h5>",input$main_d_pince_ppd_obs,"</h5>")
      main_d_38 <- "<h4>Examen fonctionnel de la main droite :</h4>"
      main_d_39 <- paste0("<h5>Pince unguéale (ramassage d'une allumette ou d'une épingle) : ",input$main_fonct_d_item1,"</h5>")
      main_d_40 <- paste0("<h5>Pince pulpo-pulpaire (plaquette de plastique) : ",input$main_fonct_d_item2,"</h5>")
      main_d_41 <- paste0("<h5>Pince pulpo-latérale (plaquette de plastique) : ",input$main_fonct_d_item3,"</h5>")
      main_d_42 <- paste0("<h5>Pince tripode (haut de la boîte cylindrique, manche d'outil, pinceau) : ",input$main_fonct_d_item4,"</h5>")
      main_d_43 <- paste0("<h5>Empaumement (boîte de conserves, manche, pinceau) : ",input$main_fonct_d_item5,"</h5>")
      main_d_44 <- paste0("<h5>Crochet (poignée) : ",input$main_fonct_d_item6,"</h5>")
      main_d_45 <- paste0("<h5>Prise sphérique (haut de la boîte cylindrique) : ",input$main_fonct_d_item7,"</h5>")
      main_d_46 <- "<h4>Examen des articulations de la main droite :</h4>"
      main_d_47 <- ifelse(input$main_d_carpometacarp_1!="Pas de blocage", paste0("<h5>Blocage de la colonne du pouce articulaire ou extra-articulaire : ",input$main_d_carpometacarp_1,"</h5>"),"<h5>Blocage de la colonne du pouce articulaire ou extra-articulaire :</h5>")
      
      main_d_48 <- ifelse(input$main_d_metacarpophal_1!="Pas de blocage", paste0("<h5>Articulation métacarpo-phalangienne du pouce : ",input$main_d_metacarpophal_1,"</h5>"),"<h5>Articulation métacarpo-phalangienne du pouce conservée</h5>")
      main_d_49 <- ifelse(input$main_d_interphalangienne_1!="Normale", paste0("<h5>Articulation inter-phalangienne du pouce : ",input$main_d_interphalangienne_1,"</h5>"),"<h5>Articulation inter-phalangienne du pouce conservée</h5>")
      main_d_50 <- ifelse(input$main_d_index_1!="Normale", paste0("<h5>Mobilité de l'index : ",input$main_d_index_1,"</h5>"),"<h5>Mobilité de l'index normale</h5>")
      main_d_51 <- ifelse(input$main_d_medius_1!="Normale", paste0("<h5>Mobilité du medius : ",input$main_d_medius_1,"</h5>"),"<h5>Mobilité du medius normale</h5>")
      main_d_52 <- ifelse(input$main_d_annulaire_1!="Normale", paste0("<h5>Mobilité de l'annulaire : ",input$main_d_annulaire_1,"</h5>"),"<h5>Mobilité de l'annulaire normale</h5>")
      if(input$main_d_activate_articulations=='Altéré avec un équivalent-amputation'){
        main_d_53 <- paste0("<h5>Amputation d'une partie du pouce : ",input$amputation_pouce_d_12,"</h5>")
        main_d_54 <- paste0("<h5>Amputation d'une partie de l'index : ",input$amputation_index_d_12,"</h5>")
        main_d_55 <- paste0("<h5>Amputation d'une partie du medius : ",input$amputation_medius_d_12,"</h5>")
        main_d_56 <- paste0("<h5>Amputation d'une partie de l'annulaire : ",input$amputation_annulaire_d_12,"</h5>")
        main_d_57 <- paste0("<h5>Amputation d'une partie de l'auriculaire : ",input$amputation_auriculaire_d_12,"</h5>")
      }else{
        main_d_53 <- ""
        main_d_54 <- ""
        main_d_55 <- ""
        main_d_56 <- ""
        main_d_57 <- ""
      }
      synthese_d <- paste0(main_d_0,main_d_1,main_d_2,main_d_3,main_d_4,main_d_5,main_d_6,main_d_7,main_d_8,main_d_9,main_d_10,main_d_11,main_d_12,main_d_13,main_d_14,main_d_15,main_d_16,main_d_17,main_d_18,main_d_19,main_d_20,main_d_21,main_d_22,main_d_23,main_d_24,main_d_25,main_d_26,main_d_27,main_d_28,main_d_29,main_d_30,main_d_31,main_d_32,main_d_33,main_d_34,main_d_35,main_d_36,main_d_37,main_d_38,main_d_39,main_d_40,main_d_41,main_d_42,main_d_43,main_d_44,main_d_45,main_d_46,main_d_47,main_d_48,main_d_49,main_d_50,main_d_51,main_d_52,main_d_53,main_d_54,main_d_55,main_d_56,main_d_57)
      mains_synthese <- paste0(mains_synthese,synthese_d)
    }
    
    
    HTML(mains_synthese)
  
  })
  
  # Synthese - Examen du rachis cervical ####
  
  output$cervical_synthese <- renderUI({
    
    cervical_synthese <- ""
    
    if(input$cervical_activate=="Oui"){
      rachiscerv_text_0 <- "<h3>Examen du rachis cervical : </h3>"  
      rachiscerv_text_1 <- paste0("<h5>Cicatrices : ",input$cervical_cic,"</h5>")
      rachiscerv_text_2 <- paste0("<h5>",input$cervical_obs,"</h5>")
      rachiscerv_text_3 <- "<h4>Mobilité sagittale et gêne fonctionnelle :</h4>"
      rachiscerv_text_4 <- paste0("<h5>Persistance de douleurs et gêne fonctionnelle : ",input$cervical_fonction,"</h5>")
      rachiscerv_text_5 <- ifelse(input$cervical_flexion_cm>=0,paste0("<h5>Flexion (Distance menton-sternum en cm) : ",input$cervical_flexion_cm,"</h5>"),"")
      rachiscerv_text_6 <- ifelse(input$cervical_extension_cm>=0,paste0("<h5>Extension (Distance menton-sternum en cm) : ",input$cervical_extension_cm,"</h5>"),"")
      rachiscerv_text_7 <- ifelse(input$cervical_rotation_G>=0,paste0("<h5>Rotation à gauche : ",input$cervical_rotation_G," degrés</h5>"),"")
      rachiscerv_text_8 <- ifelse(input$cervical_Inclinaison_G>=0,paste0("<h5>Inclinaison à gauche : ",input$cervical_Inclinaison_G," degrés</h5>"),"")
      rachiscerv_text_9 <- ifelse(input$cervical_rotation_D>=0,paste0("<h5>Rotation à droite : ",input$cervical_rotation_D," degrés</h5>"),"")
      rachiscerv_text_10 <- ifelse(input$cervical_Inclinaison_D>=0,paste0("<h5>Inclinaison à droite : ",input$cervical_Inclinaison_D," degrés</h5>"),"")
      rachiscerv_text_11 <- "<h4>Examen neurologique :</h4>"
      rachiscerv_text_12 <- paste0("<h5>Syndrome cervico-céphalique : ",input$cervicoceph,"</h5>")
      rachiscerv_text_13 <- paste0("<h5>",paste0(input$cervicoceph_a, collapse = ", "),"</h5>")
      rachiscerv_text_14 <- paste0("<h5>Déficit sensitif à Gauche : ",paste0(input$deficit_neuro_sensitif_ms_g, collapse = ", "),"</h5>")
      rachiscerv_text_15 <- paste0("<h5>Déficit sensitif à Droite : ",paste0(input$deficit_neuro_sensitif_ms_D, collapse = ", "),"</h5>")
      rachiscerv_text_16 <- paste0("<h5>Déficit moteur à Gauche : ",paste0(input$deficit_neuro_moteur_ms_g, collapse = ", "),"</h5>")
      rachiscerv_text_17 <- paste0("<h5>Déficit moteur à Droite : ",paste0(input$deficit_neuro_moteur_ms_D, collapse = ", "),"</h5>")
      
      cervical_synthese <- paste0(rachiscerv_text_0,rachiscerv_text_1,rachiscerv_text_2,rachiscerv_text_3,rachiscerv_text_4,rachiscerv_text_5,rachiscerv_text_6,rachiscerv_text_7,rachiscerv_text_8,rachiscerv_text_9,rachiscerv_text_10,rachiscerv_text_11,rachiscerv_text_12,rachiscerv_text_13,rachiscerv_text_14,rachiscerv_text_15,rachiscerv_text_16,rachiscerv_text_17)
      }
    
    HTML(cervical_synthese)
  })
  
  # Synthese - Examen du rachis et de la mobilité ####
  
  output$rachisdl_synthese <- renderUI({
    if(input$rachisdl_activate=="Oui"){
      rachisdl_text_0 <- "<h3>Examen du rachis dorsolombaire et de la mobilité : </h3>"  
      rachisdl_text_1 <- "<h4>Examen de la mobilité :</h4>"
      rachisdl_text_2 <- paste0("<h5>Marche : ",input$marche_1,"</h5>")
      rachisdl_text_3 <- paste0("<h5>Marche sur les talons : ",input$marche_2,"</h5>")
      rachisdl_text_4 <- paste0("<h5>Marche sur les pointes des pieds : ",input$marche_3,"</h5>")
      rachisdl_text_5 <- paste0("<h5>Accroupissement : ",input$accroupissement,"</h5>")
      rachisdl_text_6 <- paste0("<h5>Appui monopodal à droite : ",input$appuimono_D,"</h5>")
      rachisdl_text_7 <- paste0("<h5>Appui monopodal à gauche : ",input$appuimono_G,"</h5>")
      rachisdl_text_8 <- "<h4>Examen du rachis :</h4>"
      rachisdl_text_9 <- paste0("<h5>Allongement : ",input$allonge,"</h5>")
      rachisdl_text_10 <- paste0("<h5>Equerre : ",input$equerre,"</h5>")
      rachisdl_text_11 <- ifelse(input$schober_none==T,"<h5>Indice de Schober non mesurable</h5>", 
                                 ifelse(input$schober>0,paste0("<h5>Indice de Schober = 15/",input$schober,"cm</h5>"),
                                        "<h5>Indice de Schober non mesuré</h5>")
      )
      rachisdl_text_12 <- ifelse(input$dms_none==T,"<h5>Distance mains-sol non mesurable</h5>", 
                                 ifelse(input$dms>0,paste0("<h5>Distance mains-sol = ",input$dms,"cm</h5>"),
                                        "<h5>Distance mains-sol non mesurée</h5>")
      )
      rachisdl_text_13 <- "<h4>Examen neurologique :</h4>"
      rachisdl_text_14 <- ifelse(input$tb_sphincter=="Aucun","<h5>Pas de trouble sphinctérien</h5>",paste0("<h5>Troubles spinctériens : ", paste0(input$tb_sphincter, collapse=", "),"</h5>" ) )
      rachisdl_text_15 <- ifelse(input$rotulien=="Vifs et symétriques","<h5>Réflexes rotuliens vifs et symétriques</h5>",paste0("<h5>Réflexes rotuliens : ", paste0(input$rotulien, collapse=", "),"</h5>" ) )
      rachisdl_text_16 <- ifelse(input$achileen=="Vifs et symétriques","<h5>Réflexes achiléen vifs et symétriques</h5>",paste0("<h5>Réflexes achiléens : ", paste0(input$achileen, collapse=", "),"</h5>" ) )
      rachisdl_text_17 <- paste0("<h5>", input$obs_neuro,"</h5>" )
      rachisdl_text_18 <- "<h4>Examen neurologique à gauche :</h4>"
      rachisdl_text_19 <- ifelse(input$sciatalgies_g=="Aucune","<h5>Pas de sciatalgie à gauche</h5>",paste0("<h5>Sciatalgies à gauche : ", paste0(input$sciatalgies_g, collapse=", "),"</h5>" ) )
      rachisdl_text_20 <- ifelse(input$deficitsens_g=="Aucune","<h5>Pas de déficit sensitif à gauche</h5>",paste0("<h5>Déficit sensitif à gauche : ", paste0(input$deficitsens_g, collapse=", "),"</h5>" ) )
      rachisdl_text_21 <- ifelse(input$deficit_pied_g1=="Non","<h5>Pas déficit des releveurs du pied gauche</h5>",paste0("<h5>Déficit des releveurs du pied gauche : ", paste0(input$deficit_pied_g1, collapse=", "),"</h5>" ) )
      rachisdl_text_22 <- ifelse(input$deficit_pied_g2=="Non","<h5>Pas déficit des extenseurs du pied gauche</h5>",paste0("<h5>Déficit des extenseurs du pied gauche : ", paste0(input$deficit_pied_g2, collapse=", "),"</h5>" ) )
      rachisdl_text_23 <- ifelse(input$snp_mi_g_glob=="Normal","<h5>Pas d'atteinte objectif d'un nerf périphérique au membre inférieur gauche</h5>",
                                 paste0("<h5> Atteinte nerveuse à gauche : ",paste0(c(input$snp_mi_g1,input$snp_mi_g2),collapse = "; "),"</h5>") )
      rachisdl_text_24 <- paste0("<h5>", input$obs_neuro_g,"</h5>" )
      rachisdl_text_24b <- paste0("<h5>", input$snp_mi_obs_g,"</h5>" )
      rachisdl_text_25 <- "<h4>Examen neurologique à droite :</h4>"
      rachisdl_text_26 <- ifelse(input$sciatalgies_d=="Aucune","<h5>Pas de sciatalgie à droite</h5>",paste0("<h5>Sciatalgies à droite : ", paste0(input$sciatalgies_d, collapse=", "),"</h5>" ) )
      rachisdl_text_27 <- ifelse(input$deficitsens_d=="Aucune","<h5>Pas de déficit sensitif à gauche</h5>",paste0("<h5>Déficit sensitif à droite : ", paste0(input$deficitsens_d, collapse=", "),"</h5>" ) )
      rachisdl_text_28 <- ifelse(input$deficit_pied_d1=="Non","<h5>Pas déficit des releveurs du pied droit</h5>",paste0("<h6>Déficit des releveurs du pied droit : ", paste0(input$deficit_pied_d1, collapse=", "),"</h5>" ) )
      rachisdl_text_29 <- ifelse(input$deficit_pied_d2=="Non","<h5>Pas déficit des extenseurs du pied droit</h5>",paste0("<h6>Déficit des extenseurs du pied droit : ", paste0(input$deficit_pied_d2, collapse=", "),"</h5>" ) )
      rachisdl_text_30 <- ifelse(input$snp_mi_d_glob=="Normal","<h5>Pas d'atteinte objectif d'un nerf périphérique au membre inférieur droit</h5>",
                                 paste0("<h5> Atteinte nerveuse à gauche : ",paste0(c(input$snp_mi_d1,input$snp_mi_d2),collapse = "; "),"</h5>") )
      rachisdl_text_31 <- paste0("<h5>", input$obs_neuro,"</h5>" )
      rachisdl_text_31b <- paste0("<h5>", input$snp_mi_obs_d,"</h5>" )
      
      # concaténation des chaînes de caractères pour la synthèse du rachis dorsolombaire
      rachisdl_synthese <- paste(rachisdl_text_0,rachisdl_text_1,rachisdl_text_2,rachisdl_text_3,rachisdl_text_4,rachisdl_text_5,
                                 rachisdl_text_6,rachisdl_text_7,rachisdl_text_8,rachisdl_text_9,rachisdl_text_10,
                                 rachisdl_text_11,rachisdl_text_12,rachisdl_text_13,rachisdl_text_14,rachisdl_text_15,rachisdl_text_16,rachisdl_text_17,rachisdl_text_18,
                                 rachisdl_text_19,rachisdl_text_20,rachisdl_text_21,rachisdl_text_22,rachisdl_text_23,rachisdl_text_24,rachisdl_text_24b,
                                 rachisdl_text_25,rachisdl_text_26,rachisdl_text_27,rachisdl_text_28,rachisdl_text_29,rachisdl_text_30,rachisdl_text_31,
                                 rachisdl_text_31b)
      
    }else{
      rachisdl_synthese <- "" 
    }
    HTML(rachisdl_synthese)
  })
  # Synthèse - Examen Hanches et bassin ####
  
  output$hanchesbasssin_synthese <- renderUI({
    
    hanchebassin_synthese <- ""
    if( !any("Non"==input$hanchesbassin_activate) ){
      hb_text_0 <- "<h3>Examen de la mobilité : </h3>"
      hb_text_1 <- paste0("<h5>Marche spontanée : ", input$hanches_marche_1,"</h5>" )
      hb_text_2 <- paste0("<h5>Marche sur les talons : ", input$hanches_marche_2,"</h5>" )
      hb_text_3 <- paste0("<h5>Marche sur les pointes : ", input$hanches_marche_3,"</h5>" )
      hb_text_4 <- paste0("<h5>Accroupissement : ", input$hanches_accroupissement,"</h5>" )
      hb_text_5 <- paste0("<h5>Appui monopodal à Droite : ", input$hanches_appuimono_D,"</h5>" )
      hb_text_6 <- paste0("<h5>Appui monopodal à Gauche : ", input$hanches_appuimono_G,"</h5>" )
      
      hanchebassin_synthese <- paste0(hanchebassin_synthese,hb_text_0,hb_text_1,hb_text_2,hb_text_3,hb_text_4,hb_text_5,hb_text_6)
    }
    
    if( any("Hanche gauche"==input$hanchesbassin_activate) ){
      hb_text_0 <- "<h3>Examen de la hanche gauche : </h3>"
      hb_text_1 <- "<h4>Inpsection de la hanche gauche : </h4>"
      hb_text_2 <- paste0("<h5>Cicatrice : ",input$hanche_g_cicatrice,"</h5>")
      hb_text_3 <- paste0("<h5>Attitude vicieuse : ",input$hanche_g_attitude,"</h5>")
      hb_text_4 <- ifelse(input$hanche_g_raccourcissement>0,paste0("<h5>Raccourcissement du membre inférieur : ",input$hanche_g_raccourcissement,"</h5>"),"<h5>Pas de raccourcissement du membre inférieur gauche")
      hb_text_5 <- paste0("<h5>Cal hypertrophique : ",input$hanche_g_cal,"</h5>")
      hb_text_6 <- "<h4>Mobilités de la hanche gauche : </h4>"
      hb_text_7 <- ifelse(input$hanche_g_flexion>=0,paste0("<h5>Flexion",input$hanche_g_flexion," degrés </h5>"), "<h5>Mobilité de la flexion de la hanche gauche non évaluée</h5>")
      hb_text_8 <- ifelse(input$hanche_g_extension>=0,paste0("<h5>Extension",input$hanche_g_extension," degrés </h5>"), "<h5>Mobilité de la hanche gauche non évaluée</h5>")
      hb_text_9 <- ifelse(input$hanche_g_hyperextension>=0,paste0("<h5>Hyperextension",input$hanche_g_hyperextension," degrés </h5>"), "<h5>Mobilité de l'hyperextension de la hanche gauche non évaluée</h5>")
      hb_text_10 <- ifelse(input$hanche_g_rot_int>=0,paste0("<h5>Rotation interne",input$hanche_g_rot_int," degrés </h5>"), "<h5>Mobilité de la rotation interne de la hanche gauche non évaluée</h5>")
      hb_text_11 <- ifelse(input$hanche_g_rot_ext>=0,paste0("<h5>Rotation externe",input$hanche_g_rot_ext," degrés </h5>"), "<h5>Mobilité de la rotation externe de la hanche gauche non évaluée</h5>")
      hb_text_12 <- ifelse(input$hanche_g_abduction>=0,paste0("<h5>Abduction",input$hanche_g_abduction," degrés </h5>"), "<h5>Mobilité de l'abduction de la hanche gauche non évaluée</h5>")
      hb_text_13 <- ifelse(input$hanche_g_adduction>=0,paste0("<h5>Adduction",input$hanche_g_adduction," degrés </h5>"), "<h5>Mobilité de l'adduction de la hanche gauche non évaluée</h5>")
      hb_text_14 <- "<h4>Mensurations du membre inférieur gauche : </h4>"
      hb_text_15 <- paste0("<h5>",input$hanche_g_obs,"</h5>")
      hb_text_16 <- ifelse(input$hanche_mens_g1>=0,paste0("<h5>Cuisse (mesuré à 15 cm du sommet rotulien) : ",input$hanche_mens_g1," degrés </h5>"), "")
      hb_text_17 <- ifelse(input$hanche_mens_g2>=0,paste0("<h5>Mollet (mesuré à 10 cm de la pointe de la rotule) : ",input$hanche_mens_g2," degrés </h5>"), "")
      hanchebassin_synthese <- paste0(hanchebassin_synthese,hb_text_0,hb_text_1,hb_text_2,hb_text_3,hb_text_4,hb_text_5,hb_text_6,hb_text_7,hb_text_8,hb_text_9,hb_text_10,hb_text_11,hb_text_12,hb_text_13,hb_text_14,hb_text_15,hb_text_16,hb_text_17)
    }
    
    if( any("Hanche droite"==input$hanchesbassin_activate) ){
      hb_text_0 <- "<h3>Examen de la hanche droite : </h3>"
      hb_text_1 <- "<h4>Inpsection de la hanche droite : </h4>"
      hb_text_2 <- paste0("<h5>Cicatrice : ",input$hanche_d_cicatrice,"</h5>")
      hb_text_3 <- paste0("<h5>Attitude vicieuse : ",input$hanche_d_attitude,"</h5>")
      hb_text_4 <- ifelse(input$hanche_d_raccourcissement>0,paste0("<h5>Raccourcissement du membre inférieur : ",input$hanche_d_raccourcissement,"</h5>"),"<h5>Pas de raccourcissement du membre inférieur droit")
      hb_text_5 <- paste0("<h5>Cal hypertrophique : ",input$hanche_d_cal,"</h5>")
      hb_text_6 <- "<h4>Mobilités de la hanche droite : </h4>"
      hb_text_7 <- ifelse(input$hanche_d_flexion>=0,paste0("<h5>Flexion",input$hanche_d_flexion," degrés </h5>"), "<h5>Mobilité de la flexion de la hanche droite non évaluée</h5>")
      hb_text_8 <- ifelse(input$hanche_d_extension>=0,paste0("<h5>Extension",input$hanche_d_extension," degrés </h5>"), "<h5>Mobilité de la hanche droite non évaluée</h5>")
      hb_text_9 <- ifelse(input$hanche_d_hyperextension>=0,paste0("<h5>Hyperextension",input$hanche_d_hyperextension," degrés </h5>"), "<h5>Mobilité de l'hyperextension de la hanche droite non évaluée</h5>")
      hb_text_10 <- ifelse(input$hanche_d_rot_int>=0,paste0("<h5>Rotation interne",input$hanche_d_rot_int," degrés </h5>"), "<h5>Mobilité de la rotation interne de la hanche droite non évaluée</h5>")
      hb_text_11 <- ifelse(input$hanche_d_rot_ext>=0,paste0("<h5>Rotation externe",input$hanche_d_rot_ext," degrés </h5>"), "<h5>Mobilité de la rotation externe de la hanche droite non évaluée</h5>")
      hb_text_12 <- ifelse(input$hanche_d_abduction>=0,paste0("<h5>Abduction",input$hanche_d_abduction," degrés </h5>"), "<h5>Mobilité de l'abduction de la hanche droite non évaluée</h5>")
      hb_text_13 <- ifelse(input$hanche_d_adduction>=0,paste0("<h5>Adduction",input$hanche_d_adduction," degrés </h5>"), "<h5>Mobilité de l'adduction de la hanche droite non évaluée</h5>")
      hb_text_14 <- "<h4>Mensurations du membre inférieur droite : </h4>"
      hb_text_15 <- paste0("<h5>",input$hanche_d_obs,"</h5>")
      hb_text_16 <- ifelse(input$hanche_mens_d1>=0,paste0("<h5>Cuisse (mesuré à 15 cm du sommet rotulien) : ",input$hanche_mens_d1," degrés </h5>"), "")
      hb_text_17 <- ifelse(input$hanche_mens_d2>=0,paste0("<h5>Mollet (mesuré à 10 cm de la pointe de la rotule) : ",input$hanche_mens_d2," degrés </h5>"), "")
      hanchebassin_synthese <- paste0(hanchebassin_synthese,hb_text_0,hb_text_1,hb_text_2,hb_text_3,hb_text_4,hb_text_5,hb_text_6,hb_text_7,hb_text_8,hb_text_9,hb_text_10,hb_text_11,hb_text_12,hb_text_13,hb_text_14,hb_text_15,hb_text_16,hb_text_17)
    }
    
    if( any("Bassin"==input$hanchesbassin_activate) ){
      hb_text_0 <- "<h3>Examen du bassin : </h3>"
      
    if(input$bassin_activate=="Normal"){
      hb_text_1 <- "<h5>Examen du bassin normal</h5>"
    }else{
      hb_text_2 <- paste0("<h5>Disjonction de la symphyse pubienne : ",input$bassin_disjoncion_symphyse,"</h5>")
      hb_text_3 <- paste0("<h5>Diastasis (entraînant une mobilité anormale du sacrum, avec retentissement sur la marche, accroupissement impossible, sacralgies) : ",input$bassin_diastasis_sacroi,"</h5>")
      hb_text_4 <- paste0("<h5>Arthropathie sacro-iliaque douloureuse chronique d'origine traumatique : ",input$bassin_arthro_sacroi,"</h5>")
      hb_text_4 <- paste0("<h5>Fracture du sacrum : ",input$bassin_fx_sacrum,"</h5>")
      hb_text_5 <- paste0("<h5>Coccygodynie : ",input$bassin_coccygodynie,"</h5>")
      
      hb_text_1 <- paste0(hb_text_2,hb_text_3,hb_text_4,hb_text_5)
    }
      hanchebassin_synthese <- paste0(hanchebassin_synthese,hb_text_0,hb_text_1)
    }
    
    
    HTML(hanchebassin_synthese)
  })
  
  # Syntèse - Examen des genoux ####
  
  output$genoux_synthese <- renderUI({
    
    genoux_synthese <- ""
    
    if( !("Non" %in% input$genoux_activate) ){
      
      genoux_text_0 <- "<h3>Analyse de la mobilité<h3>"
      
      genoux_text_1 <- paste0("<h5>Marche spontanée : ",input$genoux_marche_1,"</h5>")
      genoux_text_2 <- paste0("<h5>Marche sur les talons : ",input$genoux_marche_2,"</h5>")
      genoux_text_3 <- paste0("<h5>Marche sur les pointes : ",input$genoux_marche_3,"</h5>")
      genoux_text_4 <- paste0("<h5>Accroupissement : ",input$genoux_accroupissement,"</h5>")
      genoux_text_5 <- paste0("<h5>Appui monopodal à Droite : ",input$genoux_appuimono_D,"</h5>")
      genoux_text_6 <- paste0("<h5>Appui monopodal à Gauche : ",input$genoux_appuimono_G,"</h5>")
      genoux_text_7 <- paste0("<h5>Sautillement unipodal à Droite : ",input$genoux_sautmono_D,"</h5>")
      genoux_text_8 <- paste0("<h5>Sautillement unipodal à Gauche : ",input$genoux_sautmono_G,"</h5>")
      
      genoux_synthese <- paste0(genoux_synthese, genoux_text_0, genoux_text_1, genoux_text_2, genoux_text_3, genoux_text_4, genoux_text_5, genoux_text_6, genoux_text_7, genoux_text_8)
        
      if(input$genoux_activate %in% c("Oui à gauche","Oui les deux") ){
        
        genoux_text_0 <- "<h4>Examen du genou gauche :</h4>"
        genoux_text_1 <- "<h5>Inspection du genou gauche :</h5>"
        genoux_text_2 <- paste0("<h5>Cicatrice : ",input$genou_g_cic,"</h5>")
        genoux_text_3 <- paste0("<h5>Troubles circulatoires et vaso-moteurs : ",paste0(input$genou_g_tbcirculatoire,collapse = ", "),"</h5>")
        genoux_text_4 <- ifelse(input$genou_g_genuvarum==0,"<h5>Pas de genu varum</h5>", paste0("<h5>Genu varum à ", input$genou_g_genuvarum ," degrés</h5>") )
        genoux_text_5 <- ifelse(input$genou_g_genuvalgum==0,"<h5>Pas de genu valgum</h5>", paste0("<h5>Genu vajgum à ", input$genou_g_genuvalgum ," degrés</h5>") )
        genoux_text_6 <- ifelse(input$genou_g_raccourcissement==0,"<h5>Pas de raccourcissement du membre inférieur gauche<h5>", paste0("<h5>Raccourcissement du membre inférieur gauche à ", input$genou_g_raccourcissement ," cm<h5>") )
        genoux_text_7 <- ifelse(input$genou_g_flessum==0,"<h5>Pas de flessum</h5>",  paste0("<h5>Flessum à ", input$genou_g_flessum ," degrés<h5>") )
        genoux_text_8 <- "<h5>Mobilité du genou gauche :</h5>"
        
        if(input$genou_g_mobilite_activate=="Normale"){
          genoux_text_9 <- "<h5>Mobilité du genou gauche normale</h5>"
        }else{
          if(input$genou_g_mobilite_activate=="Blocage"){
            genoux_text_1 <- paste0("<h5>Angle de blocage : ",input$genou_g_blocage_angle,"</h5>")
            genoux_text_2 <- paste0("<h5>",input$genou_g_blocage_obs,"</h5>")
            genoux_text_9 <- paste0(genoux_text_1,genoux_text_2)
          }else{
            if(input$genou_g_mobilite_activate=="Limitation"){
              genoux_text_1 <- paste0("<h5>",input$genou_g_limitation_obs,"</h5>")
              genoux_text_2 <- paste0("<h5>Déficit de l'extension : ",input$genou_g_limitationext_angle,"</h5>")
              genoux_text_3 <- paste0("<h5>Déficit de la flexion : ",input$genou_g_limitationflex_angle,"</h5>")
              genoux_text_9 <- paste0(genoux_text_1,genoux_text_2,genoux_text_3)
            }
          }
        }
        if(input$genou_g_rotule_activate=="Normal"){
          genoux_text_10 <- "<h5>Examen de la rotule normal</h5>"
        }else{
          genoux_text_1 <- paste0("<h5>",input$genou_g_rotul_obs,"</h5>")
          genoux_text_2 <- paste0("<h5>",input$genou_g_rotule_exam,"</h5>")
          genoux_text_10 <- paste0(genoux_text_1,genoux_text_2)
        }
        
        genoux_synthese <- paste0(genoux_synthese, genoux_text_0, genoux_text_1, genoux_text_2, genoux_text_3, genoux_text_4, genoux_text_5, genoux_text_6, genoux_text_7, genoux_text_8,genoux_text_9,genoux_text_10)
      }
      
      if(input$genoux_activate %in% c("Oui à droite","Oui les deux") ){
        
        genoux_text_0 <- "<h4>Examen du genou droit :</h4>"
        genoux_text_1 <- "<h5>Inspection du genou droit :</h5>"
        genoux_text_2 <- paste0("<h5>Cicatrice : ",input$genou_d_cic,"</h5>")
        genoux_text_3 <- paste0("<h5>Troubles circulatoires et vaso-moteurs : ",paste0(input$genou_d_tbcirculatoire,collapse = ", "),"</h5>")
        genoux_text_4 <- ifelse(input$genou_d_genuvarum==0,"<h5>Pas de genu varum</h5>", paste0("<h5>Genu varum à ", input$genou_d_genuvarum ," degrés</h5>") )
        genoux_text_5 <- ifelse(input$genou_d_genuvalgum==0,"<h5>Pas de genu valgum</h5>", paste0("<h5>Genu vajgum à ", input$genou_d_genuvalgum ," degrés</h5>") )
        genoux_text_6 <- ifelse(input$genou_d_raccourcissement==0,"<h5>Pas de raccourcissement du membre inférieur gauche<h5>", paste0("<h5>Raccourcissement du membre inférieur gauche à ", input$genou_d_raccourcissement ," cm<h5>") )
        genoux_text_7 <- ifelse(input$genou_d_flessum==0,"<h5>Pas de flessum</h5>",  paste0("<h5>Flessum à ", input$genou_d_flessum ," degrés<h5>") )
        genoux_text_8 <- "<h5>Mobilité du genou droit :</h5>"
      
        if(input$genou_g_mobilite_activate=="Normale"){
          genoux_text_9 <- "<h5>Mobilité du genou droit normale</h5>"
        }else{
          if(input$genou_g_mobilite_activate=="Blocage"){
            genoux_text_1 <- paste0("<h5>Angle de blocage : ",input$genou_d_blocage_angle,"</h5>")
            genoux_text_2 <- paste0("<h5>",input$genou_d_blocage_obs,"</h5>")
            genoux_text_9 <- paste0(genoux_text_1,genoux_text_2)
          }else{
            if(input$genou_g_mobilite_activate=="Limitation"){
              genoux_text_1 <- paste0("<h5>",input$genou_d_limitation_obs,"</h5>")
              genoux_text_2 <- paste0("<h5>Déficit de l'extension : ",input$genou_d_limitationext_angle,"</h5>")
              genoux_text_3 <- paste0("<h5>Déficit de la flexion : ",input$genou_d_limitationflex_angle,"</h5>")
              genoux_text_9 <- paste0(genoux_text_1,genoux_text_2,genoux_text_3)
            }
          }
        }
        if(input$genou_d_rotule_activate=="Normal"){
          genoux_text_10 <- "<h5>Examen de la rotule normal</h5>"
        }else{
          genoux_text_1 <- paste0("<h5>",input$genou_d_rotul_obs,"</h5>")
          genoux_text_2 <- paste0("<h5>",input$genou_d_rotule_exam,"</h5>")
          genoux_text_10 <- paste0(genoux_text_1,genoux_text_2)
        }
        
        genoux_synthese <- paste0(genoux_synthese, genoux_text_0, genoux_text_1, genoux_text_2, genoux_text_3, genoux_text_4, genoux_text_5, genoux_text_6, genoux_text_7, genoux_text_8,genoux_text_9,genoux_text_10)
        
      }
      
      
    }
    
    HTML(genoux_synthese)
  })
  
  # Syntèse - Examen des pieds et des chevilles ####
  output$piedschevilles_synthese <- renderUI({
    
    piedschevilles_synthese <- ""
    
    if( !("Non" %in% input$cheville_pieds_activate) ){
      
      pc_text_0 <- "<h3>Analyse de la mobilité<h3>"
      pc_text_1 <- paste0("<h5>",input$cheville_attitude_obs,"</h5>")
      pc_text_2 <- paste0("<h5>Marche spontanée : ",input$cheville_marche_1,"</h5>")
      pc_text_3 <- paste0("<h5>Marche sur les talons : ",input$cheville_marche_2,"</h5>")
      pc_text_4 <- paste0("<h5>Marche sur les pointes : ",input$cheville_marche_3,"</h5>")
      pc_text_5 <- paste0("<h5>Accroupissement : ",input$cheville_accroupissement,"</h5>")
      pc_text_6 <- paste0("<h5>Appui monopodal à Droite : ",input$cheville_appuimono_D,"</h5>")
      pc_text_7 <- paste0("<h5>Appui monopodal à Gauche : ",input$cheville_appuimono_G,"</h5>")
      pc_text_8 <- paste0("<h5>Sautillement unipodal à Droite : ",input$genoux_sautmono_D,"</h5>")
      pc_text_9 <- paste0("<h5>Sautillement unipodal à Gauche : ",input$genoux_sautmono_G,"</h5>")
      pc_text_10 <- paste0("<h5>",input$cheville_mobilite_obs,"</h5>")
      pc_text_11 <- ifelse(input$cheville_g_raccourcissement==0,"<h5>Pas de raccourcissement du membre inférieur gauche<h5>", paste0("<h5>Raccourcissement du membre inférieur gauche à ", input$cheville_g_raccourcissement ," cm<h5>") )
      pc_text_12 <- ifelse(input$cheville_d_raccourcissement==0,"<h5>Pas de raccourcissement du membre inférieur droit<h5>", paste0("<h5>Raccourcissement du membre inférieur droit à ", input$cheville_d_raccourcissement ," cm<h5>") )
      
      piedschevilles_synthese <- paste0(piedschevilles_synthese, pc_text_0,pc_text_1,pc_text_2,pc_text_3,pc_text_4,pc_text_5,pc_text_6,pc_text_7,pc_text_8,pc_text_9,pc_text_10,pc_text_11,pc_text_12)
      
      if(input$cheville_pieds_activate %in% c("Oui à gauche","Oui les deux") ){
        
        pc_text_0 <- "<h3>Examen de la cheville gauche</h3>"
        pc_text_1 <- "<h4>Mensurations à gauche en cm : </h4>"
        pc_text_2 <- ifelse(input$mens_g0_cheville>=0,paste0("<h5>Cuisse (mesuré à 15 cm du sommet rotulien) : ",input$mens_g0_cheville," degrés </h5>"), "")
        pc_text_3 <- ifelse(input$mens_g1_cheville>=0,paste0("<h5>Genou (au centre rotulien) : ",input$mens_g1_cheville," degrés </h5>"), "")
        pc_text_4 <- ifelse(input$mens_g2_cheville>=0,paste0("<h5>Mollet (à 10 cm de la pointe de la rotule) : ",input$mens_g2_cheville," degrés </h5>"), "")
        pc_text_5 <- ifelse(input$mens_g3_cheville>=0,paste0("<h5>Malléolaire : ",input$mens_g3_cheville," degrés </h5>"), "")
        pc_text_6 <- ifelse(input$mens_g4_cheville>=0,paste0("<h5>Etrier : ",input$mens_g4_cheville," degrés </h5>"), "")
        pc_text_7 <- "<h4>Inspection et palpation de la cheville gauche : </h4>"
        pc_text_8 <- paste0("<h5>Troubles vasomoteurs : ",input$cheville_g_tbvaso,"</h5>")
        pc_text_9 <- paste0("<h5>Empâtement sous-malléolaire : ",input$cheville_g_empatsous,"</h5>")
        pc_text_10 <- paste0("<h5>Empâtement rétro-malléolaire : ",input$cheville_g_empatretro,"</h5>")
        pc_text_11 <- paste0("<h5>Cicatrices à gauche : ",input$cheville_g_cicatrices_obs,"</h5>")
        pc_text_12 <- "<h4>Mobilité de la cheville gauche : </h4>" 
        pc_text_13 <- paste0("<h5>Articulation tibio-tarsienne : ",input$activate_tibiotarsienne_g,"</h5>")
        if(input$activate_tibiotarsienne_g=='Blocage'){
          pc_text_14 <- paste0("<h5>Blocage de la cheville : ",input$cheville_g_tibiotarsienne_blocage,"</h5>")
          pc_text_15 <- paste0("<h5>Déviation de la cheville : ",input$cheville_g_tibiotarsienne_blocage2,"</h5>")
        }else{
          pc_text_14 <- ""
          pc_text_15 <- ""
        }
        if(input$activate_tibiotarsienne_g=='Limitation'){
          pc_text_16 <- paste0("<h5>Flexion plantaire : ",input$cheville_g_flexionp," degrés </h5>")
          pc_text_17 <- paste0("<h5>Flexion dorsale : ",input$cheville_g_flexiond," degrés </h5>")
          pc_text_18 <- paste0("<h5>Limitation de la cheville : ",input$cheville_g_tibiotarsienne_limitation,"</h5>")
          pc_text_19 <- paste0("<h5>Déviation de la cheville : ",input$cheville_g_tibiotarsienne_limitation2,"</h5>")
        }else{
          pc_text_16 <- paste0("<h5>Flexion plantaire : ",input$cheville_g_flexionp," degrés </h5>")
          pc_text_17 <- paste0("<h5>Flexion dorsale : ",input$cheville_g_flexiond," degrés </h5>")
          pc_text_18 <- ""
          pc_text_19 <- ""
        }
        pc_text_20 <- paste0("<h5>Articulations sous-astragaliennes et tarso-métatarsiennes : ",input$activate_tarsometatarsiennes_g,"</h5>")
        pc_text_21 <- paste0("<h5>Adduction : ",input$cheville_g_tarsometatarsiennes_adduction," degrés </h5>")
        pc_text_22 <- paste0("<h5>Abduction : ",input$cheville_g_tarsometatarsiennes_adduction," degrés </h5>")
        
        pc_text_23 <- "<h3>Examen du pied gauche</h3>"
        pc_text_24 <- "<h4>Aspect général du pied gauche : </h4>"
        pc_text_25 <- paste0("<h5>Cal vicieux, exubérant : ",input$pied_g_calvicieux,"</h5>")
        pc_text_26 <- paste0("<h5>Exostose sous-calcanéenne : ",input$pied_g_exostose,"</h5>")
        pc_text_27 <- paste0("<h5>Pied creux post-traumatique : ",input$pied_g_piedcreux,"</h5>")
        pc_text_28 <- paste0("<h5>Affaissement de la voûte plantaire : ",input$pied_g_vouteplantaire,"</h5>")
        pc_text_29 <- "<h4>Examen du gros orteil : </h4>"
        pc_text_30 <- paste0("<h5>Articulations du gros orteils intactes : ",input$activate_grosorteil_g,"</h5>")
        if(input$activate_grosorteil_g=='Non'){
          pc_text_31 <- "<h5>Articulations métatarso-phalangiennes du gros orteil : </h5>"
          pc_text_32 <- paste0("<h5>Blocage du gros orteil : ",input$pied_g_grosorteil1,"</h5>")
          pc_text_33 <- paste0("<h5>Limitation de la mobilité du gros orteil : ",input$pied_g_grosorteil2,"</h5>")
          pc_text_34 <- "<h5>Articulations interphalangiennes : </h5>"
          pc_text_35 <- paste0("<h5>Interphalangienne du gros orteil : ",input$pied_g_grosorteil3,"</h5>")
        }else{
          pc_text_31 <- ""
          pc_text_32 <- ""
          pc_text_33 <- ""
          pc_text_34 <- ""
          pc_text_35 <- ""
        }
        if(input$activate_autresorteil_g=='Non'){
          pc_text_36 <- paste0("<h5>Articulations du autres orteils intactes : ",input$activate_autresorteil_g,"</h5>")
          pc_text_37 <- paste0("<h5>Existe-t-il un ou plusieurs autres orteil bloqué ? : ",input$pied_g_autresorteilblocage," : ", input$pied_g_autresorteilblocage_obs,"</h5>")
          pc_text_38 <- paste0("<h5>Existe-t-il un ou plusieurs autres orteil limités en mouvement ? : ",input$pied_g_autresorteillimitation," : ", input$pied_g_autresorteillimitation_obs,"</h5>")
        }else{
          pc_text_36 <- ""
          pc_text_37 <- ""
          pc_text_38 <- ""
        }
        
        # paste0(paste0("pc_text_",c(0:38)),collapse=",")
        piedschevilles_synthese <- paste0(piedschevilles_synthese,pc_text_0,pc_text_1,pc_text_2,pc_text_3,pc_text_4,pc_text_5,pc_text_6,pc_text_7,pc_text_8,pc_text_9,pc_text_10,pc_text_11,pc_text_12,pc_text_13,pc_text_14,pc_text_15,pc_text_16,pc_text_17,pc_text_18,pc_text_19,pc_text_20,pc_text_21,pc_text_22,pc_text_23,pc_text_24,pc_text_25,pc_text_26,pc_text_27,pc_text_28,pc_text_29,pc_text_30,pc_text_31,pc_text_32,pc_text_33,pc_text_34,pc_text_35,pc_text_36,pc_text_37,pc_text_38)
      }
      
      if(input$cheville_pieds_activate %in% c("Oui à droite","Oui les deux") ){
        pc_text_0 <- "<h3>Examen de la cheville droite</h3>"
        pc_text_1 <- "<h4>Mensurations à droite en cm : </h4>"
        pc_text_2 <- ifelse(input$mens_d0_cheville>=0,paste0("<h5>Cuisse (mesuré à 15 cm du sommet rotulien) : ",input$mens_d0_cheville," degrés </h5>"), "")
        pc_text_3 <- ifelse(input$mens_d1_cheville>=0,paste0("<h5>Genou (au centre rotulien) : ",input$mens_d1_cheville," degrés </h5>"), "")
        pc_text_4 <- ifelse(input$mens_d2_cheville>=0,paste0("<h5>Mollet (à 10 cm de la pointe de la rotule) : ",input$mens_d2_cheville," degrés </h5>"), "")
        pc_text_5 <- ifelse(input$mens_d3_cheville>=0,paste0("<h5>Malléolaire : ",input$mens_d3_cheville," degrés </h5>"), "")
        pc_text_6 <- ifelse(input$mens_d4_cheville>=0,paste0("<h5>Etrier : ",input$mens_d4_cheville," degrés </h5>"), "")
        pc_text_7 <- "<h4>Inspection et palpation de la cheville droite : </h4>"
        pc_text_8 <- paste0("<h5>Troubles vasomoteurs : ",input$cheville_d_tbvaso,"</h5>")
        pc_text_9 <- paste0("<h5>Empâtement sous-malléolaire : ",input$cheville_d_empatsous,"</h5>")
        pc_text_10 <- paste0("<h5>Empâtement rétro-malléolaire : ",input$cheville_d_empatretro,"</h5>")
        pc_text_11 <- paste0("<h5>Cicatrices à droite : ",input$cheville_d_cicatrices_obs,"</h5>")
        pc_text_12 <- "<h4>Mobilité de la cheville droite : </h4>" 
        pc_text_13 <- paste0("<h5>Articulation tibio-tarsienne : ",input$activate_tibiotarsienne_d,"</h5>")
        if(input$activate_tibiotarsienne_d=='Blocage'){
          pc_text_14 <- paste0("<h5>Blocage de la cheville : ",input$cheville_d_tibiotarsienne_blocage,"</h5>")
          pc_text_15 <- paste0("<h5>Déviation de la cheville : ",input$cheville_d_tibiotarsienne_blocage2,"</h5>")
        }else{
          pc_text_14 <- ""
          pc_text_15 <- ""
        }
        if(input$activate_tibiotarsienne_d=='Limitation'){
          pc_text_16 <- paste0("<h5>Flexion plantaire : ",input$cheville_d_flexionp," degrés </h5>")
          pc_text_17 <- paste0("<h5>Flexion dorsale : ",input$cheville_d_flexiond," degrés </h5>")
          pc_text_18 <- paste0("<h5>Limitation de la cheville : ",input$cheville_d_tibiotarsienne_limitation,"</h5>")
          pc_text_19 <- paste0("<h5>Déviation de la cheville : ",input$cheville_d_tibiotarsienne_limitation2,"</h5>")
        }else{
          pc_text_16 <- paste0("<h5>Flexion plantaire : ",input$cheville_d_flexionp," degrés </h5>")
          pc_text_17 <- paste0("<h5>Flexion dorsale : ",input$cheville_d_flexiond," degrés </h5>")
          pc_text_18 <- ""
          pc_text_19 <- ""
        }
        pc_text_20 <- paste0("<h5>Articulations sous-astragaliennes et tarso-métatarsiennes : ",input$activate_tarsometatarsiennes_d,"</h5>")
        pc_text_21 <- paste0("<h5>Adduction : ",input$cheville_d_tarsometatarsiennes_adduction," degrés </h5>")
        pc_text_22 <- paste0("<h5>Abduction : ",input$cheville_d_tarsometatarsiennes_adduction," degrés </h5>")
        
        pc_text_23 <- "<h3>Examen du pied droit</h3>"
        pc_text_24 <- "<h4>Aspect général du pied droit : </h4>"
        pc_text_25 <- paste0("<h5>Cal vicieux, exubérant : ",input$pied_d_calvicieux,"</h5>")
        pc_text_26 <- paste0("<h5>Exostose sous-calcanéenne : ",input$pied_d_exostose,"</h5>")
        pc_text_27 <- paste0("<h5>Pied creux post-traumatique : ",input$pied_d_piedcreux,"</h5>")
        pc_text_28 <- paste0("<h5>Affaissement de la voûte plantaire : ",input$pied_d_vouteplantaire,"</h5>")
        pc_text_29 <- "<h4>Examen du gros orteil : </h4>"
        pc_text_30 <- paste0("<h5>Articulations du gros orteils intactes : ",input$activate_grosorteil_d,"</h5>")
        if(input$activate_grosorteil_d=='Non'){
          pc_text_31 <- "<h5>Articulations métatarso-phalangiennes du gros orteil : </h5>"
          pc_text_32 <- paste0("<h5>Blocage du gros orteil : ",input$pied_d_grosorteil1,"</h5>")
          pc_text_33 <- paste0("<h5>Limitation de la mobilité du gros orteil : ",input$pied_d_grosorteil2,"</h5>")
          pc_text_34 <- "<h5>Articulations interphalangiennes : </h5>"
          pc_text_35 <- paste0("<h5>Interphalangienne du gros orteil : ",input$pied_d_grosorteil3,"</h5>")
        }else{
          pc_text_31 <- ""
          pc_text_32 <- ""
          pc_text_33 <- ""
          pc_text_34 <- ""
          pc_text_35 <- ""
        }
        if(input$activate_autresorteil_g=='Non'){
          pc_text_36 <- paste0("<h5>Articulations du autres orteils intactes : ",input$activate_autresorteil_d,"</h5>")
          pc_text_37 <- paste0("<h5>Existe-t-il un ou plusieurs autres orteil bloqué ? : ",input$pied_d_autresorteilblocage," : ", input$pied_d_autresorteilblocage_obs,"</h5>")
          pc_text_38 <- paste0("<h5>Existe-t-il un ou plusieurs autres orteil limités en mouvement ? : ",input$pied_d_autresorteillimitation," : ", input$pied_d_autresorteillimitation_obs,"</h5>")
        }else{
          pc_text_36 <- ""
          pc_text_37 <- ""
          pc_text_38 <- ""
        }
        piedschevilles_synthese <- paste0(piedschevilles_synthese,pc_text_0,pc_text_1,pc_text_2,pc_text_3,pc_text_4,pc_text_5,pc_text_6,pc_text_7,pc_text_8,pc_text_9,pc_text_10,pc_text_11,pc_text_12,pc_text_13,pc_text_14,pc_text_15,pc_text_16,pc_text_17,pc_text_18,pc_text_19,pc_text_20,pc_text_21,pc_text_22,pc_text_23,pc_text_24,pc_text_25,pc_text_26,pc_text_27,pc_text_28,pc_text_29,pc_text_30,pc_text_31,pc_text_32,pc_text_33,pc_text_34,pc_text_35,pc_text_36,pc_text_37,pc_text_38)
        
      }
    }
    
    HTML(piedschevilles_synthese)
    })
  
  # Syntèse - Synthèse intermédiaire ####
  
  output$intermediaire_synthese <- renderUI({
    intermediaire_text_orrigin <- paste0("<h1>OBSERVATION DU ", input$medecinconseil, " DU ",ymd(Sys.Date()) %>% format('%d %b %Y'))
    intermediaire_text_0 <- "<h2>SITUATION SOCIOPROFESSIONNELLE</h2>"
    age <- as.numeric(substr(Sys.Date(),1,4))-1-as.numeric(input$yearbirth)
    intermediaire_text_1 <- paste0(input$gender," ",paste0(age," ans"))
    intermediaire_text_2 <- paste0(input$work,", ",input$boss,", ", input$work_time, ifelse(input$mdph=="Oui",", RQTH","") )
    intermediaire_text_2b <- paste0("<section>",input$admin_context,"</section>")
    intermediaire_text_3 <- ifelse(input$cadreprestation=="Accident du travail", "<h2>Antécédents pouvants interférer avec l'accident :</h2>", "<h2>Antécédents :</h2>" )
    intermediaire_text_3b <- paste0("<h4>",input$antecedents,"</h4>")
    intermediaire_text_4 <- ifelse(input$cadreprestation=="Accident du travail", "<h2>Les faits de l'accident du travail :</h2>", "<h2>Histoire de la maladie</h2>" )
    intermediaire_text_4b <- paste0("<h4>",input$disease_context,"</h4>")
    intermediaire_text_5 <- "<h2>TRAITEMENTS ACTUELS :</h2>"
    intermediaire_text_5b <- paste0("<h4>",input$traitement_context,"</h4>")
    # concaténation des chaînes de caractères pour la synthèse intermédiaire
    intermediaire_text_6 <- "<h2>DOCUMENTS PRESENTES :</h2>"
    intermediaire_text_7 <- paste0("<h4>",input$documents_hist,"</h4>")
    intermediaire_text_8 <- "<h2>DOLEANCES :</h2>"
    intermediaire_text_9 <- paste0("<h4>",input$doleances_context,"</h4>")
    intermediaire_text_10 <- paste0("<h2>EXAMEN CLINIQUE DU ",ymd(Sys.Date()) %>% format('%d %b %Y')," :</h2>")
    intermediaire_text_11 <- paste0("<h5>Chez un ",input$dominance,"</h5>")
    
    intermediaire_synthese <- paste(intermediaire_text_orrigin, intermediaire_text_0,intermediaire_text_1,intermediaire_text_2,intermediaire_text_2b,intermediaire_text_3,intermediaire_text_3b,intermediaire_text_4,intermediaire_text_4b,intermediaire_text_5,intermediaire_text_5b,
                                    intermediaire_text_6,intermediaire_text_7,intermediaire_text_8,intermediaire_text_9,intermediaire_text_10,intermediaire_text_11)
    
    HTML(intermediaire_synthese)
  })
  
  
  # Synthese - Examen des amputations ####
  output$amputation_synthese <- renderUI({
    
    amputation_synthese <- ""
    
    if(input$activate_amputation=="Oui"){
      
      amputation_text0 <- ifelse(input$amputation_epaule_g_1=="Oui", "<h5>Amputation interscapulothoracique gauche avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os</h5>","")
      amputation_text1 <- ifelse(input$amputation_epaule_g_2=="Oui", "<h5>Désarticulation de l'épaule gauche</h5>","")
      amputation_text2 <- ifelse(input$amputation_bras_g_1=="Oui", "<h5>Amputation du bras gauche au tiers supérieur</h5>","")
      amputation_text3 <- ifelse(input$amputation_bras_g_2=="Oui", "<h5>Amputation du bras gauche au tiers moyen ou inférieur</h5>","")
      amputation_text4 <- ifelse(input$amputation_bras_g_3=="Oui", "<h5>Désarticulation du coude, avant-bras au tiers supérieur à gauche</h5>","")
      amputation_text5 <- ifelse(input$amputation_main_g_1=="Oui", "<h5>Amputation métacarpienne gauche conservant une palette</h5>","")
      amputation_text6 <- ifelse(input$amputation_epaule_d_1=="Oui", "<h5>Amputation interscapulothoracique droite avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os</h5>","")
      amputation_text7 <- ifelse(input$amputation_epaule_d_2=="Oui", "<h5>Désarticulation de l'épaule droite</h5>","")
      amputation_text8 <- ifelse(input$amputation_bras_d_1=="Oui", "<h5>Amputation du bras droit au tiers supérieur</h5>","")
      amputation_text9 <- ifelse(input$amputation_bras_d_2=="Oui", "<h5>Amputation du bras droit au tiers moyen ou inférieur</h5>","")
      amputation_text10 <- ifelse(input$amputation_bras_d_3=="Oui", "<h5>Désarticulation du coude, avant-bras au tiers supérieur à droite</h5>","")
      amputation_text11 <- ifelse(input$amputation_main_d_1=="Oui", "<h5>Amputation métacarpienne droit conservant une palette</h5>","")
      
      amputation_text12 <- ifelse(input$amputation_jambe_g_1=="Oui", "<h5>Amputation inter-ilio-abdominale gauche</h5>","")
      amputation_text13 <- ifelse(input$amputation_jambe_g_2=="Oui", "<h5>Désarticulation de la hanche gauche</h5>","")
      amputation_text14 <- ifelse(input$amputation_jambe_g_3=="Oui", "<h5>Amputation inter-trochantérienne gauche</h5>","")
      amputation_text15 <- ifelse(input$amputation_jambe_g_4=="Oui", "<h5>Amputation sous-trochantérienne gauche</h5>","")
      amputation_text16 <- ifelse(input$amputation_jambe_g_5=="Oui", "<h5>Amputation au tiers moyen ou au tiers inférieur gauche</h5>","")
      amputation_text17 <- ifelse(input$amputation_jambe_g_6=="Oui", "<h5>Désarticulation du genou gauche</h5>","")
      amputation_text18 <- ifelse(input$amputation_jambe_g_7=="Oui", "<h5>Amputation au tiers supérieur de la jambe gauche</h5>","")
      amputation_text19 <- ifelse(input$amputation_jambe_g_8=="Oui", "<h5>Amputation au tiers moyen ou inférieur gauche</h5>","")
      amputation_text20 <- ifelse(input$amputation_jambe_g_9=="Oui", "<h5>Désarticulation tibio-tarsienne gauche</h5>","")
      amputation_text21 <- ifelse(input$amputation_jambe_g_10=="Oui", "<h5>Amputation du pied, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier, gauche</h5>","")
      amputation_text22 <- ifelse(input$amputation_jambe_g_11=="Oui", "<h5>Désarticulation médio-tarsienne de Chopart gauche</h5>","")
      amputation_text23 <- ifelse(input$amputation_jambe_g_12=="Oui", "<h5>Amputation transmétatarsienne de l'avant-pied gauche</h5>","")
      amputation_text24 <- ifelse(input$amputation_jambe_g_13=="Oui", "<h5>Amputation du premier orteil gauche</h5>","")
      amputation_text25 <- ifelse(input$amputation_jambe_g_14!="Non", paste0("<h5>Amputation d'un orteil à gauche :",paste(input$amputation_jambe_g_15, collapse = ", "),"</h5>"),"")
      
      amputation_text26 <- ifelse(input$amputation_jambe_d_1=="Oui", "<h5>Amputation inter-ilio-abdominale droite</h5>","")
      amputation_text27 <- ifelse(input$amputation_jambe_d_2=="Oui", "<h5>Désarticulation de la hanche droite</h5>","")
      amputation_text28 <- ifelse(input$amputation_jambe_d_3=="Oui", "<h5>Amputation inter-trochantérienne droite</h5>","")
      amputation_text29 <- ifelse(input$amputation_jambe_d_4=="Oui", "<h5>Amputation sous-trochantérienne droite</h5>","")
      amputation_text30 <- ifelse(input$amputation_jambe_d_5=="Oui", "<h5>Amputation au tiers moyen ou au tiers inférieur droite</h5>","")
      amputation_text31 <- ifelse(input$amputation_jambe_d_6=="Oui", "<h5>Désarticulation du genou droite</h5>","")
      amputation_text32 <- ifelse(input$amputation_jambe_d_7=="Oui", "<h5>Amputation au tiers supérieur de la jambe droite</h5>","")
      amputation_text33 <- ifelse(input$amputation_jambe_d_8=="Oui", "<h5>Amputation au tiers moyen ou inférieur droite</h5>","")
      amputation_text34 <- ifelse(input$amputation_jambe_d_9=="Oui", "<h5>Désarticulation tibio-tarsienne droite</h5>","")
      amputation_text35 <- ifelse(input$amputation_jambe_d_10=="Oui", "<h5>Amputation du pied, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier, droite</h5>","")
      amputation_text36 <- ifelse(input$amputation_jambe_d_11=="Oui", "<h5>Désarticulation médio-tarsienne de Chopart droite</h5>","")
      amputation_text37 <- ifelse(input$amputation_jambe_d_12=="Oui", "<h5>Amputation transmétatarsienne de l'avant-pied droite</h5>","")
      amputation_text38 <- ifelse(input$amputation_jambe_d_13=="Oui", "<h5>Amputation du premier orteil droit</h5>","")
      amputation_text39 <- ifelse(input$amputation_jambe_d_14!="Non", paste0("<h5>Amputation d'un orteil à droite :",paste(input$amputation_jambe_d_15, collapse = ", "),"</h5>"),"")
      
      amputation_synthese <- paste0(amputation_text0, amputation_text1, amputation_text2, amputation_text3, amputation_text4, amputation_text5, amputation_text6, amputation_text7, amputation_text8, amputation_text9, amputation_text10, amputation_text11, amputation_text12, amputation_text13, amputation_text14, amputation_text15, amputation_text16, amputation_text17, amputation_text18, amputation_text19, amputation_text20, amputation_text21, amputation_text22, amputation_text23, amputation_text24, amputation_text25, amputation_text26, amputation_text27, amputation_text28, amputation_text29, amputation_text30, amputation_text31, amputation_text32, amputation_text33, amputation_text34, amputation_text35, amputation_text36, amputation_text37, amputation_text38, amputation_text39)
      
      
    }

    HTML(amputation_synthese)
  })
  
  # Synthese - Examen de la Capacité de travail ####
  
  output$inval_synthese <- renderUI({
    
    synthese <- ""
    if(input$invalidite_activate=="Oui"){
      
      # Outil AMI
      
      synthese_1 <- ifelse(input$inval_AMI_item1==0, "Capacité normale, aucune limitation dans la vie quotidienne","")
      synthese_1 <- ifelse(input$inval_AMI_item1==1, "Vue corrigée suffisante pour pouvoir se déplacer et s'orienter mais insuffisante pour reconnaitre les visages à une distance de plus de 15m.",synthese_1)
      synthese_1 <- ifelse(input$inval_AMI_item1==2, "Même avec correction ne reconnaît pas les visages à une distance de plus de 10 mètres. Même avec correction ne peut pas lire à une distance habituelle.",synthese_1)
      synthese_1 <- ifelse(input$inval_AMI_item1==3, "A une  atteinte du champ visuel ou des troubles de la vision qui impliquent des difficultés importantes et des risques dans les déplacements.",synthese_1)
      synthese_1 <- ifelse(input$inval_AMI_item1==4, "Est malvoyant ou aveugle.",synthese_1)
      synthese_1 <- paste0("<h5>Regarder : déficience = ",input$inval_AMI_item1," : ",synthese_1,"</h5>")
      
      synthese_2 <- ifelse(input$inval_AMI_item2==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_2 <- ifelse(input$inval_AMI_item2==1, "Peut avoir des difficultés à entendre ce qui est dit si quelqu'un parle normalement avec un bruit de fond",synthese_2)
      synthese_2 <- ifelse(input$inval_AMI_item2==2, "N'entend pas ce qui est dit à une distance de plus de 3 mètres lorsque quelqu'un parle fort avec un bruit de fond.",synthese_2)
      synthese_2 <- ifelse(input$inval_AMI_item2==3, "N'entend pas ce qui est dit à une distance de moins de 3 mètres lorsque quelqu'un parle fort avec un bruit de fond.",synthese_2)
      synthese_2 <- ifelse(input$inval_AMI_item2==4, "Est sourd.",synthese_2)
      synthese_1 <- paste0("<h5>Ecouter : déficience = ",input$inval_AMI_item2," : ",synthese_2,"</h5>")
      
      synthese_3 <- ifelse(input$inval_AMI_item3==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_3 <- ifelse(input$inval_AMI_item3==1, "A besoin d'un peu plus de temps que la normale pour apprendre, comprendre et retenir une instruction pour une tâche de difficulté normale.",synthese_3)
      synthese_3 <- ifelse(input$inval_AMI_item3==2, "A besoin de deux fois plus de temps que la normale pour apprendre, comprendre et retenir une instruction écrite pour une tâche de difficulté normale.",synthese_3)
      synthese_3 <- ifelse(input$inval_AMI_item3==3, "A besoin d'une démonstration pour pouvoir effectuer une tâche. Ne peut pas apprendre, comprendre et retenir une instruction écrite pour une tâche simple.",synthese_3)
      synthese_3 <- ifelse(input$inval_AMI_item3==4, "Ne peut pas apprendre et comprendre comment réaliser une tâche simple  même avec une démonstration.",synthese_3)
      synthese_3 <- paste0("<h5>Acquérir des compétences : déficience = ",input$inval_AMI_item3," : ",synthese_3,"</h5>")
      
      synthese_4 <- ifelse(input$inval_AMI_item4==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_4 <- ifelse(input$inval_AMI_item4==1, "Peut suivre un récit, une lecture, une conversation ou un programme télévisé plus d'une demi-heure.",synthese_4)
      synthese_4 <- ifelse(input$inval_AMI_item4==2, "Peut suivre un récit, une lecture, une conversation ou un programme télévisé pendant 30 minutes maximum.",synthese_4)
      synthese_4 <- ifelse(input$inval_AMI_item4==3, "Ne peut pas suivre un récit, une lecture, une conversation ou un programme télévisé pendant plus de 10 minutes.",synthese_4)
      synthese_4 <- ifelse(input$inval_AMI_item4==4, "Ne peut pas suivre un récit, une lecture, une conversation ou un programme télévisé.",synthese_4)
      synthese_4 <- paste0("<h5>Fixer son attention : déficience = ",input$inval_AMI_item4," : ",synthese_4,"</h5>")
      
      synthese_5 <- ifelse(input$inval_AMI_item5==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_5 <- ifelse(input$inval_AMI_item5==1, "A besoin d'un peu plus de temps que la normale pour exécuter des tâches ou activités habituelles.",synthese_5)
      synthese_5 <- ifelse(input$inval_AMI_item5==2, "A besoin de deux fois plus de temps que la normale pour exécuter des tâches ou activités habituelles.",synthese_5)
      synthese_5 <- ifelse(input$inval_AMI_item5==3, "A souvent besoin d'aide pour terminer une tâche ou activité habituelle.",synthese_5)
      synthese_5 <- ifelse(input$inval_AMI_item5==4, "Ne peut pas exécuter une tâche ou activité habituelle de façon autonome.",synthese_5)
      synthese_5 <- paste0("<h5>Exécuter une tâche unique : déficience = ",input$inval_AMI_item5," : ",synthese_5,"</h5>")
      
      synthese_6 <- ifelse(input$inval_AMI_item6==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_6 <- ifelse(input$inval_AMI_item6==1, "A rarement des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese_6)
      synthese_6 <- ifelse(input$inval_AMI_item6==2, "A parfois des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese_6)
      synthese_6 <- ifelse(input$inval_AMI_item6==3, "A souvent des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese_6)
      synthese_6 <- ifelse(input$inval_AMI_item6==4, "Ne peut pas enchainer et/ou coordonner des tâches ou activités habituelles.",synthese_6)
      synthese_6 <- paste0("<h5>Exécuter des tâches multiples : déficience = ",input$inval_AMI_item6," : ",synthese_6,"</h5>")
      
      synthese_7 <- ifelse(input$inval_AMI_item7==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_7 <- ifelse(input$inval_AMI_item7==1, "Ressent du désagrément et de l'inquiétude en cas de changement de programme, des routines quotidiennes, ou lors de la prise de responsabilité.",synthese_7)
      synthese_7 <- ifelse(input$inval_AMI_item7==2, "Ressent une forte frustration  (insatisfaction) lorsque les exigences de prestation sont occasionnellement plus élevées. A des difficultés pour gérer un changement imprévu dans la routine habituelle.",synthese_7)
      synthese_7 <- ifelse(input$inval_AMI_item7==3, "Ne peut pas gérer un changement prévu de la routine habituelle.",synthese_7)
      synthese_7 <- ifelse(input$inval_AMI_item7==4, "Ne supporte pas de changement de routine quotidienne.",synthese_7)
      synthese_7 <- paste0("<h5>Gérer le stress et autres exigences psychologiques : déficience = ",input$inval_AMI_item7," : ",synthese_7,"</h5>")
      
      synthese_8 <- ifelse(input$inval_AMI_item8==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_8 <- ifelse(input$inval_AMI_item8==1, "S'exprime parfois (par oral ou par écrit) avec des fautes de langage, mais qui ne bloquent pas la possibilité de compréhension.",synthese_8)
      synthese_8 <- ifelse(input$inval_AMI_item8==2, "S'exprime  (par oral ou par écrit) avec des fautes de langage, qui rendent difficile la compréhension.",synthese_8)
      synthese_8 <- ifelse(input$inval_AMI_item8==3, "S'exprime (par oral ou par écrit) avec des fautes de langage, qui rendent très difficile la compréhension.",synthese_8)
      synthese_8 <- ifelse(input$inval_AMI_item8==4, "Troubles du langage sévère, aphasie.",synthese_8)
      synthese_8 <- paste0("<h5>Comprendre et s'exprimer par la parole et l'écrit : déficience = ",input$inval_AMI_item8," : ",synthese_8,"</h5>")
      
      synthese_9 <- ifelse(input$inval_AMI_item9==0, "Capacité normale, aucune limitation dans la vie quotidienne. Peut s'incliner, se mettre à genoux ou accroupi et ramasser un papier au sol.","")
      synthese_9 <- ifelse(input$inval_AMI_item9==1, "Peut s'incliner, se mettre à genoux ou accroupi et se relever avec quelques difficultés.",synthese_9)
      synthese_9 <- ifelse(input$inval_AMI_item9==2, "Peut s'incliner, se mettre à genoux ou accroupi avec difficulté. Ne peut plus se relever seul du sol mais peut se lever d'une chaise.",synthese_9)
      synthese_9 <- ifelse(input$inval_AMI_item9==3, "Ne peut pas s'incliner ni se relever sans l'aide d'une autre personne. Parvient seul mais avec difficulté à se lever d'une chaise.",synthese_9)
      synthese_9 <- ifelse(input$inval_AMI_item9==4, "Ne peut pas se lever de la position assise sans l'aide d'une autre personne.",synthese_9)
      synthese_9 <- paste0("<h5>Changer la position corporelle de base : déficience = ",input$inval_AMI_item9," : ",synthese_9,"</h5>")
      
      synthese_10 <- ifelse(input$inval_AMI_item10==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_10 <- ifelse(input$inval_AMI_item10==1, "Ne peut rester debout plus de 30 minutes, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 60 minutes sans être obligé de se lever.",synthese_10)
      synthese_10 <- ifelse(input$inval_AMI_item10==2, "Ne peut rester debout plus de 15 à 30 minutes, même avec la possibilté de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 30 minutes sans être obligé de se lever.",synthese_10)
      synthese_10 <- ifelse(input$inval_AMI_item10==3, "Ne peut rester debout plusde 15 minutes, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 15 minutes sans être obligé de se lever.",synthese_10)
      synthese_10 <- ifelse(input$inval_AMI_item10==4, "Ne peut rester debout plus de 10 minutes sans l'appui d'une autre personne, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 10 minutes sans être obligé de se lever.",synthese_10)
      synthese_10 <- paste0("<h5>Garder la position du corps : déficience = ",input$inval_AMI_item10," : ",synthese_10,"</h5>")
      
      synthese_11 <- ifelse(input$inval_AMI_item11==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_11 <- ifelse(input$inval_AMI_item11==1, "Ne peut pas porter et déplacer un objet qui pèse plus de 20 kg (exemple: deux packs d'eau)",synthese_11)
      synthese_11 <- ifelse(input$inval_AMI_item11==2, "Ne peut pas porter et déplacer un objet qui pèse plus de 10kg (exemple: un pack d'eau).",synthese_11)
      synthese_11 <- ifelse(input$inval_AMI_item11==3, "Ne peut pas porter et déplacer un objet qui pèse plus de 5 kg (exemple : un sac pommes terres).",synthese_11)
      synthese_11 <- ifelse(input$inval_AMI_item11==4, "Ne peut pas avec porter et déplacer une charge d'1kg (exemple: un paquet de sucre).",synthese_11)
      synthese_11 <- paste0("<h5>Soulever et porter des objets : déficience = ",input$inval_AMI_item11," : ",synthese_11,"</h5>")
      
      synthese_12 <- ifelse(input$inval_AMI_item12==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_12 <- ifelse(input$inval_AMI_item12==1, "Ne peut pas sans difficulté saisir une épingle sur une table avec l'une ou l'autre main.",synthese_12)
      synthese_12 <- ifelse(input$inval_AMI_item12==2, "Peut saisir une pièce de deux euros ou un petit objet rond (comme une balle de ping pong) avec une main, mais pas avec l'autre. Peut utiliser un crayon, un clavier et une souris d'ordinateur une demi-heure au maximum.",synthese_12)
      synthese_12 <- ifelse(input$inval_AMI_item12==3, "Ne peut pas saisir une pièce de deux euros ou un petit objet rond (comme une balle de ping pong) avec l'une ou l'autre main. Ne peut utiliser ni crayon, ni clavier, ni souris d'ordinateur. Ne peut pas boutonner ou déboutonner une chemise ou un chemisier.",synthese_12)
      synthese_12 <- ifelse(input$inval_AMI_item12==4, "Ne peut pas composer un numéro de téléphone. Ne peut pas tourner les pages d'un livre de l'une ou l'autre main. Ne réussit pas à utiliser une clé.",synthese_12)
      synthese_12 <- paste0("<h5>Activités de motricité fine : déficience = ",input$inval_AMI_item12," : ",synthese_12,"</h5>")
      
      synthese_13 <- ifelse(input$inval_AMI_item13==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_13 <- ifelse(input$inval_AMI_item13==1, "Ne peut pas sans difficulté changer une ampoule. Ne peut pas sans difficulté ouvrir une boîte de conserve.",synthese_13)
      synthese_13 <- ifelse(input$inval_AMI_item13==2, "Peut lever un bras, mais pas l'autre, pour mettre un chapeau ou un bonnet. Peut ouvrir un robinet en forme d'étoile avec une main, mais pas avec l'autre. Ne peut pas tordre une serpillière. A des difficultés à tendre le bras pour verser une boisson.",synthese_13)
      synthese_13 <- ifelse(input$inval_AMI_item13==3, "Ne peut pas lever le bras pour mettre un chapeau ou un bonnet. Ne peut pas ouvrir une porte avec une poignée.Ne peut pas pousser ou tirer une petite valise de 10 kg à roulettes. Ne peut pas verser une bouteille de lait d'un litre.",synthese_13)
      synthese_13 <- ifelse(input$inval_AMI_item13==4, "Ne peut pas lever un bras pour mettre quelque chose dans la poche de sa veste. Ne peut pas étendre la main en arrière pour enfiler une veste. Ne peut pas ouvrir un robinet en forme d'étoile avec une main.",synthese_13)
      synthese_13 <- paste0("<h5>Utilisation des mains et des bras : déficience = ",input$inval_AMI_item13," : ",synthese_13,"</h5>")
      
      synthese_14 <- ifelse(input$inval_AMI_item14==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_14 <- ifelse(input$inval_AMI_item14==1, "Ne peut pas marcher sur une longue distance (environ 1km) sans s'arrêter plusieurs fois ou éprouver un grand inconfort.",synthese_14)
      synthese_14 <- ifelse(input$inval_AMI_item14==2, "Ne peut pas marcher sur une distance moyenne (environ 500 m) sur un sol régulier sans s'arrêter plusieurs fois ou éprouver un grand inconfort. Ne peut pas monter ou descendre un escalier sur plus d'un étage.",synthese_14)
      synthese_14 <- ifelse(input$inval_AMI_item14==3, "Ne peut pas marcher sans aide à la marche sans s'arrêter plusieurs fois ou éprouver un grand inconfort. Ne peut pas monter et descendre un escalier sur un étage.",synthese_14)
      synthese_14 <- ifelse(input$inval_AMI_item14==4, "Ne peut pas marcher.",synthese_14)
      synthese_14 <- paste0("<h5>Marcher : déficience = ",input$inval_AMI_item14," : ",synthese_14,"</h5>")
      
      synthese_15 <- ifelse(input$inval_AMI_item15==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_15 <- ifelse(input$inval_AMI_item15==1, "A des difficultés pour se déplacer mais y arrive seul.",synthese_15)
      synthese_15 <- ifelse(input$inval_AMI_item15==2, "Ne peut pas se déplacer de manière autonome à plus de 5 km du domicile.",synthese_15)
      synthese_15 <- ifelse(input$inval_AMI_item15==3, "Ne peut pas se déplacer de manière autonome à plus de 500 m du domicile.",synthese_15)
      synthese_15 <- ifelse(input$inval_AMI_item15==4, "Ne peut pas se déplacer de manière autonome à proximité du domicile.",synthese_15)
      synthese_15 <- paste0("<h5>Déplacements à l'extérieur du domicile : déficience = ",input$inval_AMI_item15," : ",synthese_15,"</h5>")
      
      synthese_16 <- ifelse(input$inval_AMI_item16==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
      synthese_16 <- ifelse(input$inval_AMI_item16==1, "Évite parfois les contacts sociaux.  Agressivité : discrète et rare. Intolérance aux critiques : discrète.",synthese_16)
      synthese_16 <- ifelse(input$inval_AMI_item16==2, "Evite souvent les contacts sociaux. Agressivité : modérée et peu fréquente (plusieurs fois par mois). Intolérance aux critiques : modérée. Respect des règles sociales : difficulté modérée.",synthese_16)
      synthese_16 <- ifelse(input$inval_AMI_item16==3, "Évite le contact avec les autres autant que possible. Agressivité: importante et fréquente (plusieurs fois par semaine). Intolérance aux critiques: importante et fréquente. Respect des règles sociales: difficulté importante.",synthese_16)
      synthese_16 <- ifelse(input$inval_AMI_item16==4, "Contact impossible avec les autres. Agressivité empêchant toute activité suivie. Respect des règles sociales : impossible.",synthese_16)
      synthese_16 <- paste0("<h5>Relations et interactions avec autrui : déficience = ",input$inval_AMI_item16," : ",synthese_16,"</h5>")
      
      synthese_AMI <- paste0(synthese_1,synthese_2,synthese_3,synthese_4,synthese_5,synthese_6,synthese_7,synthese_8,synthese_9,synthese_10,synthese_11,synthese_12,synthese_13,synthese_14,synthese_15,synthese_16)
      
      # Appreciation generale 
      inval0 <- paste0("<h5>",input$inval_travail_obs,"</h5>")
      inval1 <- ifelse(input$inval_travail_item1=="Oui", "<h5>L'assuré exerce actuellement une activité à temps partiel ou temps plein</h5>","<h5>L'assuré n'exerce pas actuellement une activité à temps partiel ou temps plein</h5>")
      inval2 <- ifelse(input$inval_travail_item2=="Oui","<h5>L'assuré a la capacité de maintenir une activité quelconque à temps partiel (>=2j par semaine)</h5>","<h5>L'assuré n'a pas la capacité de maintenir une activité quelconque à temps partiel (>=2j par semaine)</h5>")
      inval3 <- ifelse(input$inval_travail_item3=="Oui","<h5>L'assuré veut maintenir une activité à temps partiel (>=2j par semaine)</h5>","<h5>L'assuré ne veut pas de maintenir une activité à temps partiel (>=2j par semaine)</h5>")
      
      # Questionnaire ciblé inval CANCER
      if(input$inval_activate_cancer=='Oui'){
        invalgrav0 <- "<h4>Questionnaire ciblé cancérologie : </h4>"
        invalgrav0b <- paste0("<h5>",input$inval_cancer_grav_obs,"</h5>")
        invalgrav1 <- ""
        invalgrav1 <- ifelse(input$inval_cancer_grav_item1=="Oui","<h5>Il existe un traitement actif en cours</h5>",invalgrav1)
        invalgrav1 <- ifelse(input$inval_cancer_grav_item1=="Non","<h5>Il n'existe pas de traitement actif en cours</h5>",invalgrav1)
        invalgrav2 <- ""
        invalgrav2 <- ifelse(input$inval_cancer_grav_item2=="Oui","<h5>Il existe une chirurgie récente</h5>",invalgrav2)
        invalgrav2 <- ifelse(input$inval_cancer_grav_item2=="Non","<h5>Il n'existe pas de chirurgie récente</h5>",invalgrav2)
        invalgrav3 <- ""
        invalgrav3 <- ifelse(input$inval_cancer_grav_item3=="Oui","<h5>Il existe un traitement palliatif</h5>",invalgrav3)
        invalgrav3 <- ifelse(input$inval_cancer_grav_item3=="Non","<h5>Il n'existe pas de traitement palliatif</h5>",invalgrav3)
        invalgrav4 <- ""
        invalgrav4 <- ifelse(input$inval_cancer_grav_item4=="Oui","<h5>Il existe des métastases ou une masse non résécable</h5>",invalgrav4)
        invalgrav4 <- ifelse(input$inval_cancer_grav_item4=="Non","<h5>Il n'existe pas de métastases ou de masse non résécable</h5>",invalgrav4)
        invalgrav5 <- ""
        invalgrav5 <- ifelse(input$inval_cancer_grav_item5=="Oui","<h5>Il existe une surveillance active</h5>",invalgrav5)
        invalgrav5 <- ifelse(input$inval_cancer_grav_item5=="Non","<h5>Il n'existe pas de surveillance active</h5>",invalgrav5)
        
        invalgravspe <- paste0(invalgrav0,invalgrav0b,invalgrav1,invalgrav2,invalgrav3,invalgrav4,invalgrav5) 
      }else{
        invalgravspe <- ""
      }
      # Questionnaire ciblé inval PSY
      if(input$inval_activate_psy=='Oui'){
        
        invalgrav0 <- "<h4>Questionnaire ciblé psychiatrique : </h4>"
        invalgrav0b <- paste0("<h5>",input$inval_psy_grav_obs,"</h5>")
        invalgrav1 <- ""
        invalgrav1 <- ifelse(input$inval_psy_grav_item1=="Oui","<h5>Il existe un suivi psychiatrique mensuel ou inframensuel</h5>",invalgrav1)
        invalgrav1 <- ifelse(input$inval_psy_grav_item1=="Non","<h5>Il n'existe pas de suivi psychiatrique mensuel ou inframensuel</h5>",invalgrav1)
        invalgrav2 <- ""
        invalgrav2 <- ifelse(input$inval_psy_grav_item2=="Oui","<h5>Il existe une hospitalisation en cours</h5>",invalgrav2)
        invalgrav2 <- ifelse(input$inval_psy_grav_item2=="Non","<h5>Il n'existe pas d'hospitalisation en cours</h5>",invalgrav2)
        
        invalgrav3 <- ""
        invalgrav3 <- ifelse(input$inval_psy_grav_item3=="Oui","<h5>Il existe une hospitalisation récente en psychiatrie (< 2 ans)</h5>",invalgrav3)
        invalgrav3 <- ifelse(input$inval_psy_grav_item3=="Non","<h5>Il n'existe pas d'hospitalisation récente en psychiatrie (< 2 ans)</h5>",invalgrav3)
        
        invalgrav4 <- ""
        invalgrav4 <- ifelse(input$inval_psy_grav_item4=="Oui","<h5>L'assuré présente un danger quotidien ou hebdomadaire pour lui même</h5>",invalgrav4)
        invalgrav4 <- ifelse(input$inval_psy_grav_item4=="Non","<h5>L'assuré ne présente pas de danger quotidien ou hebdomadaire pour lui même</h5>",invalgrav4)
        
        invalgrav5 <- ""
        invalgrav5 <- ifelse(input$inval_psy_grav_item5=="Oui","<h5>L'assuré est dépendant d'une ou de plusieurs personnes</h5>",invalgrav5)
        invalgrav5 <- ifelse(input$inval_psy_grav_item5=="Non","<h5>L'assuré n'est pas dépendant d'une ou de plusieurs personnes</h5>",invalgrav5)
        
        invalgravspe <- paste0(invalgravspe, invalgrav0,invalgrav0b,invalgrav1,invalgrav2,invalgrav3,invalgrav4,invalgrav5) 
               
      }else{
        invalgravspe <- paste0(invalgravspe,"")
      }
      # Questionnaire ciblé inval TMS
      if(input$inval_activate_tms=='Oui'){
      
        invalgrav0 <- "<h4>Questionnaire ciblé troubles musculosquelettiques : </h4>"
        invalgrav0b <- paste0("<h5>",input$inval_tms_grav_obs,"</h5>")
        invalgrav1 <- ""
        invalgrav1 <- ifelse(input$inval_tms_grav_item1=="Oui","<h5>L'assuré est empéché d'effectuer des gestes quotidiens essentiels</h5>",invalgrav1)
        invalgrav1 <- ifelse(input$inval_tms_grav_item1=="Non","<h5>L'assuré n'est pas empéché significativement d'effectuer des gestes quotidiens essentiels</h5>",invalgrav1)
        invalgrav2 <- ""
        invalgrav2 <- ifelse(input$inval_tms_grav_item2=="Oui","<h5>L'assuré peut effectuer des tâches non manuelles</h5>",invalgrav2)
        invalgrav2 <- ifelse(input$inval_tms_grav_item2=="Non","<h5>L'assuré ne peut pas effectuer des tâches non manuelles</h5>",invalgrav2)
        
        invalgrav3 <- ""
        invalgrav3 <- ifelse(input$inval_tms_grav_item3=="Oui","<h5>L'assuré est capable de lire et d'écrire (ou d'apprendre dans l'année)</h5>",invalgrav3)
        invalgrav3 <- ifelse(input$inval_tms_grav_item3=="Non","<h5>L'assuré n'est pas capable de lire et d'écrire (ou d'apprendre dans l'année)</h5>",invalgrav3)
        
        invalgrav4 <- ""
        invalgrav4 <- ifelse(input$inval_tms_grav_item4=="Oui","<h5>La poursuite d'une activité manuelle présent une menace pour sa santé</h5>",invalgrav4)
        invalgrav4 <- ifelse(input$inval_tms_grav_item4=="Non","<h5>La poursuite d'une activité manuelle ne présente pas une menace pour sa santé</h5>",invalgrav4)
        
        invalgravspe <- paste0(invalgravspe, invalgrav0,invalgrav0b,invalgrav1,invalgrav2,invalgrav3,invalgrav4)
        
      }else{
        invalgravspe <- paste0(invalgravspe,"")
      }
      # Questionnaire ciblé inval Maladie dégénérative
      if(input$inval_activate_degeneratif=='Oui'){
        
        invalgrav0 <- "<h4>Questionnaire ciblé maladie dégénérative : </h4>"
        invalgrav0b <- paste0("<h5>",input$inval_tms_grav_obs,"</h5>")
        invalgrav1 <- ""
        invalgrav1 <- ifelse(input$inval_degen_grav_item1=="Oui","<h5>Il existe un traitement curatif en cours</h5>",invalgrav1)
        invalgrav1 <- ifelse(input$inval_degen_grav_item1=="Non","<h5>Il n'existe pas de traitement curatif en cours</h5>",invalgrav1)
        invalgrav2 <- ""
        invalgrav2 <- ifelse(input$inval_degen_grav_item2=="Oui","<h5>Il existe un traitement pallatif en cours</h5>",invalgrav2)
        invalgrav2 <- ifelse(input$inval_degen_grav_item2=="Non","<h5>Il n'existe pas de traitement pallatif en cours</h5>",invalgrav2)
        
        invalgrav3 <- ""
        invalgrav3 <- ifelse(input$inval_degen_grav_item3=="Oui","<h5>L'autonomie de l'assuré est menacée à court terme (<2 ans)</h5>",invalgrav3)
        invalgrav3 <- ifelse(input$inval_degen_grav_item3=="Non","<h5>L'autonomie de l'assuré n'est pas menacée à court terme (<2 ans)</h5>",invalgrav3)
        
        invalgrav4 <- ""
        invalgrav4 <- ifelse(input$inval_degen_grav_item4=="Oui","<h5>L'assuré est dépendant d'une ou de plusieurs personnes</h5>",invalgrav4)
        invalgrav4 <- ifelse(input$inval_degen_grav_item4=="Non","<h5>L'assuré n'est pas dépendant d'une ou de plusieurs personnes</h5>",invalgrav4)
        
        invalgravspe <- paste0(invalgravspe, invalgrav0,invalgrav0b,invalgrav1,invalgrav2,invalgrav3,invalgrav4)
        
      }else{
        invalgravspe <- paste0(invalgravspe,"")
      }
      
      synthese <- paste0("<h3>Evaluation de la capacité de travail</h3><h4>Questionnaire d'aide à la mise en invalidité</h4>",synthese_AMI,"<h4>Questionnaire ciblé d'aide à la mise en invalidité</h4>",inval0,inval1,inval2,inval3,invalgravspe)
    }
    
    HTML(synthese)
    
  })
  # Production des commentaires retour UI
    
  output$inval_AMI_item1_com1 <- renderUI({
      
    synthese <- ifelse(input$inval_AMI_item1==0, "Capacité normale, aucune limitation dans la vie quotidienne","")
    synthese <- ifelse(input$inval_AMI_item1==1, "Vue corrigée suffisante pour pouvoir se déplacer et s'orienter mais insuffisante pour reconnaitre les visages à une distance de plus de 15m.",synthese)
    synthese <- ifelse(input$inval_AMI_item1==2, "Même avec correction ne reconnaît pas les visages à une distance de plus de 10 mètres. Même avec correction ne peut pas lire à une distance habituelle.",synthese)
    synthese <- ifelse(input$inval_AMI_item1==3, "A une  atteinte du champ visuel ou des troubles de la vision qui impliquent des difficultés importantes et des risques dans les déplacements.",synthese)
    synthese <- ifelse(input$inval_AMI_item1==4, "Est malvoyant ou aveugle.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item2_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item2==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item2==1, "Peut avoir des difficultés à entendre ce qui est dit si quelqu'un parle normalement avec un bruit de fond",synthese)
    synthese <- ifelse(input$inval_AMI_item2==2, "N'entend pas ce qui est dit à une distance de plus de 3 mètres lorsque quelqu'un parle fort avec un bruit de fond.",synthese)
    synthese <- ifelse(input$inval_AMI_item2==3, "N'entend pas ce qui est dit à une distance de moins de 3 mètres lorsque quelqu'un parle fort avec un bruit de fond.",synthese)
    synthese <- ifelse(input$inval_AMI_item2==4, "Est sourd.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item3_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item3==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item3==1, "A besoin d'un peu plus de temps que la normale pour apprendre, comprendre et retenir une instruction pour une tâche de difficulté normale.",synthese)
    synthese <- ifelse(input$inval_AMI_item3==2, "A besoin de deux fois plus de temps que la normale pour apprendre, comprendre et retenir une instruction écrite pour une tâche de difficulté normale.",synthese)
    synthese <- ifelse(input$inval_AMI_item3==3, "A besoin d'une démonstration pour pouvoir effectuer une tâche. Ne peut pas apprendre, comprendre et retenir une instruction écrite pour une tâche simple.",synthese)
    synthese <- ifelse(input$inval_AMI_item3==4, "Ne peut pas apprendre et comprendre comment réaliser une tâche simple  même avec une démonstration.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item4_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item4==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item4==1, "Peut suivre un récit, une lecture, une conversation ou un programme télévisé plus d'une demi-heure.",synthese)
    synthese <- ifelse(input$inval_AMI_item4==2, "Peut suivre un récit, une lecture, une conversation ou un programme télévisé pendant 30 minutes maximum.",synthese)
    synthese <- ifelse(input$inval_AMI_item4==3, "Ne peut pas suivre un récit, une lecture, une conversation ou un programme télévisé pendant plus de 10 minutes.",synthese)
    synthese <- ifelse(input$inval_AMI_item4==4, "Ne peut pas suivre un récit, une lecture, une conversation ou un programme télévisé.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item5_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item5==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item5==1, "A besoin d'un peu plus de temps que la normale pour exécuter des tâches ou activités habituelles.",synthese)
    synthese <- ifelse(input$inval_AMI_item5==2, "A besoin de deux fois plus de temps que la normale pour exécuter des tâches ou activités habituelles.",synthese)
    synthese <- ifelse(input$inval_AMI_item5==3, "A souvent besoin d'aide pour terminer une tâche ou activité habituelle.",synthese)
    synthese <- ifelse(input$inval_AMI_item5==4, "Ne peut pas exécuter une tâche ou activité habituelle de façon autonome.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item6_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item6==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item6==1, "A rarement des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese)
    synthese <- ifelse(input$inval_AMI_item6==2, "A parfois des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese)
    synthese <- ifelse(input$inval_AMI_item6==3, "A souvent des difficultés pour enchainer et/ou coordonner des tâches ou activités habituelles.",synthese)
    synthese <- ifelse(input$inval_AMI_item6==4, "Ne peut pas enchainer et/ou coordonner des tâches ou activités habituelles.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item7_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item7==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item7==1, "Ressent du désagrément et de l'inquiétude en cas de changement de programme, des routines quotidiennes, ou lors de la prise de responsabilité.",synthese)
    synthese <- ifelse(input$inval_AMI_item7==2, "Ressent une forte frustration  (insatisfaction) lorsque les exigences de prestation sont occasionnellement plus élevées. A des difficultés pour gérer un changement imprévu dans la routine habituelle.",synthese)
    synthese <- ifelse(input$inval_AMI_item7==3, "Ne peut pas gérer un changement prévu de la routine habituelle.",synthese)
    synthese <- ifelse(input$inval_AMI_item7==4, "Ne supporte pas de changement de routine quotidienne.",synthese)
    HTML(synthese)
  })
  
  output$inval_AMI_item8_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item8==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item8==1, "S'exprime parfois (par oral ou par écrit) avec des fautes de langage, mais qui ne bloquent pas la possibilité de compréhension.",synthese)
    synthese <- ifelse(input$inval_AMI_item8==2, "S'exprime  (par oral ou par écrit) avec des fautes de langage, qui rendent difficile la compréhension.",synthese)
    synthese <- ifelse(input$inval_AMI_item8==3, "S'exprime (par oral ou par écrit) avec des fautes de langage, qui rendent très difficile la compréhension.",synthese)
    synthese <- ifelse(input$inval_AMI_item8==4, "Troubles du langage sévère, aphasie.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item9_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item9==0, "Capacité normale, aucune limitation dans la vie quotidienne. Peut s'incliner, se mettre à genoux ou accroupi et ramasser un papier au sol.","")
    synthese <- ifelse(input$inval_AMI_item9==1, "Peut s'incliner, se mettre à genoux ou accroupi et se relever avec quelques difficultés.",synthese)
    synthese <- ifelse(input$inval_AMI_item9==2, "Peut s'incliner, se mettre à genoux ou accroupi avec difficulté. Ne peut plus se relever seul du sol mais peut se lever d'une chaise.",synthese)
    synthese <- ifelse(input$inval_AMI_item9==3, "Ne peut pas s'incliner ni se relever sans l'aide d'une autre personne. Parvient seul mais avec difficulté à se lever d'une chaise.",synthese)
    synthese <- ifelse(input$inval_AMI_item9==4, "Ne peut pas se lever de la position assise sans l'aide d'une autre personne.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item10_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item10==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item10==1, "Ne peut rester debout plus de 30 minutes, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 60 minutes sans être obligé de se lever.",synthese)
    synthese <- ifelse(input$inval_AMI_item10==2, "Ne peut rester debout plus de 15 à 30 minutes, même avec la possibilté de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 30 minutes sans être obligé de se lever.",synthese)
    synthese <- ifelse(input$inval_AMI_item10==3, "Ne peut rester debout plusde 15 minutes, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 15 minutes sans être obligé de se lever.",synthese)
    synthese <- ifelse(input$inval_AMI_item10==4, "Ne peut rester debout plus de 10 minutes sans l'appui d'une autre personne, même avec la possibilité de bouger, sans être obligé de s'asseoir. Ne peut rester assis plus de 10 minutes sans être obligé de se lever.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item11_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item11==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item11==1, "Ne peut pas porter et déplacer un objet qui pèse plus de 20 kg (exemple: deux packs d'eau)",synthese)
    synthese <- ifelse(input$inval_AMI_item11==2, "Ne peut pas porter et déplacer un objet qui pèse plus de 10kg (exemple: un pack d'eau).",synthese)
    synthese <- ifelse(input$inval_AMI_item11==3, "Ne peut pas porter et déplacer un objet qui pèse plus de 5 kg (exemple : un sac pommes terres).",synthese)
    synthese <- ifelse(input$inval_AMI_item11==4, "Ne peut pas avec porter et déplacer une charge d'1kg (exemple: un paquet de sucre).",synthese)
    HTML(synthese)
  })
  
  output$inval_AMI_item12_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item12==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item12==1, "Ne peut pas sans difficulté saisir une épingle sur une table avec l'une ou l'autre main.",synthese)
    synthese <- ifelse(input$inval_AMI_item12==2, "Peut saisir une pièce de deux euros ou un petit objet rond (comme une balle de ping pong) avec une main, mais pas avec l'autre. Peut utiliser un crayon, un clavier et une souris d'ordinateur une demi-heure au maximum.",synthese)
    synthese <- ifelse(input$inval_AMI_item12==3, "Ne peut pas saisir une pièce de deux euros ou un petit objet rond (comme une balle de ping pong) avec l'une ou l'autre main. Ne peut utiliser ni crayon, ni clavier, ni souris d'ordinateur. Ne peut pas boutonner ou déboutonner une chemise ou un chemisier.",synthese)
    synthese <- ifelse(input$inval_AMI_item12==4, "Ne peut pas composer un numéro de téléphone. Ne peut pas tourner les pages d'un livre de l'une ou l'autre main. Ne réussit pas à utiliser une clé.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item13_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item13==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item13==1, "Ne peut pas sans difficulté changer une ampoule. Ne peut pas sans difficulté ouvrir une boîte de conserve.",synthese)
    synthese <- ifelse(input$inval_AMI_item13==2, "Peut lever un bras, mais pas l'autre, pour mettre un chapeau ou un bonnet. Peut ouvrir un robinet en forme d'étoile avec une main, mais pas avec l'autre. Ne peut pas tordre une serpillière. A des difficultés à tendre le bras pour verser une boisson.",synthese)
    synthese <- ifelse(input$inval_AMI_item13==3, "Ne peut pas lever le bras pour mettre un chapeau ou un bonnet. Ne peut pas ouvrir une porte avec une poignée.Ne peut pas pousser ou tirer une petite valise de 10 kg à roulettes. Ne peut pas verser une bouteille de lait d'un litre.",synthese)
    synthese <- ifelse(input$inval_AMI_item13==4, "Ne peut pas lever un bras pour mettre quelque chose dans la poche de sa veste. Ne peut pas étendre la main en arrière pour enfiler une veste. Ne peut pas ouvrir un robinet en forme d'étoile avec une main.",synthese)
    HTML(synthese)
  })
  
  output$inval_AMI_item14_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item14==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item14==1, "Ne peut pas marcher sur une longue distance (environ 1km) sans s'arrêter plusieurs fois ou éprouver un grand inconfort.",synthese)
    synthese <- ifelse(input$inval_AMI_item14==2, "Ne peut pas marcher sur une distance moyenne (environ 500 m) sur un sol régulier sans s'arrêter plusieurs fois ou éprouver un grand inconfort. Ne peut pas monter ou descendre un escalier sur plus d'un étage.",synthese)
    synthese <- ifelse(input$inval_AMI_item14==3, "Ne peut pas marcher sans aide à la marche sans s'arrêter plusieurs fois ou éprouver un grand inconfort. Ne peut pas monter et descendre un escalier sur un étage.",synthese)
    synthese <- ifelse(input$inval_AMI_item14==4, "Ne peut pas marcher.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item15_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item15==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item15==1, "A des difficultés pour se déplacer mais y arrive seul.",synthese)
    synthese <- ifelse(input$inval_AMI_item15==2, "Ne peut pas se déplacer de manière autonome à plus de 5 km du domicile.",synthese)
    synthese <- ifelse(input$inval_AMI_item15==3, "Ne peut pas se déplacer de manière autonome à plus de 500 m du domicile.",synthese)
    synthese <- ifelse(input$inval_AMI_item15==4, "Ne peut pas se déplacer de manière autonome à proximité du domicile.",synthese)
    HTML(synthese)
  })
  output$inval_AMI_item16_com1 <- renderUI({
    
    synthese <- ifelse(input$inval_AMI_item16==0, "Capacité normale, aucune limitation dans la vie quotidienne.","")
    synthese <- ifelse(input$inval_AMI_item16==1, "Évite parfois les contacts sociaux.  Agressivité : discrète et rare. Intolérance aux critiques : discrète.",synthese)
    synthese <- ifelse(input$inval_AMI_item16==2, "Evite souvent les contacts sociaux. Agressivité : modérée et peu fréquente (plusieurs fois par mois). Intolérance aux critiques : modérée. Respect des règles sociales : difficulté modérée.",synthese)
    synthese <- ifelse(input$inval_AMI_item16==3, "Évite le contact avec les autres autant que possible. Agressivité: importante et fréquente (plusieurs fois par semaine). Intolérance aux critiques: importante et fréquente. Respect des règles sociales: difficulté importante.",synthese)
    synthese <- ifelse(input$inval_AMI_item16==4, "Contact impossible avec les autres. Agressivité empêchant toute activité suivie. Respect des règles sociales : impossible.",synthese)
    HTML(synthese)
  })
  
  # Module d'aide à la décision ####
  
  # Alerte module d'aide à la décision
  output$decisionmaking_alertes1 <- renderUI({
    
    synthese <- ""
    synthese_2 <- ""
    
    synthese <- ifelse(input$cadreprestation=="Non renseigné","<strong><font color='red'>Module d'aide à la décision non utilisable car cadre de prestation non renseigné</font></strong>","")
    
    if(input$cadreprestation!="Non renseigné"){
      synthese <- ifelse(input$cadreprestation=="Accident du travail" & input$date_AT=="","<strong><font color='red'>Module d'aide à la décision non utilisable car date de l'accident du travail non renseignée</font></strong>",synthese)
      
      synthese <- ifelse(input$cadreprestation=="Maladie professionnelle" & input$date_MP=="","<strong><font color='red'>Module d'aide à la décision non utilisable car date de la reconnaissance MP non renseignée</font></strong>",synthese)
      
      synthese_2 <- ifelse(input$objectif_convoc=="Fixation de séquelles" & ( (input$cadreprestation=="Accident du travail" & input$date_AT_4=="")  | (input$cadreprestation=="Maladie professionnelle" | input$date_MP_4=="")  ), "<strong><font color='red'>Module d'aide à la décision non utilisable car date du CFD non renseignée</font></strong>",synthese_2 )
      
    }
        
    HTML(paste(synthese, synthese_2, sep="</br>" ))
    })
  
  # Module Aide à la décision - Invalidité ####
  
  output$decisionmaking_inval1 <- renderUI({
    synthese <- ""
    if(input$date_AS2=="" & input$date_AS1==""){
      synthese <- "<strong><font color='red'>Date de PIT et de forclusion manquantes</font></strong>"
    }else{
      FORCLUSION <- ifelse(input$date_AS1!="" & input$date_AS2=="",dmy(input$date_AS1)+3*365,dmy(input$date_AS2))
      PIT <- ifelse(input$date_AS2!="" & input$date_AS1=="",dmy(input$date_AS2)-3*365,dmy(input$date_AS1))
      TODAY <- as.numeric(ymd(Sys.Date()))
      STATUS <- ifelse(FORCLUSION-TODAY<0,2,0)
      STATUS <- ifelse( (0<FORCLUSION-TODAY) & (FORCLUSION-TODAY<6),2,STATUS)
      if(input$invalidite_activate=="Oui"){
        
        DECISION <- ifelse(input$invalidite_appreciation_MC=="Invalidité catégorie 2 (DEUX)" & STATUS>0,"Invalidité 2","")
        DECISION <- ifelse(input$invalidite_appreciation_MC=="Invalidité catégorie 2 (DEUX)" & STATUS<0,"Invalidité 2",DECISION)
        DECISION <- ifelse(input$invalidite_appreciation=="Invalidité catégorie 2 (DEUX)" & STATUS==0 & input$traitement_prevu=="Traitements actifs susceptibles d'améliorer la situation", "<p>Invalidité 2 sous réserve car des soins sont susceptible d'améliorer l'état de santé de l'assuré</p>",DECISION)
        DECISION <- ifelse(input$invalidite_appreciation=="Invalidité catégorie 2 (DEUX)" & STATUS==0 & input$traitement_prevu=="Aucun ou soins d'entretien" & input$traitement_travail=="Oui pour un temps plein ou un travail non adapté", "<p>Invalidité 2 sous réserve car les soins n'empechent pas de poursuivre un travail sur poste adapté</p>",DECISION)
        DECISION <- ifelse(input$invalidite_appreciation=="Invalidité catégorie 1 (UN)" & STATUS>0, "Invalidité 1",DECISION)
        DECISION <- ifelse(input$invalidite_appreciation=="Invalidité catégorie 1 (UN)" & STATUS==0 & input$traitement_prevu=="Traitements actifs susceptibles d'améliorer la situation", "<p>Invalidité 1 sous réserve car des soins sont susceptible d'améliorer l'état de santé de l'assuré</p>",DECISION)
        DECISION <- ifelse(input$invalidite_appreciation=="Invalidité catégorie 1 (UN)" & STATUS==0 & input$traitement_prevu=="Aucun ou soins d'entretien" & input$traitement_travail=="Oui pour un temps plein ou un travail non adapté", "<p>Invalidité 1 sous réserve car les soins n'empechent pas de poursuivre un travail sur poste adapté</p>",DECISION)
      
        COMMENTAIRE <- ifelse(input$invalidite_appreciation_MC=="Invalidité catégorie 2 (DEUX)", "<p>L'examen des capacités restantes met en évidence une perte de la capacité de gain supérieure aux deux tiers et la capacité résiduelle de travail est nulle, l'état de l'assuré relève de l'invalidité catégorie 2.</p>","")
        COMMENTAIRE <- ifelse(input$invalidite_appreciation_MC=="Invalidité catégorie 1 (UN)", "<p>L'examen des capacités restantes met en évidence une perte de la capacité de gain supérieure aux deux tiers et il persiste une capacité de travail, l'état de l'assuré relève de l'invalidité catégorie 1.</p>",COMMENTAIRE)
        COMMENTAIRE <- ifelse(input$invalidite_appreciation_MC=="Aptitude à un travail quelconque", "<p>L'examen des capacités restantes ne met pas en évidence une perte de la capacité de gain supérieure aux deux tiers. Il existe une aptitude à un travail quelconque, l'assuré ne relève pas de l'invalidité</p>",COMMENTAIRE)
        
        synthese_inval0 <- "<h3>ARGUMENTS EN <strong><font color='greedn'>FAVEUR</font></strong> D'UNE MISE EN INVALIDITE :</h3>"
        synthese_inval1 <- ifelse(input$invalidite_appreciation_MC %in% c("Invalidité catégorie 1 (UN)","Invalidité catégorie 2 (DEUX)"),"<h5>Vous pensez que cette assuré relève globalement de l'invalidité</h5>","")
        synthese_inval2 <- ifelse(input$inval_AMI_item1>=3 | input$inval_AMI_item2>=3 | input$inval_AMI_item3>=3 | input$inval_AMI_item4>=3 | input$inval_AMI_item5>=3 | input$inval_AMI_item6>=3 | input$inval_AMI_item7>=3 | input$inval_AMI_item8>=3 | input$inval_AMI_item9>=3 | input$inval_AMI_item10>=3 | input$inval_AMI_item11>=3 | input$inval_AMI_item12>=3 | input$inval_AMI_item13>=3 | input$inval_AMI_item14>=3 | input$inval_AMI_item15>=3 | input$inval_AMI_item16>=3, "<h5>Il existe au moins un domaine de l'AMI avec une déficience grave</h5>","")
        
        # paste0(paste0("input$inval_AMI_item",c(1:16),"<2"),collapse = " & ")
        
        synthese_inval3 <- "<h3>ARGUMENTS EN <strong><font color='red'>DEFAVEUR</font></strong> D'UNE MISE EN INVALIDITE :</h3>"
        
        synthese_inval4 <- ifelse(input$inval_AMI_item1<2 & input$inval_AMI_item2<2 & input$inval_AMI_item3<2 & input$inval_AMI_item4<2 & input$inval_AMI_item5<2 & input$inval_AMI_item6<2 & input$inval_AMI_item7<2 & input$inval_AMI_item8<2 & input$inval_AMI_item9<2 & input$inval_AMI_item10<2 & input$inval_AMI_item11<2 & input$inval_AMI_item12<2 & input$inval_AMI_item13<2 & input$inval_AMI_item14<2 & input$inval_AMI_item15<2 & input$inval_AMI_item16<2,"<h5>Il n'existe aucun item de l'AMI ayant une déficience supérieure ou égale à modérée</h5>" ,"")
        synthese_inval5 <- ifelse(input$traitement_prevu=="Traitements actifs susceptibles d'améliorer la situation", "<h5>Vous avez indiqué que les traitements de l'assurés étaient susceptibles d'améliorer son état de santé</h5>","")
        
        synthese <- paste0(synthese_inval0,synthese_inval1,synthese_inval2,synthese_inval3,synthese_inval4,synthese_inval5)
      }else{
          synthese <- "<strong><font color='red'>L'évaluation de la capacité de travail est manquante</font></strong>"
        }
      
      }
    
    
    HTML(synthese)
  })
  
  # Synthse - Examen de la dépression ####
  
  output$depression_synthese <- renderUI({
    
    synthese <- ""
    # browser()
    if(input$depression_activate=="Oui"){
      psy0 <- "<h3> Examen psychiatrique</h3><h4>Eléments d'appréciation global<h4>"
      psy1 <- paste0("<h5>",input$psy_obs,"</h5>")
      psy2 <- ifelse(input$psy_obj_item1=="Oui","<h5>Il existe un suivi psychiatrique spécialisé</h5>","")
      psy3 <- ifelse(input$psy_obj_item2=="Oui","<h5>L'assuré a un traitement de fond</h5>","")
      psy4 <- ifelse(input$psy_obj_item3=="Oui","<h5>Il existe une incurie</h5>","")
      psy5 <- ifelse(input$psy_obj_item4=="Oui","<h5>Il existe des signes de mutilation</h5>","")
      #
      psy2 <- ifelse(input$psy_obj_item1=="Non","<h5>Il n'existe pas de suivi psychiatrique spécialisé</h5>",psy2)
      psy3 <- ifelse(input$psy_obj_item2=="Non","<h5>L'assuré n'a pas de traitement de fond</h5>",psy3)
      psy4 <- ifelse(input$psy_obj_item3=="Non","<h5>Il n'existe pas d'incurie</h5>",psy4)
      psy5 <- ifelse(input$psy_obj_item4=="Non","<h5>Il n'existe pas de signe de mutilation</h5>",psy5)
      #
      psy6 <- "<h4>Recherche de signe de gravité psychiatrique :<h4>"
      psy7 <- paste0("<h5>","Isolement social : ",input$psy_grav_item1,"</h5>")
      psy8 <- paste0("<h5>","Contexte de deuil : ",input$psy_grav_item2,"</h5>")
      psy9 <- paste0("<h5>","Antécédents de tentatives de suicide : ",input$psy_grav_item3,"</h5>")
      psy10 <- paste0("<h5>","Existence d’une planification de suicide : ",input$psy_grav_item4,"</h5>")
      psy11 <- paste0("<h5>","Maladie chronique douloureuse ou handicap : ",input$psy_grav_item5,"</h5>")
      psy12 <- paste0("<h5>","Utilisation de substances toxiques (alcool, drogue) : ",input$psy_grav_item6,"</h5>")
      #
      psy13 <- "<h4>Evaluation de la sévérité de la dépression par le questionnaire PHC (<em>roenke K, Spitzer RL, Williams JB. The PHQ-9: validity of a brief depression severity measure. J Gen Intern Med 2001;16(9):606-13 (41)</em>)</h4>"
      psy14 <- paste0("<h5>","1. Une diminution marquée d’intérêt ou de plaisir dans vos activités ? : ",input$PHQ9_item1,"</h5>")
      psy15 <- paste0("<h5>","2. Un sentiment d’abattement, de dépression ou de perte d’espoir ? : ",input$PHQ9_item2,"</h5>")
      psy16 <- paste0("<h5>","3. Difficultés à vous endormir, à rester endormi(e), ou au contraire une tendance à trop dormir ? : ",input$PHQ9_item3,"</h5>")
      psy17 <- paste0("<h5>","4. Une sensation de fatigue ou de manque d’énergie ? : ",input$PHQ9_item4,"</h5>")
      psy18 <- paste0("<h5>","5. Un manque ou un excès d’appétit ? : ",input$PHQ9_item5,"</h5>")
      psy19 <- paste0("<h5>","6. Une mauvaise opinion de vous-même : l’impression que vous êtes un(e) raté(e) ou que vous vous êtes laissé(e) aller ou que vous avez négligé votre famille ? : ",input$PHQ9_item6,"</h5>")
      psy20 <- paste0("<h5>","7. De la peine à vous concentrer, par exemple pour lire le journal ou regarder la télévision. : ",input$PHQ9_item7,"</h5>")
      psy21 <- paste0("<h5>","8. L’impression de parler ou de vous déplacer si lentement que cela se remarquait – ou, au contraire, une fébrilité ou agitation telle que vous ne teniez pas en place ? : ",input$PHQ9_item8,"</h5>")
      psy22 <- paste0("<h5>","9. Penser vous que vous préféreriez être mort ou penser à vous faire du mal ? : ",input$PHQ9_item9,"</h5>")
      
      synthese <- paste0(psy0,psy1,psy2,psy3,psy4,psy5,psy6,psy7,psy8,psy9,psy10,psy11,psy12,psy13,psy14,psy15,psy16,psy17,psy18,psy19,psy20,psy21,psy22)
    }
    
    HTML(synthese)
  })
  
  # Module Aide à la décision -- Evaluation des lesions bareme AT ####
  
  observeEvent(input$btn, {
    
    lesion <- as.data.frame(matrix(NA, nrow=1,ncol=4))
    colnames(lesion) <- c("Module","Région","Lésion identifiée","IP")
    
    # Evaluation des lesions bareme AT ---- Amputations #### 
    if(input$activate_amputation=="Oui"){
      if( input$amputation_epaule_g_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation interscapulothoracique gauche chez un gaucher avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os sur bras dominant","95%"))
      }
      if( input$amputation_epaule_g_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation interscapulothoracique gaucher chez un droitier avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os sur bras dominant","85%"))
      }
      if( input$amputation_epaule_d_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation interscapulothoracique droite chez un gaucher avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os sur bras dominant","85%"))
      }
      if( input$amputation_epaule_d_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation interscapulothoracique droite chez un droitier avec résection totale ou partielle de la clavicule et de l'omoplate, ou de l'un de ces deux os sur bras dominant","95%"))
      }
      #
      if( input$amputation_epaule_g_2=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation de l'épaule gauche chez un gaucher","95%"))
      }
      if( input$amputation_epaule_g_2=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation de l'épaule gaucher chez un droitier","85%"))
      }
      if( input$amputation_epaule_d_2=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation de l'épaule droite chez un gaucher","85%"))
      }
      if( input$amputation_epaule_d_2=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation de l'épaule droite chez un droitier","95%"))
      }
      #
      if( input$amputation_bras_g_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras gauche au tiers supérieur chez un gaucher","95%"))
      }
      if( input$amputation_bras_g_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras gauche au tiers supérieur chez un droitier","80%"))
      }
      if( input$amputation_bras_d_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras droit au tiers supérieur chez un gaucher","80%"))
      }
      if( input$amputation_bras_d_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras droit au tiers supérieur chez un droitier","95%"))
      }
      #
      if( input$amputation_bras_g_2=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras gauche au tiers moyen ou inférieur chez un gaucher","90%"))
      }
      if( input$amputation_bras_g_2=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras gauche au tiers moyen ou inférieur chez un droitier","80%"))
      }
      if( input$amputation_bras_d_2=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras droit au tiers moyen ou inférieur chez un gaucher","80%"))
      }
      if( input$amputation_bras_d_2=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation du bras droit au tiers moyen ou inférieur chez un droitier","90%"))
      }
      #
      if( input$amputation_bras_g_3=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation du coude, avant-bras au tiers supérieur gauche chez un gaucher","90%"))
      }
      if( input$amputation_bras_g_3=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation du coude, avant-bras au tiers supérieur gauche chez un droitier","80%"))
      }
      if( input$amputation_bras_d_3=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation du coude, avant-bras au tiers supérieur droit chez un gaucher","80%"))
      }
      if( input$amputation_bras_d_3=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Désarticulation du coude, avant-bras au tiers supérieur droit chez un droitier","90%"))
      }
      #
      if( input$amputation_main_g_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation métacarpienne conservant une palette gauche chez un gaucher","70%"))
      }
      if( input$amputation_main_g_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation métacarpienne conservant une palette gauche chez un droitier","60%"))
      }
      if( input$amputation_main_d_1=="Oui" & input$dominance!="Droitier"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation métacarpienne conservant une palette droite chez un gaucher","60%"))
      }
      if( input$amputation_main_d_1=="Oui" & input$dominance!="Gaucher"){
        lesion <- rbind(lesion, c("Amputation","Membre supérieur","Amputation métacarpienne conservant une palette droite chez un droitier","70%"))
      }
      #
      if(input$amputation_jambe_g_1=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation inter-ilio-abdominale gauche","100%"))
      }
      if(input$amputation_jambe_d_1=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation inter-ilio-abdominale droite","100%"))
      }
      
      if(input$amputation_jambe_g_2=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation de la hanche gauche","100%"))
      }
      if(input$amputation_jambe_d_2=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation de la hanche droite","100%"))
      }
      
      if(input$amputation_jambe_g_3=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation inter-trochantérienne gauche","100%"))
      }
      if(input$amputation_jambe_d_3=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation inter-trochantérienne droite","100%"))
      }
      if(input$amputation_jambe_g_4=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation sous-trochantérienne gauche","100%"))
      }
      if(input$amputation_jambe_d_4=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation sous-trochantérienne droite","100%"))
      }
      if(input$amputation_jambe_g_5=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers moyen ou au tiers inférieur de la cuisse gauche","100%"))
      }
      if(input$amputation_jambe_d_5=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers moyen ou au tiers inférieur de la cuisse droite","100%"))
      }
      #
      if(input$amputation_jambe_g_6=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation du genou gauche","80%"))
      }
      if(input$amputation_jambe_d_6=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation du genou droit","80%"))
      }
      if(input$amputation_jambe_g_7=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers supérieur de la jambe gauche","70%"))
      }
      if(input$amputation_jambe_d_7=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers supérieur de la jambe droite","70%"))
      }
      if(input$amputation_jambe_g_8=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers moyen ou inférieur de la jambe gauche","70%"))
      }
      if(input$amputation_jambe_d_8=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation au tiers moyen ou inférieur de la jambe droite","70%"))
      }
      if(input$amputation_jambe_g_9=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation tibio-tarsienn gauche","50%"))
      }
      if(input$amputation_jambe_d_9=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation tibio-tarsienn droite","50%"))
      }
      if(input$amputation_jambe_g_10=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du pied gauche, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier (avec mouvement du pied restant satisfaisant et sans bascule en varus)","40%"))
      }
      if(input$amputation_jambe_d_10=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du pied droit, avec conservation de la partie postérieure du calcanéum avec bon appui talonnier (avec mouvement du pied restant satisfaisant et sans bascule en varus)","40%"))
      }
      if(input$amputation_jambe_g_11=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation médio-tarsienne de Chopart gauche","45%"))
      }
      if(input$amputation_jambe_d_11=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Désarticulation médio-tarsienne de Chopart droit","45%"))
      }
      if(input$amputation_jambe_g_12=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation transmétatarsienne de l'avant-pied gauche","30%"))
      }
      if(input$amputation_jambe_d_12=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation transmétatarsienne de l'avant-pied droit","30%"))
      }
      if(input$amputation_jambe_g_13=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Perte de cinq orteils gauche","25%"))
      }
      if(input$amputation_jambe_d_13=="Oui"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Perte de cinq orteils droit","25%"))
      }
      if(input$amputation_jambe_d_14=="Les deux phalanges avec le métatarsien"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation deux phalanges du gros orteil droit avec le métatarsien","20%"))
      }
      if(input$amputation_jambe_g_14=="Les deux phalanges avec le métatarsien"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation deux phalanges du gros orteil gauche droit avec le métatarsien","20%"))
      }
      if(input$amputation_jambe_d_14=="Les deux phalanges"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation deux phalanges du gros orteil droit sans le métatarsien","12%"))
      }
      if(input$amputation_jambe_g_14=="Les deux phalanges"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation deux phalanges du gros orteil gauche droit sans le métatarsien","12%"))
      }
      if(input$amputation_jambe_d_14=="Phalange distale"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation de la phalange distale du gros orteil droit ","5%"))
      }
      if(input$amputation_jambe_g_14=="Phalange distale"){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation de la phalange distale du gros orteil gauche","5%"))
      }
      #
      if(any(input$amputation_jambe_d_15=="Amputation d'un orteil")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation d'un orteil à droite","2%"))
      }
      if(any(input$amputation_jambe_g_15=="Amputation d'un orteil")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation d'un orteil à gauche","2%"))
      }
      if(any(input$amputation_jambe_d_15=="Deuxième ou cinquième orteil avec leur métatarsien")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du deuxième ou cinquième orteil à droite avec leur métatarsien","10%"))
      }
      if(any(input$amputation_jambe_g_15=="Deuxième ou cinquième orteil avec leur métatarsien")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du deuxième ou cinquième orteil à gauche avec leur métatarsien","10%"))
      }
      if(any(input$amputation_jambe_d_15=="Troisième ou quatrième orteil avec leur métatarsien")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du troisième ou quatrième orteil à gauche avec leur métatarsien","0% (ZERO)"))
      }
      if(any(input$amputation_jambe_g_15=="Troisième ou quatrième orteil avec leur métatarsien")){
        lesion <- rbind(lesion, c("Amputation","Membre inférieur","Amputation du troisième ou quatrième orteil à gauche avec leur métatarsien","0% (ZERO)"))
      }
      
    }
    
    # Evaluation des lesions bareme MP ---- Dépression ####
    if(input$depression_activate=="Oui"){
    phq_fun <- function(x,y=c("Jamais","Plusieurs jours","Plus de la moitié des jours","Presque tous les jours")){
      0+1*(x==y[2])+2*(x==y[3])+3*(x==y[4])
    }
    tbcomp_score <- phq_fun(input$psy_mod_1,c("Non ou minimes","Oui modérés","Oui sévères","Oui très sévères"))
    anxiete_score <- phq_fun(input$psy_mod_3,c("Non ou minimes","Oui avec évitement ciblés","Oui avec pantophobie","XXX"))
    PHQ_score <- 0
    if(input$PHQ9_item0=="Oui"){
      PHQ_score <-phq_fun(input$PHQ9_item1)+phq_fun(input$PHQ9_item2)+phq_fun(input$PHQ9_item3)+phq_fun(input$PHQ9_item4)+phq_fun(input$PHQ9_item5)+phq_fun(input$PHQ9_item6)+phq_fun(input$PHQ9_item7)+phq_fun(input$PHQ9_item8)+phq_fun(input$PHQ9_item9)
    }
    if(PHQ_score<5){
      if(anxiete_score==1){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Troubles anxieux","5%"))
        
      }
    }
    if(PHQ_score>4 & PHQ_score<10){
      if(anxiete_score==0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression traitée","10%"))
        
      }
      if(anxiete_score>0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression traitée avec troubles anxieux","15%"))
        
      }
      if(anxiete_score>0 & tbcomp_score>0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression traitée avec troubles anxieux et troubles du comportement","20%"))
        
      }
    }
    if(PHQ_score>9 & PHQ_score<15){
      if(anxiete_score==0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression modérément sévère","15%"))
        
      }
      if(anxiete_score>0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression modérément sévère avec troubles anxieux","20%"))
        
      }
      if(anxiete_score>0 & tbcomp_score>0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression modérément sévère avec troubles anxieux et troubles du comportement","25%"))
        
      }
    }
    if(PHQ_score>14 & PHQ_score<20){
      if(anxiete_score==0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression sévère","20%"))
        
      }
      if(anxiete_score>0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression modérément sévère avec troubles anxieux","25%"))
        
      }
      if(anxiete_score>0 & tbcomp_score>0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression modérément sévère avec troubles anxieux et troubles du comportement","30%"))
        
      }
      
    }
    if(PHQ_score>19){
      
      if(anxiete_score==0 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère","40%"))
        
      }
      if(anxiete_score==1 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression sévère avec troubles anxieux","45%"))
        
      }
      if(anxiete_score==2 & tbcomp_score==0){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression sévère avec troubles anxieux pantophobique","50%"))
        
      }
      
      if(anxiete_score==1 & tbcomp_score==1){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux et troubles du comportements","55%"))
        
      }
      if(anxiete_score==2 & tbcomp_score==1){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux pantophobique et troubles du comportements","60%"))
        
      }
      if(anxiete_score==1 & tbcomp_score==2){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux et troubles du comportements sévères","65%"))
        
      }
      if(anxiete_score==2 & tbcomp_score==2){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux pantophobique et troubles du comportements sévères","70%"))
        
      }
      if(anxiete_score==1 & tbcomp_score==3){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux et troubles du comportements très sévères","80%"))
        
      }
      if(anxiete_score==2 & tbcomp_score==3){
        lesion <- rbind(lesion, c("Psychiatrie","Dépression","Dépression très sévère avec troubles anxieux pantophobique et troubles du comportements très sévères","90%"))
        
      }
      
    }
    
    }
    
      # Evaluation des lesions bareme AT ---- Epaule ####
      if(input$epaule_activate %in% c("Oui à gauche","Oui les deux")){
        angle_1 <- ifelse(input$mob_act_g1>=0,input$mob_act_g1,NA)
        angle_1b <- ifelse(input$mob_pas_g2>=0,input$mob_pas_g2,NA)
        angle_2 <- ifelse(input$mob_act_g3>=0,input$mob_act_g3,NA)
        angle_2b <- ifelse(input$mob_pas_g4>=0,input$mob_pas_g4,NA)
        
        severite_epaule <- NA
        if(!is.na(angle_1) | !is.na(angle_1b)){
          if(max(angle_1,angle_1b, na.rm = T)<=160){
            severite_epaule <- 1
          }
          if(max(angle_1,angle_1b, na.rm = T)<=90){
            severite_epaule <- 2
          }
          if(max(angle_1,angle_1b, na.rm = T)<=20){
            severite_epaule <- 3
          }
          if(max(angle_1,angle_1b, na.rm = T)<=5){
            severite_epaule <- 4
          }
        }
        if(!is.na(angle_2) | !is.na(angle_2b)){
          #
          if(max(angle_2,angle_2b, na.rm = T)<=150){
            severite_epaule <- 1
          }
          if(max(angle_2,angle_2b, na.rm = T)<=90){
            severite_epaule <- 2
          }
          if(max(angle_2,angle_2b, na.rm = T)<=60){
            severite_epaule <- 3
          }
          if(max(angle_2,angle_2b, na.rm = T)<=5){
            severite_epaule <- 4
          }
        }
        # 
        if(severite_epaule==1){
          if(input$dominance!="Droitier"){
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Limitation légère de tous les mouvements de l'épaule gauche chez un gaucher","12%"))
          }else{
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Limitation légère de tous les mouvements de l'épaule gauche chez un droitier","8%"))
          } 
          
          }
        if(severite_epaule==2){
          if(input$dominance!="Droitier"){
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Limitation moyenne de tous les mouvements de l'épaule gauche chez un gaucher","20%"))
          }else{
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Limitation moyenne de tous les mouvements de l'épaule gauche chez un droitier","15%"))
          }        
          }
        if(severite_epaule==3){
          if(input$dominance!="Droitier"){
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Blocage de l'épaule gauche, avec omoplate mobile chez un gaucher","40%"))
          }else{
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Blocage de l'épaule gauche, avec omoplate mobile chez un droitier","30%"))
          }
        }
        if(severite_epaule==4){
          if(input$dominance!="Droitier"){
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Blocage de l'épaule gauche, omoplate bloquée chez un gaucher","55%"))
          }else{
            lesion <- rbind(lesion, c("Epaules","Epaule Gauche","Blocage de l'épaule gauche, omoplate bloquée chez un droitier","45%"))
          }
          
        }
      }

        if(input$epaule_activate %in% c("Oui à droite","Oui les deux")){
          angle_1 <- ifelse(input$mob_act_d1>=0,input$mob_act_d1,NA)
          angle_1b <- ifelse(input$mob_pas_d2>=0,input$mob_pas_d2,NA)
          angle_2 <- ifelse(input$mob_act_d3>=0,input$mob_act_d3,NA)
          angle_2b <- ifelse(input$mob_pas_d4>=0,input$mob_pas_d4,NA)
          
          severite_epaule <- NA
          if(!is.na(angle_1) | !is.na(angle_1b)){
            if(max(angle_1,angle_1b, na.rm = T)<=160){
              severite_epaule <- 1
            }
            if(max(angle_1,angle_1b, na.rm = T)<=90){
              severite_epaule <- 2
            }
            if(max(angle_1,angle_1b, na.rm = T)<=20){
              severite_epaule <- 3
            }
            if(max(angle_1,angle_1b, na.rm = T)<=5){
              severite_epaule <- 4
            }
          }
          if(!is.na(angle_2) | !is.na(angle_2b)){
            #
            if(max(angle_2,angle_2b, na.rm = T)<=150){
              severite_epaule <- 1
            }
            if(max(angle_2,angle_2b, na.rm = T)<=90){
              severite_epaule <- 2
            }
            if(max(angle_2,angle_2b, na.rm = T)<=60){
              severite_epaule <- 3
            }
            if(max(angle_2,angle_2b, na.rm = T)<=5){
              severite_epaule <- 4
            }
          }
          # 
          if(severite_epaule==1){
            if(input$dominance!="Droitier"){
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Limitation légère de tous les mouvements de l'épaule droite chez un gaucher","8%"))
            }else{
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Limitation légère de tous les mouvements de l'épaule droite chez un droitier","12%"))
            } 
            
          }
          if(severite_epaule==2){
            if(input$dominance!="Droitier"){
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Limitation moyenne de tous les mouvements de l'épaule droite chez un gaucher","15%"))
            }else{
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Limitation moyenne de tous les mouvements de l'épaule droite chez un droitier","20%"))
            }        
          }
          if(severite_epaule==3){
            if(input$dominance!="Droitier"){
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Blocage de l'épaule droite, avec omoplate mobile chez un gaucher","30%"))
            }else{
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Blocage de l'épaule droite, avec omoplate mobile chez un droitier","40%"))
            }
          }
          if(severite_epaule==4){
            if(input$dominance!="Droitier"){
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Blocage de l'épaule droite, omoplate bloquée chez un gaucher","45%"))
            }else{
              lesion <- rbind(lesion, c("Epaules","Epaule Droite","Blocage de l'épaule droite, omoplate bloquée chez un droitier","55%"))
            }
            
          }
          
        
        }
    
    # Evaluation des lesions bareme MP ---- Coudes ####
    if(input$epicondylite_activate!="Non"){
      if(input$epicondylite_activate %in% c("Oui les deux","Oui à droite")){
        if(input$coude_d_mob1 == 'Limitation des mouvements de flexion-extension'){
          if(input$coude_d_mob2=="Mouvements conservés de 70 à 145 degrés"){
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 70 à 145 degrés du coude droit chez un droitier","10%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 70 à 145 degrés du coude droit chez un gaucher","8%"))
            } 
          }
          if(input$coude_d_mob2=="Mouvements conservés autour de l'angle favorable (60-100 degrés)"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés autour de l'angle favorable (60-100 degrés) du coude droit chez un droitier","20%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements cconservés autour de l'angle favorable (60-100 degrés) du coude droit chez un gaucher","15%"))
            }
            
          }
          if(input$coude_d_mob2=="Mouvements conservés de 0 à 70 degrés"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 0 à 70 degrés du coude droit chez un droitier","25%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 0 à 70 degrés du coude droit chez un gaucher","22%"))
            }
            
          }

        }
        if(input$coude_d_mob1 == 'Blocage de la flexion-extension'){
          
          if(input$coude_d_mob3=="Angle favorable (60-100)"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle favorable (60-100) du coude droit chez un droitier","25%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle favorable (60-100) du coude droit chez un gaucher","22%")) 
            }
            
          }
          if(input$coude_d_mob3=="Angle défavorable (de 100 à 145 ou de 0 à 60)"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle défavorable (de 100 à 145 ou de 0 à 60) du coude droit chez un droitier","40%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle défavorable (de 100 à 145 ou de 0 à 60) du coude droit chez un gaucher","35%")) 
            }
            
          }
          
        }
        
        
      }
      if(input$epicondylite_activate %in% c("Oui les deux","Oui à gauche"))
      
        if(input$coude_g_mob1 == 'Limitation des mouvements de flexion-extension'){
          if(input$coude_g_mob2=="Mouvements conservés de 70 à 145 degrés"){
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 70 à 145 degrés du coude droit chez un droitier","10%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 70 à 145 degrés du coude droit chez un gaucher","8%"))
            } 
          }
          if(input$coude_g_mob2=="Mouvements conservés autour de l'angle favorable (60-100 degrés)"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés autour de l'angle favorable (60-100 degrés) du coude droit chez un droitier","20%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements cconservés autour de l'angle favorable (60-100 degrés) du coude droit chez un gaucher","15%"))
            }
            
          }
          if(input$coude_g_mob2=="Mouvements conservés de 0 à 70 degrés"){
            
            if(input$dominance!="Gaucher"){
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 0 à 70 degrés du coude droit chez un droitier","25%"))
            }else{
              lesion <- rbind(lesion, c("Coude","Coude Droit","Limitation des mouvements de flexion-extension avec mouvements conservés de 0 à 70 degrés du coude droit chez un gaucher","22%"))
            }
            
          }
          
        }
      if(input$coude_g_mob1 == 'Blocage de la flexion-extension'){
        
        if(input$coude_g_mob3=="Angle favorable (60-100)"){
          
          if(input$dominance!="Gaucher"){
            lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle favorable (60-100) du coude droit chez un droitier","25%"))
          }else{
            lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle favorable (60-100) du coude droit chez un gaucher","22%")) 
          }
          
        }
        if(input$coude_g_mob3=="Angle défavorable (de 100 à 145 ou de 0 à 60)"){
          
          if(input$dominance!="Gaucher"){
            lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle défavorable (de 100 à 145 ou de 0 à 60) du coude droit chez un droitier","40%"))
          }else{
            lesion <- rbind(lesion, c("Coude","Coude Droit","Blocage de flexion-extension avec un angle défavorable (de 100 à 145 ou de 0 à 60) du coude droit chez un gaucher","35%")) 
          }
          
        }
        
      }
        
    }
    
    if(input$rachisdl_activate=="Oui"){
      
      if(input$snp_mi_g_glob=="altéré"){
        
        if("Paralysie totale d'un membre inférieur flasque" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie totale du membre inférieur gauche flasque","75%"))
        }
        if("Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie complète du nerf sciatique gauche (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)","60%"))
        }
        if("Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie du nerf sciatique poplité externe gauche (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)","30%"))
        }
        if("Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaore","Membre inférieur gauche","Paralysie du nerf sciatique poplité interne gauche (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)","30%"))
        }
        if("Paralysie du nerf crural (quadriceps)" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie du nerf crural gauche (quadriceps)","40%"))
        }
        if("Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)" %in% input$snp_mi_g1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie du nerf obturateur gauche (pectiné, obturateur externe, adducteur)","15%"))
        }
        
        if("Paralysie totale d'un membre inférieur flasque" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie modéré (4/5) du membre inférieur gauche flasque","56%"))
        }
        if("Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie modéré (4/5) du nerf sciatique gauche (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)","45%"))
        }
        if("Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaore","Membre inférieur gauche","Paralysie modéré (4/5) du nerf sciatique poplité externe gauche (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)","23%"))
        }
        if("Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie modéré (4/5) du nerf sciatique poplité interne gauche (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)","23%"))
        }
        if("Paralysie du nerf crural (quadriceps)" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie modéré (4/5) du nerf crural gauche (quadriceps)","30%"))
        }
        if("Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)" %in% input$snp_mi_g2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur gauche","Paralysie modéré (4/5) du nerf obturateur gauche (pectiné, obturateur externe, adducteur)","11%"))
        }
        
      }
      
      if(input$snp_mi_d_glob=="altéré"){
        
        if("Paralysie totale d'un membre inférieur flasque" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie totale du membre inférieur droit flasque","75%"))
        }
        if("Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie complète du nerf sciatique droit (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)","60%"))
        }
        if("Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaore","Membre inférieur droit","Paralysie du nerf sciatique poplité externe droit (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)","30%"))
        }
        if("Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie du nerf sciatique poplité interne droit (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)","30%"))
        }
        if("Paralysie du nerf crural (quadriceps)" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaore","Membre inférieur droit","Paralysie du nerf crural droit (quadriceps)","40%"))
        }
        if("Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)" %in% input$snp_mi_d1){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie du nerf obturateur droit (pectiné, obturateur externe, adducteur)","15%"))
        }
        
        if("Paralysie totale d'un membre inférieur flasque" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du membre inférieur droit flasque","56%"))
        }
        if("Paralysie complète du nerf sciatique (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du nerf sciatique droit (demi-tendineux, demi membraneux, biceps fémoral, une partie du grand adducteur, auxquels se joignent les muscles innervés par le sciatique poplité externe et le sciatique poplité interne)","45%"))
        }
        if("Paralysie du nerf sciatique poplité externe (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du nerf sciatique poplité externe droit (jambier antérieur, extenseur propre du gos orteil, extenseur commun, long et court péroniers latéraux, pédieux)","23%"))
        }
        if("Paralysie du nerf sciatique poplité interne (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du nerf sciatique poplité interne droit (poplité, jumeaux, soléaire, plantaire grêle, jambier postérieur, fléchisseur commun, long fléchisseur du premier orteil, tous les muscles plantaires)","23%"))
        }
        if("Paralysie du nerf crural (quadriceps)" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du nerf crural droit (quadriceps)","30%"))
        }
        if("Paralysie du nerf obturateur (pectiné, obturateur externe, adducteur)" %in% input$snp_mi_d2){
          lesion <- rbind(lesion, c("Rachis dorsolombaire","Membre inférieur droit","Paralysie modéré (4/5) du nerf obturateur droit (pectiné, obturateur externe, adducteur)","11%"))
        }
        
      }
      
    }
     
    if(input$mainspoignets_activate!="Non"){
      
      # Evaluation des lesions bareme AT ---- Mains et poignets ####
      item1_g <- 3.5
      item2_g <- 10.5
      item3_g <- 10.5
      item4_g <- 10.5
      item5_g <- 21
      item6_g <- 7
      item7_g <- 7
      #
      item1_d <- 3.5
      item2_d <- 10.5
      item3_d <- 10.5
      item4_d <- 10.5
      item5_d <- 21
      item6_d <- 7
      item7_d <- 7
      #
      item1_g <- ifelse(input$main_fonct_g_item1=="INTERMEDIAIRE",1.5,item1_g)
      item2_g <- ifelse(input$main_fonct_g_item2=="INTERMEDIAIRE",7,item2_g)
      item3_g <- ifelse(input$main_fonct_g_item3=="INTERMEDIAIRE",7,item3_g)
      item4_g <- ifelse(input$main_fonct_g_item4=="INTERMEDIAIRE",7,item4_g)
      item5_g <- ifelse(input$main_fonct_g_item5=="INTERMEDIAIRE",14,item5_g)
      item6_g <- ifelse(input$main_fonct_g_item6=="INTERMEDIAIRE",3.5,item6_g)
      item7_g <- ifelse(input$main_fonct_g_item7=="INTERMEDIAIRE",3.5,item7_g)
      #
      item1_g <- ifelse(input$main_fonct_g_item1=="NULLE",0,item1_g)
      item2_g <- ifelse(input$main_fonct_g_item2=="NULLE",0,item2_g)
      item3_g <- ifelse(input$main_fonct_g_item3=="NULLE",0,item3_g)
      item4_g <- ifelse(input$main_fonct_g_item4=="NULLE",0,item4_g)
      item5_g <- ifelse(input$main_fonct_g_item5=="NULLE",0,item5_g)
      item6_g <- ifelse(input$main_fonct_g_item6=="NULLE",0,item6_g)
      item7_g <- ifelse(input$main_fonct_g_item7=="NULLEE",0,item7_g)
      #
      item1_d <- ifelse(input$main_fonct_d_item1=="INTERMEDIAIRE",1.5,item1_d)
      item2_d <- ifelse(input$main_fonct_d_item2=="INTERMEDIAIRE",7,item2_d)
      item3_d <- ifelse(input$main_fonct_d_item3=="INTERMEDIAIRE",7,item3_d)
      item4_d <- ifelse(input$main_fonct_d_item4=="INTERMEDIAIRE",7,item4_d)
      item5_g <- ifelse(input$main_fonct_d_item5=="INTERMEDIAIRE",14,item5_d)
      item6_d <- ifelse(input$main_fonct_d_item6=="INTERMEDIAIRE",3.5,item6_d)
      item7_d <- ifelse(input$main_fonct_d_item7=="INTERMEDIAIRE",3.5,item7_d)
      #
      item1_d <- ifelse(input$main_fonct_d_item1=="NULLE",0,item1_d)
      item2_d <- ifelse(input$main_fonct_d_item2=="NULLE",0,item2_d)
      item3_d <- ifelse(input$main_fonct_d_item3=="NULLE",0,item3_d)
      item4_d <- ifelse(input$main_fonct_d_item4=="NULLE",0,item4_d)
      item5_d <- ifelse(input$main_fonct_d_item5=="NULLE",0,item5_d)
      item6_d <- ifelse(input$main_fonct_d_item6=="NULLE",0,item6_d)
      item7_d <- ifelse(input$main_fonct_d_item7=="NULLEE",0,item7_d)
      #
      Score_D <- (item1_d+item2_d+item3_d+item4_d+item5_d+item6_d+item7_d)*0.01
      Score_G <- (item1_g+item2_g+item3_g+item4_g+item5_g+item6_g+item7_g)*0.01

      if(input$mainspoignets_activate %in% c("Oui les deux","Oui à droite")){
      if(Score_D<0.7){
        if(input$dominance!="Droitier"){
          lesion <- rbind(lesion, c("Mains et poignets","Main Droite","Atteinte fonctionnelle de la main droite chez un gaucher",paste0(floor((1-Score_D)*60),"%") ))
        }else{
          lesion <- rbind(lesion, c("Mains et poignets","Main Droite","Atteinte fonctionnelle de la main droite chez un droitier",paste0(floor((1-Score_D)*70),"%") ))
        }
      }
      }
      if(input$mainspoignets_activate %in% c("Oui les deux","Oui à gauche")){
      if(Score_G<0.7){
        if(input$dominance!="Droitier"){
          lesion <- rbind(lesion, c("Mains et poignets","Main Gauche","Atteinte fonctionnelle de la main gauche chez un gaucher",paste0(floor(Score_G*70),"%") ))
        }else{
          lesion <- rbind(lesion, c("Mains et poignets","Main Gauche","Atteinte fonctionnelle de la main gauche chez un droitier",paste0(floor(Score_G*60),"%") ))
        }
      }
        }
    }
    
    
      
    
    #
    if(nrow(lesion)>1){
      lesion <- lesion[!is.na(lesion$Module),]
    }
    output$AT_bareme <- DT::renderDataTable(lesion)
  })
}
# ="Oui" & (input$dominance %in% c("Gaucher","Ambidextre (Travailleur du bois)")) )| (input$amputation_epaule_d_1=="Oui" & (input$dominance %in% c("Droitier","Ambidextre (Travailleur du bois)")) )


# eval_AT <- function(input,output, session){

# }

