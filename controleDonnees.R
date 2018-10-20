#Chargement des packages necessaires
library(sqldf)
library(stringr)
library(lubridate)
library(readr)




# Helper_functions -------------------------------------------------------------------------------------------------------

#function to convert date column
convert_to_Date <- function(ddate) {
  #try dd/mm/yyyy
  pattern = "^[0-3]?[0-9]/[0-1]?[0-9]/([0-9])+$"
  if(grepl(pattern, ddate)) {
    return(as.Date(ddate, "%d/%m/%Y"))
  }
  
  #try dd-oct.-1976
  pattern = "^[0-3]?[0-9]-[a-z]+.-([0-9])+$"
  if(grepl(pattern, ddate)) {
    return(as.Date(ddate, "%d-%b-%Y"))
  }
  
  #try dd-octobre.-1976
  pattern = "^[0-3]?[0-9]-[a-z]+-([0-9])+$"
  if(grepl(pattern, ddate)) {
    return(as.Date(ddate, "%d-%B-%Y"))
  }
  
  return(as.Date(ddate))
}


# fonction qui convertit une regle en sa négation sous forme
# d'une instruction SQL
rule_to_sql <- function(rule, dataframe) {
  # rule est supposé être au format : colonne operateur valeur
  # colonne est une colonne valide de dataframe
  #operateur est soit =, != > , < , >=, <= 
  # valeur est une valeur numerique
  #split the rule into 3 strings
  x = strsplit(rule, " ")
  colonne = x[[1]][1]
  operateur = x[[1]][2]
  valeur = x[[1]][3]  #if operateur = in valeur is val1,val2,...
  
  #check if dateframe is valide
  if(!(dataframe %in% c("R01", "R02", "R03", "R04"))) {
     warning(paste('dataframe', dataframe,'does not exist !'))
    return("")
  }
  
  #check if colonne is valide column in dataframe
  
  if(!(colonne %in% colnames(eval(parse(text = dataframe))))) {
     warning(paste('colonne', colonne,'does not exist in  ', dataframe, '!'))
    return("")
  }
  
  # oposite of valid operators
  operateurs <- c('!=', '=','<=', '>=', '<', '>', ' not in ', ' in ')
  
  #valid operators
  names(operateurs) <- c('=', '!=', '>' , '<' , '>=', '<=', 'in', 'not in')
  
  # if invalid operator return empty string with warning
  if(!(operateur %in% names(operateurs))) {
     warning(paste('operateur', operateur,'invalide !'))
    return("")
  }
  
  #get the opposite of operator
  operateur = operateurs[operateur]
  
  #si l'operateur est in ou not in alors il faut créer une liste à partir de valeur
  if(operateur %in% c(' in ', ' not in ')) {
    #valeur = paste(valeur,collapse = ',')
    valeur = paste('(',valeur,')')
  }
  
  #return the sql string
  return( paste("select * from ", dataframe, " where ", colonne, operateur, valeur,sep = "" ))
  
}


# Fin helper functions --------------------------------------------------------------------------------------------



#Chemin des données
path = "D:/Shared/a.teffal/Application_Simulation_FS/Application_R/"


# colonnes_valables -----------------------------------------------------------------------------------------------


#Nom des colonnes des fichiers
true_cols_R01 <-
  c(
    "Identifiant",
    "Strate",
    "Sexe",
    "SituationFamiliale",
    "DateNaissance",
    "DateEngagement",
    "SalaireDeBase",
    "IndemniteDeResidence",
    "PrimeAnciennete",
    "SoldeIndividuelRetraite",
    "ComplementSalarial"
  )

true_cols_R02 <-
  c(
    "Identifiant",
    "Type",
    "DateNaissance",
    "Sexe",
    "SituationFamiliale",
    "PensionAnnuelle",
    "ChargeFamille",
    "MajorationFamiliale",
    "NombreEnfants",
    "PensionTheorique"
  )

true_cols_R03 <-
  c(
    "Identifiant",
    "Catégorie",
    "Rang",
    "DateNaissance",
    "EnfantInvalide",
    "DateDebutRelation",
    "DateFinRelation",
    "MotifFinRelation"
  )

true_cols_R04 <-
  c(
    "Identifiant",
    "Type",
    "Rang",
    "Sexe",
    "DateNaissance",
    "DateDebutRelation",
    "DateFinRelation",
    "MotifFinRelation"
  )


# Importation_données ---------------------------------------------------------------------------------------------



#Importation des données à contrôler
R01 <-
  read.csv(
    paste(path, "R01_2017.csv", sep = ''),
    sep = ';',
    colClasses = c("character", "numeric", "character", "character", "character", "character", rep("numeric",5)),
    dec = ',',
    stringsAsFactors = FALSE
  )

R01_N_1 <-
  read.csv(
    paste(path, "R01_2016.csv", sep = ''),
    sep = ';',
    colClasses = c("character", "numeric", "character", "character", "character", "character", rep("numeric",5)),
    dec = ',',
    stringsAsFactors = FALSE
  )


R02 <-
  read.csv(
    paste(path, "R02_2017.csv", sep = ''),
    sep = ';',
    colClasses = c(rep("character",5), rep("numeric",5) ),
    dec = ',',
    stringsAsFactors = FALSE
  )

R03 <-
  read.csv(
    paste(path, "R03_2017.csv", sep = ''),
    sep = ';',
    colClasses = c(rep("character",2), "numeric",rep("character",5) ),
    dec = ',',
    stringsAsFactors = FALSE
  )

R04 <-
  read.csv(
    paste(path, "R04_2017.csv", sep = ''),
    sep = ';',
    colClasses = c(rep("character",2), "numeric",rep("character",5) ),
    dec = ',',
    stringsAsFactors = FALSE
  )




# Contrôles -------------------------------------------------------------------------------------------------------

#Verifie si les colonnes requises par les contrôles existent
cols_R01 <- colnames(R01)
cols_R02 <- colnames(R02)
cols_R03 <- colnames(R03)
cols_R04 <- colnames(R04)

# controle R01
for (c in true_cols_R01) {
  if (!(c %in% cols_R01)) {
    print(paste("R01 ---- ", c,  "n'existe pas dans R01 ! "))
  }
}

# controle R02
for (c in true_cols_R02) {
  if (!(c %in% cols_R02)) {
    print(paste("R02 ---- ", c,  "n'existe pas dans R02 ! "))
  }
}


# controle R03
for (c in true_cols_R03) {
  if (!(c %in% cols_R03)) {
    print(paste("R03 ---- ", c,  "n'existe pas dans R03 ! "))
  }
}

# controle R04
for (c in true_cols_R04) {
  if (!(c %in% cols_R04)) {
    print(paste("R04 ---- ", c,  "n'existe pas dans R04 ! "))
  }
}

#transformation des dates
R01$DateNaissance = as.Date(R01$DateNaissance, "%d/%m/%Y")
R01$DateEngagement = as.Date(R01$DateEngagement, "%d/%m/%Y")
R02$DateNaissance = as.Date(R02$DateNaissance, "%d/%m/%Y")
R03$DateNaissance = as.Date(R03$DateNaissance, "%d/%m/%Y")
R03$DateDebutRelation = as.Date(R03$DateDebutRelation, "%d/%m/%Y")
R03$DateFinRelation = as.Date(R03$DateFinRelation, "%d/%m/%Y")
R04$DateNaissance = as.Date(R04$DateNaissance, "%d/%m/%Y")
R04$DateDebutRelation = as.Date(R04$DateDebutRelation, "%d/%m/%Y")
R04$DateFinRelation = as.Date(R04$DateFinRelation, "%d/%m/%Y")

############################### controle R01 intra ###############################
# Calcul du salaire total
R01$SalaireTotal = R01$SalaireDeBase + R01$IndemniteDeResidence + R01$PrimeAnciennete

#Structure des fichiers
str(R01)
str(R02)
str(R03)
str(R04)

#Première et dernières lignes de R01
head(R01)
tail(R01)

#Doublons sur identifiant
doublons_ids = sqldf(
  'select * from R01 where Identifiant in (select Identifiant from R01 group by Identifiant having count(*) > 1) '
)
print('R01 - Doublons sur Identifiant : ')
print(doublons_ids)

#Strate non valide
strate_nv = sqldf('select * from R01 where Strate < 1 or Strate > 12 ')
print('R01 - Strate non valide : ')
print(strate_nv)



#Sexe non valide
sexe_nv = sqldf("select * from R01 where Sexe not in ('Féminin', 'Masculin') ")
print('R01 - Sexe non valide : ')
print(sexe_nv)

#Situation de famille non valide
sitfam_nv = sqldf(
  "select * from R01 where SituationFamiliale not in ('Marié(e)', 'Célibataire', 'Divorcé(e)', 'Veuf(ve)') "
)
print('R01 - Situation familiale non valide : ')
print(sitfam_nv)


#Lecture du fichier des règles
rules2 <- as.data.frame(read_excel("D:/Shared/a.teffal/Application_Simulation_FS/Application_R/rules2.xlsx"))

str(rules2)

for(i in 1:nrow(rules2)) {
  regle = rules2[i, "Regle"]
  etat = rules2[i, "Etat"]
  
  sql_regle = rule_to_sql(regle, etat)
  
  testregle = sqldf(sql_regle)
  
  if (nrow(testregle) > 0) {
    print(paste('---------------', rules2[i, "NomRegle"], ' : ', rules2[i, "Regle"],'--> ',
      nrow(testregle),' anomalie(s)' ))
    print(testregle)
  } else {
    
    print(paste('--', rules2[i, "NomRegle"],' : ',rules2[i, "Regle"],'--- OK ---'))
  }
}




















































