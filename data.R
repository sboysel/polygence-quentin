library(tidyverse)

## country codes
cc_url <- "https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv"
cc <-  readr::read_csv(cc_url)
# print(cc)
# print(names(cc))

cc <- cc %>%
    dplyr::select(con = Continent, cou = `official_name_en`,
                  iso = `ISO3166-1-Alpha-3`) %>%
    dplyr::filter(!is.na(con), !is.na(cou)) %>%
    dplyr::distinct()

## crosswalk: mig(i) <- cc(mig_con):(cc_con) <- cc(con) 
crosswalk <- data.frame(
    cc_con = c("AF", "AS", "EU", "NA", "OC", "SA"),
    mig_con = c("AFRI", "ASIA", "EURO", "NOAM", "OCEA", "SCAC")
)

## crosswalk (sectors)
crosswalk_sector_mig <- data.frame(
    s_ind = c(8, 7, 7, 10, 3, 1, 3, 2, 11, 1, 9, 5, 10, 10, 1, 1, 5, 5, 11,
          3, 3, 3, 8, 7, 9, 6, 1, 12, 7, 2, 1),
    s_mig = c(
    "Electricity, gas and water supply",
    "Wholesale and retail trade; repair of motor vehicles, motorcycles and personal and household goods",
    "Hotels and restaurants",
    "Public administration and defence; compulsory social security",
    "Construction",
    "Financial intermediation",
    "Mining and quarrying",
    "Education",
    "Manufacturing",
    "Other community, social and personal service activities",
    "Transport, storage and communications",
    "Agriculture and fishing",
    "Real estate, renting and business activities",
    "Health and social work",
    "Private households with employed persons",
    "Extra-territorial organizations and bodies",
    "Agriculture, fishing, forestry",
    "Agricultural and food industries",
    "Apparel and leather ; Publishing, printing and reproduction; Pharmaceuticals and perfumes ; Household equipment industry",
    "Car industry",
    "Construction of boats, planes and trains ; Mechanical equipment industry ; Electric and electronic equipment industry",
    "Mineral products industry ; Textile industry ; Wood and paper industry ; Chemical, rubber and plastic products ; Metallurgy and metal transformation ;",
    "Fuel production ; Water, gas, electricity",
    "Sale and repair of motor vehicles ; Wholesale trade ; Retail trade and repair",
    "Transports",
    "Financial activities",
    "Real estate activities",
    "Post and telecommunications ; Operational services ; Research and development",
    "Hotels and restaurants ; Leisure, cultural and sport activities ; Personal and domestic services",
    "Education ; Health, social work",
    "Administration ; Activities of membership  organisations and extra-territorial organisations"
    )
)

crosswalk_sector_faid <- data.frame(
    s_ind = c(1, 10, 2, 11, 3, 12, 4, 5, 1, 1, 6, 1, 1, 7, 1, 8, 9),
    s_faid = c("Bilateral ODA Commitments by Purpose (CRS)",
               "SOCIAL INFRASTRUCTURE & SERVICES",
               "Education",
               "PRODUCTION SECTORS",
               "Industry, mining and construction",         
               "MULTISECTOR",
               "Water supply and sanitation",               
               "Agriculture, forestry and fishing",
               "HUMANITARIAN AID",
               "ACTION RELATING TO DEBT",
               "ECONOMIC INFRASTRUCTURE AND SERVICES",
               "PROGRAMME ASSISTANCE",
               "Food Aid",
               "Trade and tourism",
               "UNALLOCATED/UNSPECIFIED",
               "Energy",
               "Transport and Communications")
)


## migration data
mig <- readr::read_csv("data/DIOC_SECTOR_05112022184158400.csv")

# print(mig)

# print("=> origin country:")
# print(unique(mig$COUB))
# print("=> destination country:")
# print(unique(mig$COU))
# print("=> Education:")
# print(unique(mig$`Education level`))
# print("=> Sector:")
# print(unique(mig$Sector))

# TODO:
# [ ] i is at continent level
mig <- mig %>%
    dplyr::filter(
        SEX %in% c("1", "2"), # 1 = male, 2 = female
        !Sector %in% c("All sectors", "All sectors - France", "Unknown"),
        !`Education level` %in% c("All levels of education", "Unknown education"),
        COUB %in% crosswalk$mig_con
    ) %>% 
    dplyr::mutate(
        SEX = dplyr::recode(SEX, `1` = "male", `2` = "female"),
        EDU = dplyr::recode( # char_vec, a = "Apple", b = "Banana")
            # Reference: https://en.wikipedia.org/wiki/International_Standard_Classification_of_Education
            EDU, `1` = "low",`2` = "mid", `3` = "high"
        )
    ) %>%
    dplyr::select(i = COUB, j = COU, s = Sector, e = EDU,
                  g = SEX, mig = Value)

## foreign aid
faid <- readr::read_csv("data/DACSECTOR_05112022181713400.csv")

# print(faid)

# print("=> origin country:")
# print(unique(faid$Donor))
# print("=> destination country:")
# print(unique(faid$Recipient))
# print("=> Sector:")
# print(unique(faid$Sector))

faid <- faid %>%
    dplyr::filter(
        !grepl("total", Donor, ignore.case = TRUE),
        !grepl("total", Recipient, ignore.case = TRUE)
    ) %>%
    dplyr::select(i = Donor, j = Recipient, s = Sector, t = Year, faid = Value)


## merging

crosswalk <- dplyr::left_join(cc, crosswalk, by = c(`con` = "cc_con")) %>%
    dplyr::select(con = mig_con, cou, iso) %>%
    dplyr::filter(cou %in% unique(c(faid$i, faid$j)) | iso %in% unique(c(mig$j)))
# print(crosswalk)

# print(crosswalk %>% dplyr::distinct(con) %>% dplyr::select(`i_con` = "con"))

# print(mig)

mig <- mig %>%
    dplyr::mutate(i_con = i) %>%
    dplyr::left_join(
        crosswalk %>%
            dplyr::distinct(con),
        by = c(`i_con` = "con")
    ) %>%
    dplyr::left_join(
        crosswalk %>%
            dplyr::distinct(con, cou, iso),
        by = c(`j` = "iso")
    ) %>%
    dplyr::rename(`j_con` = "con", `j_cou` = "cou") %>%
    dplyr::left_join(crosswalk_sector_mig, by = c(`s` = "s_mig")) %>%
    dplyr::select(i_con_mig = i_con,
                  j_con_mig = j_con,
                  j_cou_mig = j_cou,
                  s = s_ind, e, g, mig)

# print(unique(mig$s))
# print(mig)

faid <- faid %>%
    dplyr::left_join(
        crosswalk,
        by = c(`i` = "cou")
    ) %>%
    dplyr::rename(`i_con` = "con", `i_iso` = "iso") %>%
    dplyr::left_join(
        crosswalk,
        by = c(`j` = "cou")
    ) %>%
    dplyr::rename(`j_con` = "con", `j_iso` = "iso") %>%
    dplyr::left_join(crosswalk_sector_faid, by = c(`s` = "s_faid")) %>%
    dplyr::select(i_con_faid = i_con, i_cou_faid = i,
                  j_con_faid = j_con,
                  j_cou_faid = j,
                  s = s_ind, t, faid)

# print(unique(faid$s))
# print(faid)

## final merge
# faid <- mig
final <- dplyr::left_join(
    faid %>%
        dplyr::select(i_con_faid, i_cou_faid, j = j_con_faid, s, t, faid) %>%
        dplyr::group_by(i_con_faid, i_cou_faid, j, s) %>%
        dplyr::summarise(faid = mean(faid)) %>%
        dplyr::mutate(t = 2011),
    mig %>%
        dplyr::select(i = i_con_mig, j_con_mig, j_cou_mig, s, e, g, mig) %>%
        dplyr::mutate(t = 2011),
    by = c(`j` = "i", "s", "t"))

final <- final %>%
    dplyr::filter(!is.na(mig) & !is.na(faid))

final %>%
    readr::write_csv("data/analysis.csv")

