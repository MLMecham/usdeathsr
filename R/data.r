#' CDC Vital Statistics Link Lookup Table
#'
#' A lookup table of download URLs for CDC vital statistics data files,
#' scraped from the CDC Vital Statistics Online page.
#'
#' @format A tibble with 219 rows and 8 columns:
#' \describe{
#'   \item{section}{CDC data section name}
#'   \item{year}{Data year}
#'   \item{user_guide_url}{URL to the user guide PDF}
#'   \item{user_guide_size}{Raw file size string of the user guide}
#'   \item{us_data_url}{URL to the U.S. data zip file}
#'   \item{us_data_size}{Raw file size string of the U.S. data file}
#'   \item{user_guide_size_mb}{User guide file size in megabytes}
#'   \item{us_data_size_mb}{U.S. data file size in megabytes}
#' }
#' @source \url{https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm}
"cdc_link_lookup"


#' Birth Cohort Layouts
#'
#' Layout data for birth cohort files.
#'
#' @name birth_cohort_layouts
#' @docType data
NULL

#' @rdname birth_cohort_layouts
"data_birth_cohort_1983"


#' Births Layouts
#'
#' Layout data for births files. These layouts define the fixed-width column
#' positions used to parse CDC natality data files.
#'
#' @note Files for 1968-1972 are distributed by CDC as Microsoft Publisher
#'   (.pub) files which require conversion via LibreOffice before reading.
#'   Conversion takes approximately 10-20 minutes per file. Pre-converted
#'   versions of these files are available as a GitHub release to avoid
#'   this conversion step. See the package README for details.
#'
#' @name births_layouts
#' @docType data
NULL

#' @rdname births_layouts
"data_births_1968"

#' @rdname births_layouts
"data_births_1969"

#' @rdname births_layouts
"data_births_1970"

#' @rdname births_layouts
"data_births_1971"

#' @rdname births_layouts
"data_births_1972"

#' @rdname births_layouts
"data_births_1973"

#' @rdname births_layouts
"data_births_1974"

#' @rdname births_layouts
"data_births_1975"

#' @rdname births_layouts
"data_births_1976"

#' @rdname births_layouts
"data_births_1977"

#' @rdname births_layouts
"data_births_1978"


#' Fetal Death Layouts
#'
#' Layout data for fetal death files.
#'
#' @name fetal_death_layouts
#' @docType data
NULL

#' @rdname fetal_death_layouts
"data_fetal_death_1982"

#' Mortality multiple cause data layouts
#'
#' Metadata tables describing the fixed-width file layout for US mortality
#' multiple cause datasets from the CDC NCHS. Each object covers one data year
#' and contains the column positions, types, and code mappings needed to parse
#' the corresponding fixed-width file.
#'
#' @format A tibble with 7 variables:
#' \describe{
#'   \item{name}{Column name}
#'   \item{start}{Start position in fixed-width file}
#'   \item{end}{End position in fixed-width file}
#'   \item{size}{Field width in characters}
#'   \item{type}{Data type: "int" or "str"}
#'   \item{description}{Human-readable field description}
#'   \item{codes}{Pipe-delimited key=label pairs for coded fields, empty string if none}
#' }
#' @source \url{https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/}
#' @name mortality_layouts
NULL

#' @rdname mortality_layouts
"data_mortality_multiple_1968"

#' @rdname mortality_layouts
#' @note This is an early year with limited coding — several fields left blank.
#' @note 1968-1978 all use the same user guide. We only make data_mortality_multiple_1969 because its a funny number.
"data_mortality_multiple_1969"

#' @rdname mortality_layouts
"data_mortality_multiple_1970"

#' @rdname mortality_layouts
"data_mortality_multiple_1971"

#' @rdname mortality_layouts
"data_mortality_multiple_1972"

#' @rdname mortality_layouts
"data_mortality_multiple_1973"

#' @rdname mortality_layouts
"data_mortality_multiple_1974"

#' @rdname mortality_layouts
"data_mortality_multiple_1975"

#' @rdname mortality_layouts
"data_mortality_multiple_1976"

#' @rdname mortality_layouts
"data_mortality_multiple_1977"

#' @rdname mortality_layouts
"data_mortality_multiple_1978"

#' @rdname mortality_layouts
"data_mortality_multiple_1979"

#' @rdname mortality_layouts
"data_mortality_multiple_1980"

#' @rdname mortality_layouts
"data_mortality_multiple_1981"

#' @rdname mortality_layouts
"data_mortality_multiple_1982"

#' @rdname mortality_layouts
"data_mortality_multiple_1983"

#' @rdname mortality_layouts
"data_mortality_multiple_1984"

#' @rdname mortality_layouts
"data_mortality_multiple_1985"

#' @rdname mortality_layouts
"data_mortality_multiple_1986"

#' @rdname mortality_layouts
"data_mortality_multiple_1987"

#' @rdname mortality_layouts
"data_mortality_multiple_2010"

#' @rdname mortality_layouts
"data_mortality_multiple_2011"

#' @rdname mortality_layouts
"data_mortality_multiple_2012"

#' @rdname mortality_layouts
"data_mortality_multiple_2013"

#' @rdname mortality_layouts
"data_mortality_multiple_2014"

#' @rdname mortality_layouts
"data_mortality_multiple_2015"

#' @rdname mortality_layouts
"data_mortality_multiple_2016"

#' @rdname mortality_layouts
#' @note Both 1989 and 2003 education revision fields are present. Bridged-race
#'   variables are the only race summary fields; \code{race_recode_40} is absent.
#' @note Occupation and industry fields are absent. \code{method_of_disposition}
#'   uses a simplified code set (Burial, Cremation, Other, Unknown only).
"data_mortality_multiple_2017"

#' @rdname mortality_layouts
#' @note Both education revision fields are present, though the 1989 field is
#'   blank in practice as all states completed the 2003 transition this year.
#' @note \code{race_recode_40} is introduced alongside the existing bridged-race
#'   variables. Occupation and industry fields are absent.
"data_mortality_multiple_2018"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2018. Five records were corrected
#'   in a March 2021 re-release, recoding erroneous terrorism firearm deaths to \code{X95}.
#' @note \code{method_of_disposition} retains the simplified code set from
#'   2017–2018; occupation and industry fields are absent.
"data_mortality_multiple_2019"

#' @rdname mortality_layouts
#' @note Occupation and industry fields are introduced for the first time, with
#'   data from 46 participating states coded to 2012 Census classifications via NIOSH.
#' @note This is the first year in which COVID-19 (\code{U07.1}) appears as a
#'   cause of death and is included as a rankable cause in the recode lists.
"data_mortality_multiple_2020"

#' @rdname mortality_layouts
#' @note The \code{record_type} field is introduced at position 19. The 1989
#'   education revision field is removed, as is the full bridged-race variable block.
#' @note \code{race_recode_40} is now the sole race summary field, using the
#'   1997 OMB standard. Occupation and industry fields are retained.
"data_mortality_multiple_2021"

#' @rdname mortality_layouts
#' @note Layout follows the 2021 structure with \code{record_type}, 2003-only
#'   education, and \code{race_recode_40} as the sole race summary field.
#' @note Occupation and industry fields are retained.
"data_mortality_multiple_2022"

#' @rdname mortality_layouts
#' @note Layout follows the 2021–2022 structure with \code{record_type}, 2003-only
#'   education, and \code{race_recode_40} as the sole race summary field.
#' @note Occupation and industry fields are retained.
"data_mortality_multiple_2024"

#' @rdname mortality_layouts
#' @note Geographic identifiers are present: positions 21–60 contain state,
#'   county, and city of both occurrence and residence, PMSA/MSA, CMSA, and
#'   place of birth. This detail was removed from public-use files from 2005
#'   onward per NCHS data access policy.
#' @note Both 1989 and 2003 education revision fields are present alongside an
#'   \code{education_reporting_flag} indicating which revision was used.
#' @note \code{record_type} is present at position 19. \code{marital_status}
#'   includes code \code{N} (not on certificate). Occupation and industry fields
#'   are absent.
"data_mortality_multiple_2003"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2003, retaining full geographic
#'   detail in positions 21–60 and dual education revision fields.
#' @note \code{record_type} is present at position 19. \code{marital_status}
#'   includes code \code{N} (not on certificate). Occupation and industry fields
#'   are absent.
"data_mortality_multiple_2004"

#' @rdname mortality_layouts
#' @note Geographic identifiers were removed from the public-use file this year
#'   per a new NCHS data access policy. Positions 1–19 and 21–60 are reserved
#'   (blank). The \code{record_type} field is absent from 2005 onward.
#' @note \code{marital_status} code \code{N} (not on certificate) is removed.
#'   Both 1989 and 2003 education revision fields are retained. Occupation and
#'   industry fields are absent.
"data_mortality_multiple_2005"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2005. Geographic identifiers remain
#'   excluded per NCHS data access policy.
#' @note Both education revision fields are present. Occupation and industry
#'   fields are absent.
"data_mortality_multiple_2006"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2005–2006. Geographic identifiers
#'   remain excluded per NCHS data access policy.
#' @note Both education revision fields are present. Occupation and industry
#'   fields are absent.
"data_mortality_multiple_2007"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2005–2007. Geographic identifiers
#'   remain excluded per NCHS data access policy.
#' @note Both education revision fields are present. Occupation and industry
#'   fields are absent.
"data_mortality_multiple_2008"

#' @rdname mortality_layouts
#' @note Layout is structurally identical to 2005–2008. Geographic identifiers
#'   remain excluded per NCHS data access policy.
#' @note Both education revision fields are present. Occupation and industry
#'   fields are absent.
"data_mortality_multiple_2009"

#' @rdname mortality_layouts
#' @note This year introduced a revised race classification schema.
#' @note Additional cause of death fields were added to the layout this year.
"data_mortality_multiple_2023"
