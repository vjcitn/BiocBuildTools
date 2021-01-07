> library(HCAExplorer)
> library(HCAMatrixBrowser)

# Show HCAExplorer as tibble
> hca <- HCAExplorer()
> hca
class: ProjectBrowser
Using azul backend at:
  https://service.explore.data.humancellatlas.org/repository/projects

Showing projects with 15 results per page
# A tibble: 15 x 7
   projects.projec… samples.sampleE… samples.organ protocols.libra…
   <fct>            <fct>            <fct>         <fct>
 1 1.3 Million Bra… specimens        brain         10X v2 sequenci…
 2 A Single-Cell T… specimens        pancreas      inDrop
 3 A single-cell m… specimens        embryo        10X 3' v1 seque…
 4 A single-cell r… specimens        lung, medias… 10X v2 sequenci…
 5 A single-cell t… specimens        eye           10X v2 sequenci…
 6 Assessing the r… organoids        NA            10X v2 sequenci…
 7 Bone marrow pla… specimens        hematopoieti… MARS-seq
 8 Cell hashing wi… specimens        blood         CITE-seq
 9 Census of Immun… specimens        immune syste… 10X v2 sequenci…
10 Comparison, cal… cellLines        NA            Drop-Seq, DroNc…
11 Dissecting the … specimens        liver         10X v2 sequenci…
12 HDCA project: s… specimens        cerebellum, … 10x 3' v3 seque…
13 Ischaemic sensi… specimens        esophagus, s… 10X v2 sequenci…
14 Melanoma infilt… specimens        tumor, lymph… Smart-seq2
15 Precursors of h… specimens        blood         Smart-seq2
# … with 3 more variables: protocols.pairedEnd <fct>,
#   donorOrganisms.genusSpecies <fct>, samples.disease <fct>

# Grab Manifest Information
> manifest <- getManifest(project = "Census of Immune Cells")

#Create LoomExperiments from downloaded Expression Matrices
> lex <- HCAMatrixBrowser::loadHCAMatrix(manifest$bundle_fqids)
