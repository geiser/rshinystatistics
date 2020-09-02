# title: "Summary of Kruskal-Wallis Test for {{ paste0(paste0('`', dvs, '`'), collapse = ',') }}"
# author: {{ author }} <{{ email }}>
# comment: This file is automatically generate by Shiny-Statistic app (https://statistic.geiser.tech/)
#          Author - Geiser C. Challco <geiser@usp.br>
#
#          Shiny-Statistic is distributed in the hope that it will be useful,
#          but WITHOUT ANY WARRANTY; without even the implied warranty of
#          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#          GNU General Public License for more details.
#
#          You should have received a copy of the GNU General Public License.
#         If not, see <https://www.gnu.org/licenses/>.

if (!'remotes' %in% rownames(installed.packages())) install.packages('remotes')
if (!"rshinystatistics" %in% rownames(installed.packages())) {
  remotes::install_github("geiser/rshinystatistics")
} else if (packageVersion("rshinystatistics") < "{{ rshinystatistics.version }}") {
  remotes::install_github("geiser/rshinystatistics")
}

wants <- c('ggplot2','ggpubr','rshinystatistics','utils')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(ggpubr)
library(ggplot2)
library(rshinystatistics)

## Initial Data

### Setting identificator, dependent and independent variables

wid <- "{{ wid }}"
between <- c({{ paste0(paste0('"', between, '"'), collapse = ',') }})
dvs <- c({{ paste0(paste0('"', dvs, '"'), collapse = ',') }})
ldvs <- as.list(dvs); names(ldvs) <- dvs
dat <- lapply(ldvs, FUN = function(dv) {
  data <- read.csv(paste0("{{ path }}/data-",dv,".csv"))
  rownames(data) <- data[["{{ wid }}"]]
  return(data)
})


## Removing outliers from the data

outliers <- {{ code.outliers }}
sdat <- remove_from_datatable(dat, outliers, wid, "var")


## Computation Kruskal-Wallis and Pairwise comparison

### Kruskal-Wallis test

kruskal <- get.kruskal.test(sdat, dvs, between, dv.var = "var")
(get.kruskal.table(kruskal))


### Pairwise comparison showing only significant differences

pwc <- get.kruskal.pwc(sdat, dvs, between, pwc.method = "{{ pwc.method }}", p.adjust.method = "{{ p.adjust.method }}", dv.var = "var")
(get.kruskal.pwc.table(pwc, only.sig = T))

## Descriptive Statistics and Plots

descriptive_statistics(sdat, dvs, between, "common", "var")


{{ kruskal.plots  }}

