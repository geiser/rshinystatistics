# title: "Scheirer-Ray-Hare for {{ paste0('`',dv,'` ~ ',paste0(paste0('`',between,'`'), collapse='*')) }}"
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

wants <- c('ggplot2','ggpubr','rshinystatistics','car','stats','rstatix','utils','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(rshinystatistics)

## Initial Data

dat <- read.csv("{{ path }}/data.csv")
rownames(dat) <- dat[["{{ wid }}"]]

## Removing outliers from the data

outliers <- c({{ paste0(paste0('"',outlier.ids,'"'),collapse = ',') }})
sdat <- dat[!dat[["{{ wid }}"]] %in% outliers,]

## Computation Scheirer Ray Hare and Pairwise comparison

### Scheirer Ray Hare test

srh <- get.scheirer.test(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}))
(get.scheirer.table(srh))

### Pairwise comparison showing all comparisons

pwc <- get.scheirer.pwc(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}), p.adjust.method="{{ p.adjust.method }}")
(get.scheirer.pwc.table(pwc))


## Descriptive Statistics and Plots

descriptive_statistics(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}), "common")


{{ srh.plots  }}

