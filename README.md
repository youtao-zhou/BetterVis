<p align="center">
  <img src="https://raw.githubusercontent.com/youtao-zhou/BetterVis_Helper/main/Figure/Github_Page_Logo1.png" width="800" height="200"/>
</p>

![GitBook](https://img.shields.io/badge/GitBook-%23000000.svg?style=for-the-badge&logo=gitbook&logoColor=white)![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)	![RStudio](https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white)![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)![](https://komarev.com/ghpvc/?username=your-github-username&style=for-the-badge&base=1000&abbreviated=true)

### Authors: Youtao Zhou, Zikai Lin, Hengrui Liang

BetterVis is designed to create beautiful graphics with elegant and concise code. It is especially suitable for medical researchers, helping to reduce the inefficient time they spend on plot beautification and to achieve publication-ready graphics for mainstream journals in the shortest possible time. 

<table>
  <tr>
    <td align="center" width="50%">
      <img src="https://raw.githubusercontent.com/youtao-zhou/BetterVis_Helper/main/Figure/Github_Page_Logo2.png" width="100%"/>
    </td>
    <td align="center" width="50%">
      <img src="https://raw.githubusercontent.com/youtao-zhou/BetterVis_Helper/main/Figure/Github_Page_Logo3.png" width="100%"/>
    </td>
  </tr>
</table>

<br>

Advantage 🚀
---------------------------------------------
Visit our full online documentation at [BetterVis_Helper Docs](https://youtao-zhou.github.io/BetterVis_Helper/) for detailed instructions on how to:

- 🔌 Install BetterVis in R with a single command  
- 🎓 Get started quickly with concise code and easy-to-understand tutorials  
- 📊 Create a wide array of advanced plots for comprehensive data exploration  
- 📄 Generate figures suitable for most clinical research papers  
- ⚙️ Fine-tune every plot element with extensive customization options  
- 💾 Export publication-quality figures with ease


<br>

## Install of BetterVis

Before installing BetterVis, please check that your R version meets the requirements.

**We require R version 4.4 or higher and ggplot2 package version 3.5.0 or higher**



BetterVis has not been uploaded to CRAN yet. If users need to download BetterVis, they should install it via Github. Before this, installing devtools is mandatory.

```R
install.packages('devtools)
devtools::install_github("youtao-zhou/BetterVis")
```



Some required packages, including jjPlot, linkET, ComplexHeatmap, ggsankey, and vegan, may not be installed automatically and could have dependency issues. Please use the following commands to install them

```R
devtools::install_github("junjunlab/jjPlot")
devtools::install_github("Hy4m/linkET", force = TRUE)
devtools::install_github("jokergoo/ComplexHeatmap")
devtools::install_github("davidsjoberg/ggsankey")
install.packages("vegan")
```

