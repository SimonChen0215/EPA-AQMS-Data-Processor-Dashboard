# EPA-AQMS-Data-Processor-Dashboard

[中文](#中文) | [English](#english)

---

## English

An R Shiny dashboard for exploring and processing air quality monitoring data from EPA networks in **NSW, QLD, VIC and SA**.

### Features

- **AQMS Stations**: interactive map and station search
- **Data Pull**: fetch data from the NSW and QLD EPA APIs, and load VIC and SA data from local files
- **Results**: hourly and daily summary statistics
- **Visualizations**: time series and dust plots
- **Export**: download processed data
- **Guide**: in-app usage notes

### Project Structure

```
app.R          Entry point
global.R       Packages, helper sourcing, station cache
ui.R           User interface
server.R       Server logic
R/             Helper modules (API fetching, processing, plots, VIC/SA loaders)
cache/         Cached station and parameter lists (.rds)
www/           Custom CSS
SA-AQMS-Data/      Local SA data
Victoria-AQMS-Data/ Local VIC data (xlsx)
```

### Setup

1. Install R (4.x recommended) and RStudio (optional).
2. Install the required packages:

   ```r
   install.packages(c("shiny", "shinyjs", "shinyWidgets", "openair", "ggplot2",
                      "dplyr", "tidyr", "lubridate", "DT", "httr", "jsonlite",
                      "reshape2", "leaflet", "shinythemes"))
   ```

3. Set the VIC API key. Create a `.Renviron` file in the project folder:

   ```
   VIC_API_KEY=your_key_here
   ```

   Restart R after saving the file.

### Running the App

Open the project in RStudio and run `app.R`, or from the R console:

```r
shiny::runApp()
```

### Notes

- Station lists are cached in `cache/`. To refresh them, call `refresh_station_cache()` from the R console.
- Do not commit API keys to the repository.

---

## 中文

这是一个基于 R Shiny 的仪表盘，用于查看和处理 **新南威尔士州（NSW）、昆士兰州（QLD）、维多利亚州（VIC）和南澳州（SA）** 的环境空气质量监测数据。

### 功能

- **AQMS Stations**：交互式地图和站点搜索
- **Data Pull**：从 NSW、QLD 的 EPA API 获取数据，并从本地文件加载 VIC 和 SA 数据
- **Results**：小时和日均统计结果
- **Visualizations**：时间序列和扬尘图
- **Export**：导出处理后的数据
- **Guide**：应用内使用说明

### 项目结构

```
app.R          程序入口
global.R       加载程序包、引入辅助脚本、站点缓存
ui.R           用户界面
server.R       服务器端逻辑
R/             辅助模块（API 获取、数据处理、绘图、VIC/SA 数据加载）
cache/         缓存的站点和参数列表（.rds）
www/           自定义 CSS
SA-AQMS-Data/      本地 SA 数据
Victoria-AQMS-Data/ 本地 VIC 数据（xlsx）
```

### 安装步骤

1. 安装 R（推荐 4.x）和 RStudio（可选）。
2. 安装所需程序包：

   ```r
   install.packages(c("shiny", "shinyjs", "shinyWidgets", "openair", "ggplot2",
                      "dplyr", "tidyr", "lubridate", "DT", "httr", "jsonlite",
                      "reshape2", "leaflet", "shinythemes"))
   ```

3. 设置 VIC API Key。在项目文件夹中创建 `.Renviron` 文件：

   ```
   VIC_API_KEY=你的key
   ```

   保存后重启 R。

### 运行应用

在 RStudio 中打开项目并运行 `app.R`，或在 R 控制台中运行：

```r
shiny::runApp()
```

### 注意事项

- 站点列表缓存在 `cache/` 中。如需刷新，可在 R 控制台调用 `refresh_station_cache()`。
- 不要将 API Key 提交到代码仓库。
