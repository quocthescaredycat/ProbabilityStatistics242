# ProbabilityStatistics Assignment HK242
## 🧐 Overview
This repository contains the code and report for **Project 1: Ad Click Prediction**, a course assignment in Probability & Statistics at Ho Chi Minh City University of Technology. We explore a synthetic Kaggle dataset of user‑ad interactions to:
- Understand demographic and behavioral drivers of ad clicks  
- Perform exploratory data analysis (EDA)  
- Build and compare predictive models (Logistic Regression vs. Random Forest)  
- Evaluate model performance and interpret key factors
## 📂 Dataset

- **Source**: [Marius Ciobanu – Ad Click Prediction Dataset on Kaggle](https://www.kaggle.com/datasets/marius2303/ad-click-prediction-dataset)  
- **Sample size**: 10 000 records  
- **Columns**:  
  - `id` – unique user record  
  - `full_name` – (anonymized) user name  
  - `age` – integer (18–64)  
  - `gender` – Male / Female / Non‑Binary / Unknown  
  - `device_type` – Desktop / Mobile / Tablet / Unknown  
  - `ad_position` – Top / Side / Bottom / Unknown  
  - `browsing_history` – Shopping / Education / Entertainment / Social Media / Unknown  
  - `time_of_day` – Morning / Afternoon / Evening / Night / Unknown  
  - `click` – target (0 = no, 1 = yes)
## ✨ Features

- **Exploratory Data Analysis**: histograms, boxplots, correlation matrix  
- **Data Preprocessing**: per‑user imputation, handling of missing values  
- **Predictive Modeling**:  
  - Logistic Regression  
  - Random Forest  
- **Hypothesis Testing**: gender, device type, and time‑of‑day effects  
- **Visualizations**: ggplot2 (R) and corrplot

---

## 🚀 Setup & Installation

1. **Clone** this repository
```bash
git clone https://github.com/quocthescaredycat/ProbabilityStatistics242.git
cd ProbabilityStatistics242
```
2. Download the dataset and place it at Desktop/ad_click_dataset.csv
3. Run on Rstudio

## 📄 License
This project is licensed under the MIT License.

## 📬 Contact
Author: Nguyen Hoang Quoc
Email: quoc.nguyenhoang2305@hcmut.edu.vn
