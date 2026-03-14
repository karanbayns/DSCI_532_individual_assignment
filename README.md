# Academic Performance Dashboard (Shiny for R)

## Live App
**Deployed Application:** [https://karanbayns-dsci-532-individual-assignment.share.connect.posit.cloud]

## Overview
This project is an interactive dashboard built using **Shiny for R**. It is a re-implementation of a group project dashboard originally developed in Shiny for Python, created for the DSCI 532 Individual Assignment. 

The dashboard explores factors associated with student academic performance to support data-driven educational decision-making for school administrators and families.

## App Features 
This application fulfills the individual assignment requirements by including the following features ported from the original project:

* **Input Components:** UI filters for **School Type** and **Parental Education Level**.
* **Reactive Calculation:** A reactive dataframe block that dynamically filters the underlying student dataset whenever the user changes the input parameters.
* **Output Components:** * **Value Boxes:** Real-time KPI summaries (Average Exam Score, Average Hours Studied, Average Attendance).
    * **Plots:** Visualizations exploring relationships (e.g., Study habits vs. exam performance, Family income and score distribution).

## Installation and Local Usage

To run this Shiny for R application locally on your machine, follow these steps:

### 1. Clone the repository
Open your terminal and clone this repository:
```bash
git clone https://github.com/karanbayns/DSCI_532_individual_assignment.git
cd DSCI_532_individual_assignment
