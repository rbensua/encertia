# ENCERTIA Platform

![ENCERTIA Logo](www/encertia_azul_con_texto_v1.png)

---

## 🔍 The Concept of Efficiency

The ENCERTIA platform performs a complete **efficiency analysis** using **Data Envelopment Analysis (DEA)**.  
Its objective is to determine whether a company operates efficiently according to a selected set of production indicators.

If a company is found to be inefficient, the tool calculates how specific indicators must be adjusted to reach efficiency.  
The analysis always compares the selected company against a **homogeneous peer group** defined by:

- company size  
- economic subsector  
- geographic location  

> **Efficiency definition**  
> A company is efficient if there is no other company that achieves greater production (output) using fewer resources (inputs) within the feasible production set.

In DEA terminology:

- **Inputs** = resources consumed by the company  
- **Outputs** = indicators describing the company’s production  

---

## 📊 Indicators Used in the Analysis

### **Inputs**

1. **Total Assets**  
   Includes all economic resources (goods and rights) used by the company.

2. **Staff Costs**  
   Includes wages, salaries, social security contributions, pensions, and other labor-related expenses.

3. **Other Costs**  
   All non-personnel business expenses.

### **Output**

1. **Operating Revenues**  
   Income generated from the company’s primary business activities.

---

## 🏛️ Organizational Structure of ENCERTIA

The application is organized into several functional sections:

---

### **1. Data Loading**

The user can choose between:

#### ✔️ Uploading a custom `.csv` file  
The file must contain the following columns:

- `DMU`  
- `Total Assets`  
- `Staff Costs`  
- `Other Costs`  
- `Operating Revenues`  
- `Nemployees`

Each row must represent one company (DMU).  
If the format is incorrect, the platform identifies and reports the exact error.

#### ✔️ Using the default dataset  
This dataset contains financial information on companies from the **Valencian Community (Spain)**, extracted from the **SABI** database.

---

### **2. Data Filtering**

The user can configure:

- Range of number of employees  
- Exclusion of specific companies from the benchmark set  
- Choice between:
  - **CRS (Constant Returns to Scale)** — more demanding, broader production possibilities  
  - **VRS (Variable Returns to Scale)**  
- Choice of slack type:
  - Arbitrary (default)
  - Max-slack  
  - Min-slack (MILP)  
  - Min-slack (MF)

Once settings are chosen, click **“Compute”**.

---

### **3. Efficiency Results**

This section presents:

- Original and target values for each indicator  
- A semicircular visual representation to show required improvements  
- A list of benchmark firms (efficient peers)  
- Sliders for each indicator allowing the user to:
  - adjust feasible improvements
  - restrict changes (setting the slider to 0 means no change allowed)

Reducing flexibility for one indicator usually requires larger adjustments in others to reach efficiency.

---

### **4. Benchmark Study**

The final output displays:

- The benchmark firms used to compute target values  
- Their indicator values  
- A bar chart showing the **relative contribution** of each benchmark firm to the target construction

---

## 📬 Contact

For more information about the ENCERTIA project, please contact:  
📧 **maria.c.bas@uv.es**

---

