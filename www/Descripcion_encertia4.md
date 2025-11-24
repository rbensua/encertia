---
output: html_document
header-includes:
  - \usepackage[none]{hyphenat}
  - \usepackage{tcolorbox}
---

---------------------------------------------------

![](www/encertia_azul_con_texto_v1.png){width="314"}

------------------------------------------------------------------------

## The concept of efficiency


The ENCERTIA platform offers the following analytical output:

**EFFICIENCY:** An efficiency analysis is conducted using the Data Envelopment Analysis technique. The objective is to ascertain whether the companies in question are operating efficiently, according to a set of production indicators selected for use in this mathematical model. In the event that the company is deemed to be inefficient, the analysis will indicate the manner in which certain indicators should be modified in order to enhance economic outcomes and thereby attain efficiency. In order to obtain the efficiency study, the analysis compares each of the participating companies with respect to a homogeneous group of companies according to size, economic sub-sector and location.


<style>
.box {
  border: 2px solid #007ACC;
  padding: 10px;
  border-radius: 8px;
  background-color: #F0F8FF;
}
</style>

<div class="box">
In this analysis, a company is deemed to be efficient if, within a specified set of production possibilities, there is no other company that, with a reduced input, can achieve a greater output.
In this model, the terms "inputs" and "outputs" are defined. The term "inputs" is defined as the resources utilized by the company in its primary operational activities. The term "outputs" refers to the indicators that quantify the company's production.
</div>
      

## Indicators Set

In this study, the following production indicators have been considered as inputs and outputs:

**Inputs** are defined as follows:

1.  **Total Assets**: encompasses all the elements (goods and rights that can be valued economically) that positively influence.

2.  **Staff costs** include wages, salaries and remuneration, as well as expenses directly determined on the basis of wages or salaries, such as social security contributions, pensions and other social benefits.

3.  **Other costs** is used to describe all business-related costs that are not attributed to personnel.

**Output** is defined as follows:

1.  **Operating revenues** refers to the revenue generated from the sale of products or services that are directly related to the primary business activity of the enterprise.


## The organizational structure of Encertia


The following section provides a description of the main parts of the Encertia application:

**Data Loading.** The user may select the data source to be employed in the analysis. Two modes of operation are available: 

- Uploading your own database via a .csv file. In this file, each column must correspond to the identification of the company, the different indicators mentioned, and the number of employees: "DMU" "Total Assets" "Staff Costs" "Other Costs" "Operating Revenues" "Nemployees"; while each row must represent a firm subject to evaluation. Should the file fail to conform to the required format, the platform is designed to precisely identify the location of the error and report it to the user. 
- Using a default data set that contains information on companies located in the Valencian Community (Spain). These data have been retrieved from the financial database SABI.

**Data Filtering.** The user may select the company on which the efficiency analysis will be conducted. In addition, the following filters and configurations are available:

-   Determining a range for the number of employees of the companies on which the efficiency analysis will be conducted.  
-   Excluding one or several companies from the set employed to perform the efficiency analysis in relation to the selected company. 
-   Choosing between two distinct conceptualisations of efficiency: one with constant returns to scale and another with variable returns to scale. In the constant returns to scale model variant, the notion of efficiency is more demanding, as it encompasses a broader set of production possibilities. A firm that is deemed efficient under the constant returns to scale specification will also be considered efficient under the variable returns to scale specification.
-   Selecting the type of slacks: Arbitrary, Max-slack, Min-slack (MILP), or Min-slack (MF). Since the occurrence of multiple optimal solutions is uncommon, option "Arbitrary" is selected bu default and hence, the second stage is omitted. 

Once all these options have been specified, the analysis is executed by pressing the ***“Compute”*** button.

**Efficiency result.** This part corresponds to the presentation of the efficiency analysis results. The output provides a comparison between the original values of the indicators for the selected firm and the target values that must be achieved for the firm to be deemed efficient. The required target values for each indicator are represented by the position of the semicircle on the graph. In addition, the top section displays both the number and the list of firms—allowing the user to include or exclude them—that serve as benchmarks for assessing the efficiency of the selected firm.

Each indicator is equipped with a slider that enables the user to regulate the magnitude of its adjustment, thereby aligning the model with the firm’s actual possibilities. In situations where modifying a particular indicator is costly, the corresponding slider can be reduced, constraining its variation to a minimum when set to 0. However, this approach typically results in more substantial adjustments to the other indicators.

**Benchmark study.** In the final section of the analysis, the benchmark firms and their corresponding indicator values are presented. These firms represent the efficient units from which the target values have been derived. The bar chart depicts the relative contribution of each benchmark firm to the construction of the target value.





--------------------------------------------------------------

<style>
.box {
  border: 2px solid #007ACC;
  padding: 10px;
  border-radius: 8px;
  background-color: #F0F8FF;
}
</style>

<div class="box">
For more information on the ENCERTIA project contact maria.c.bas@uv.es
</div>


------------------------------------------------------------------------
