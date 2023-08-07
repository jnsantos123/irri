---
editor_options: 
  markdown: 
    wrap: 72
---

# irri

Merge Function

[Description] Merges two files (metadata and plot level data) into a
single file by some predetermined variables. This function requires 2
inputs (a directory where the files are stored and another where the
merged files will be stored). The files representing an experiment must
be consecutive (metadata must be after the plot file of the same
experiment) when ordered alphabetically. This function also removes all
special characters and converts all variable names to uppercase. Merged
data are written on the specified directory following the naming
convention: "Occurrence_name\_\_Occurrence_code"

Traits_List Function

[Description] Lists all traits that appeared, at least once, on a group
of experiments. This function requires 2 inputs (a directory where the
merged data are stored and another where the list of traits will be
stored).

Traits_Summary Function

[Description] Creates a summary (count data) for each trait for all
experiment data. This function requires 3 inputs (a directory where the
merged data are stored, another for where the summary will be stored,
and lastly where the list of traits present in the experiments are
stored).
