# PLSDA Classifier

The `pg_plsda_classifier_operator` is used to create a linear classifier using a Matlab implementation.

##### Usage

Input projection|.
---|---
`y-axis`        | LFC, continuous data 
`row`           | ID, categorical, char
`column`        | Cell.line, categorical, char
`colors`        | Response, categorical, char 



##### Details

Calls PLS-DA, developed by R. de Wijn and implemented in Matlab.

Can only work with 1 data point per cell (row x col combo).

##### See Also


