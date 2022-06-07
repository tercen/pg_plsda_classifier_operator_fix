# PLSDA Classifier

The `pg_plsda_classifier_operator` is used to create a linear classifier using a Matlab implementation.

##### Usage

Input projection|.|.
---|---|---
`y-axis`        | Values, numerical | values of the predictor variables 
`row`           | ID, categorical, char| factor(s) that identifies the variables or features
`column`        | Cell.line, categorical, char| factor(s) that identifies the observations or samples 
`colors`        | Response, categorical, char | response variable



##### Details

Calls PLS-DA, developed by R. de Wijn and implemented in Matlab.

Can only work with 1 data point per cell (row x col combo).

##### See Also
https://github.com/rdewijn/pg_plsda_classifier_operator/blob/4d93ac13f3b8f382b01f0616c18de27a76ac0ad2/mgPlsda.md


