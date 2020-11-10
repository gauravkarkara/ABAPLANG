# ABAPLANG
## Select Single Row from Database

```ABAP
DATA: lt_result TYPE STANDARD TABLE OF t100,
      ls_result TYPE t100.

*To check existence of the row in DB, Following syntax can be used: Using abap_true wont even transfer a single line from DB. So, its much better
*than using 'Select Single ' OR 'UP TO 1 rows' for checking existence of entry in DB

SELECT SINGLE @abap_true
 FROM t100 INTO @ls_result
 WHERE sprsl = @sy-langu AND
 arbgb = 'SABAPDEMOSi'.
IF sy-subrc = 0.
* Sy-subrc check will ensure entry existence.
* You can also check if ls_result = abap_true instead of Sy-subrc

*Fight Between 'UP TO 1 rows & Select single
* To Check existence of row in DB, Either of both can be used. If we are not going to use the result, then you can either of both
* If we are going to use the result, then 'Up to 1 rows' is preferred over 'Select single'. Not because of performance reasons (Performance is same for both, use same native syntax),
* but because you get defined entry using 'Up to 1 Rows' because you can use 'ORDER BY PRIMARY KEY' but with 'Select single', you get any random row. So, row is not defined. So, Avoid
* 'Select Single'.

* SCN Link : http://scn.sap.com/community/abap/blog/2015/03/11/selecting-one-line-from-an-database-table
ENDIF. 
```

