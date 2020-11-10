# New ABAP
## Content
 - [Select Single Row from Database](#how-to-get-started-with-clean-code)
## Select Single Row from Database 
> [This section] (#how-to-get-started-with-clean-code)

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

## Line Functions
```` ABAP
TRY.

*Checks Line Existence
    IF line_exists( lt_result[ arbgb = 'SABAPDEMOSi' ] ).
      ...
    ENDIF.

*Where itab[ ... ] is a table expression that works here like READ TABLE ... TRANSPORTING NO FIELDS.

*Returns the line Idex
    DATA(idx) = line_index( lt_result[ arbgb = 'SABAPDEMOSi' ] ).

*Where itab[ ... ] is a table expression that works here like
*READ TABLE ... TRANSPORTING NO FIELDS. If no line is found,
* the value 0 is returned. For Hash-Tables or hashed secondary keys, always the value -1 is returned.


*Returns number of lines in table
    DATA(lv_lines) = lines( lt_result ).

  CATCH cx_sy_itab_line_not_found.
ENDTRY.
````

## Field Symbols
```
*Declarations
TYPES: tt_mara TYPE STANDARD TABLE OF mara.
DATA: t_mara TYPE tt_mara.
*
FIELD-SYMBOLS: <lfs_mara> LIKE LINE OF t_mara. " <<
*
" Field symbol without type
FIELD-SYMBOLS:
  <lfs_any_tab> TYPE ANY TABLE,
  <lfs_any>     TYPE any.

* Append line
APPEND INITIAL LINE TO t_mara ASSIGNING <lfs_mara>.
<lfs_mara>-matnr = '123456'.
*
* insert table
INSERT INITIAL LINE INTO t_mara ASSIGNING <lfs_mara> INDEX 2.
<lfs_mara>-matnr = 'ABCDEF'.

* READ and MODIFY
READ TABLE t_mara ASSIGNING <lfs_mara>
 WITH KEY matnr = '123456'.
IF sy-subrc EQ 0.
  <lfs_mara>-ersda = sy-datum.
ENDIF.
*
* LOOP and MODIFY
LOOP AT t_mara ASSIGNING <lfs_mara>.
  <lfs_mara>-ersda = sy-datum + 1.
ENDLOOP.

* Check if Field-Symbol is assigned
IF <lfs_mara> IS ASSIGNED.
  WRITE: 'Assigned'.
ELSE.
  WRITE: 'Unassigned'.
ENDIF.

"remvoe the reference
UNASSIGN <lfs_mara>.

*Copy internal table using two field-symbols
DATA: lt_1 TYPE STANDARD TABLE OF t100.
DATA: lt_2 TYPE STANDARD TABLE OF t100.
FIELD-SYMBOLS: <lt_1> TYPE ANY TABLE.
FIELD-SYMBOLS: <lt_2> TYPE ANY TABLE.
*
ASSIGN lt_1 TO <lt_1>.
*


SELECT * FROM t100
 INTO TABLE <lt_1>
 UP TO 10 ROWS.
*
ASSIGN lt_2 TO <lt_2>.
<lt_2> = <lt_1>.
*
lv_lines = lines( lt_2 ).
WRITE: lv_lines.

*Assign Data using 2 field symbols
*DATA: lt_1 TYPE STANDARD TABLE OF t100.
*DATA: lt_2 TYPE STANDARD TABLE OF t100.
FIELD-SYMBOLS: <ls_1> TYPE t100.
FIELD-SYMBOLS: <ls_2> TYPE t100.
*
SELECT * FROM t100
 INTO TABLE lt_1
 UP TO 10 ROWS.
*
LOOP AT lt_1 ASSIGNING <ls_1>.
  APPEND INITIAL LINE TO lt_2 ASSIGNING <ls_2>.
  <ls_2> = <ls_1>.
ENDLOOP.
*
*DATA: lv_lines TYPE i.
lv_lines = lines( lt_2 ).
WRITE: lv_lines.

*Field Symbol to access component by Name : Commented because of conflicts in declarations in program , otherwise code is valid

*DATA: lt_1 TYPE STANDARD TABLE OF t100.
*FIELD-SYMBOLS: <ls_1> LIKE LINE OF lt_1.
*
*SELECT * FROM t100
* INTO TABLE lt_1
* UP TO 10 ROWS.
**
** dynamic access
*DATA: lv_field TYPE char30.
*FIELD-SYMBOLS: <lv_field> TYPE ANY.
*lv_field = 'TEXT'.
*LOOP AT lt_1 ASSIGNING <ls_1>.
* ASSIGN COMPONENT lv_field OF STRUCTURE <ls_1> TO <lv_field>.
* CHECK <lv_field> IS ASSIGNED.
* WRITE: / <lv_field>.
*ENDLOOP.

*Field Symbol to access component by Component Number :Commented because of conflicts in declarations in program , otherwise code is valid

*DATA: lt_1 TYPE STANDARD TABLE OF t100.
*FIELD-SYMBOLS: <ls_1> LIKE LINE OF lt_1.
**
*SELECT * FROM t100
* INTO TABLE lt_1
* UP TO 10 ROWS.
**
** dynamic access
*DATA: lv_comp_number TYPE i.
*FIELD-SYMBOLS: <lv_field> TYPE ANY.
*lv_comp_number = 4.


*LOOP AT lt_1 ASSIGNING <ls_1>.
* ASSIGN COMPONENT lv_comp_number OF STRUCTURE <ls_1> TO <lv_field>.
* CHECK <lv_field> IS ASSIGNED.
* WRITE: / <lv_field>.
*ENDLOOP.
```

## Value Operator
```
TYPES t_itab TYPE STANDARD TABLE OF i
 WITH DEFAULT KEY .

* classical
DATA itab_o TYPE t_itab.
APPEND: 100 TO itab_o,
 0 TO itab_o,
 300 TO itab_o.

* using VALUE - Variation 1
DATA(itab) = VALUE t_itab( ( 100 ) ( ) ( 300 ) ).

* using VALUE - Variation 2
DATA itab_2 TYPE t_itab.
itab_2 = VALUE #( ( 100 ) ( ) ( 300 ) ).

* Example 4
TYPES:
  BEGIN OF ty_data,
    kunnr TYPE kunnr,
    name1 TYPE name1,
    ort01 TYPE ort01,
    land1 TYPE land1,
  END OF ty_data.
TYPES: tt_data TYPE STANDARD TABLE OF ty_data
 WITH DEFAULT KEY.
* classical
DATA: itab_multi_c TYPE tt_data.
FIELD-SYMBOLS: <fs> LIKE LINE OF itab_multi_c.
APPEND INITIAL LINE TO itab_multi_c ASSIGNING <fs>.
<fs>-kunnr = '123'.
<fs>-name1 = 'ABCD'.
<fs>-ort01 = 'LV'.
<fs>-land1 = 'NV'.
APPEND INITIAL LINE TO itab_multi_c ASSIGNING <fs>.
<fs>-kunnr = '456'.
<fs>-name1 = 'XYZ'.
<fs>-ort01 = 'LA'.
<fs>-land1 = 'CA'.

* Using VALUE
DATA(itab_multi_comp) =


 VALUE tt_data( ( kunnr = '123' name1 = 'ABCD' ort01 = 'LV' land1 = 'NV' )
 ( kunnr = '456' name1 = 'XYZ' ort01 = 'LA' land1 = 'CA' )
 ).
 
```

## Create a RANGE table
```
TYPES:
  BEGIN OF ty_customer,
    customer TYPE char10,
    name     TYPE char30,
    city     TYPE char30,
    route    TYPE char10,
  END OF ty_customer.
TYPES: tt_customers TYPE SORTED TABLE OF ty_customer
 WITH UNIQUE KEY customer.

DATA(t_customres) =
 VALUE tt_customers(
 ( customer = 'C0001' name = 'Test Customer 1' city = 'NY' route = 'R0001' )
 ( customer = 'C0002' name = 'Customer 2' city = 'LA' route = 'R0003' )
 ( customer = 'C0003' name = 'Good Customer 3' city = 'DFW' route = 'R0001' )
 ( customer = 'C0004' name = 'Best Customer 4' city = 'CH' route = 'R0003' )
 ).

*Create a range table
TYPES: tt_route_range TYPE RANGE OF char10.

DATA(t_route_range) =
 VALUE tt_route_range(
 "( sign = 'I' option = 'BT' low = '001' high = '002' )
 FOR ls_cust_r IN t_customres
 LET s = 'I'
 o = 'EQ'
 IN sign = s
 option = o
 ( low = ls_cust_r-route high = ls_cust_r-name )

 ).
```
## String Templates
```
*The expression or any mathematical formula can be included within the String template.
*To distinguish an expression from regular text, you would need to wrap the expression in the curly brackets { .. } .
*The expression within the brackets would be evaluated first and result would be than converted to character string.
*To be able to understand it more, check out these code lines. It has Old Code using the helper lines and a new code using the String templates.
*Both providing the same results.


* --- OLD using Helper Variables
DATA: lv_i TYPE i.
DATA: lv_c TYPE char10.
DATA: lv_string TYPE string.
DO 5 TIMES.
  lv_i = sy-index * 10.
  lv_c = lv_i.
  CONDENSE lv_c.
  CONCATENATE lv_string 'Value' lv_c ',' INTO lv_string.
ENDDO.
WRITE: / lv_string.

* --- NEW
DATA: lv_string1 TYPE string.
DO 5 TIMES.
  lv_string1 = lv_string1 && |Value| && |{ sy-index * 10 }| && ','.
ENDDO.
WRITE: / lv_string1.

* --- Space
* To provide space while concatenating with &&, you have to use it like '&& | | &&

```

## Use of COND
```
*Just for testing
DATA(lv_time) = CONV t( sy-timlo - 12 * 3600 ).

*Transforms a time to 12 hour format using a conditional expression in an operand position.
*The type of the result is used by the operand after the first specified THEN, which makes it string.
cl_demo_output=>display(
  COND #( LET m = '120000' IN
          WHEN sy-timlo < m THEN
            |{ sy-timlo TIME = ISO } AM|
          WHEN sy-timlo > m AND sy-timlo < '240000' THEN
            |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|
          WHEN sy-timlo = m THEN
            |High Noon|
          ELSE
            |Throw Error| ) ).



*A typical all day example goes as follwos.

DATA language TYPE string.
IF sy-langu = 'D'.
  language = 'DE'.
ELSE.
  language = 'EN'.
ENDIF.
cl_abap_docu_external=>get_abap_docu_for_adt(
    EXPORTING
      language =  language
   IMPORTING
     html      =     DATA(html) ).


*OR

cl_abap_docu_external=>get_abap_docu_for_adt(
    EXPORTING
      language =  COND #( WHEN sy-langu = 'D' THEN `DE` ELSE `EN` )
   IMPORTING
     html      =     html ).
```

## GROUP BY for internal tables, replaces AT-ENDAT
```
SELECT * FROM spfli INTO TABLE @DATA(lt_spfli).

*Single Field Grouping
LOOP AT lt_spfli INTO DATA(ls_spfli) GROUP BY ls_spfli-carrid.

*Inside the loop LS_SPFLI represents a group,not a structure - A loop can be put on group which contains rows with same CARRID.
  LOOP AT GROUP ls_spfli INTO DATA(lw_spfli).
*   Here is work area of line SPFLI
*    WRITE:/ lw_spfli-carrid.

  ENDLOOP.
*  WRITE: / 'Next group'.
ENDLOOP.


*Grouping by number of fields
LOOP AT lt_spfli INTO ls_spfli GROUP BY ( key1 = ls_spfli-carrid key2 = ls_spfli-airpfrom ).

*Inside the loop LS_SPFLI represents a group,not a structure - A loop can be put on group which contains rows with same CARRID.
  LOOP AT GROUP ls_spfli INTO lw_spfli.
*   Here is work area of line SPFLI
*    WRITE:/ lw_spfli-carrid, lw_spfli-airpfrom.

  ENDLOOP.
*  WRITE: / 'Next group'.
ENDLOOP.


*Alternate way - Doesn't report ATC error for Loop inside Loop
DATA: lt_temp_table LIKE lt_spfli.
LOOP AT lt_spfli INTO ls_spfli GROUP BY ( key1 = ls_spfli-carrid key2 = ls_spfli-airpfrom )
                                         ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_group>).

  lt_temp_table = VALUE #( FOR n IN GROUP <lfs_group> ( n ) ).

  cl_demo_output=>write( lt_temp_table ).
ENDLOOP.

cl_demo_output=>display( ).
```

## Date Format Conversion
```
DATA: lv_date TYPE d VALUE '20190718'.
DATA(lv_user_format) = |{ lv_date DATE = USER }|. "RAW,ISO,USER,ENVIRONMENT
```
## Select on Internal Table, ABAP_SORTORDER_TAB
```
DATA(sentence) = 'ABAP is excellent'.
SPLIT condense( sentence ) AT space INTO TABLE DATA(words).
*out->write( | Number of Words: { lines( words ) } | ).

LOOP AT words ASSIGNING FIELD-SYMBOL(<lfs_words>).

  DATA(characters) = VALUE abap_sortorder_tab( FOR char = 0 THEN char + 1 UNTIL char = strlen( <lfs_words> ) ( name = <lfs_words>+char(1) ) ).
  SELECT DISTINCT * FROM @characters AS characters INTO TABLE @DATA(unique_characters).
*out->write( | No of unique chars in the word: { lines( unique_characters ) }| ).

ENDLOOP.
```

## REDUCE and FILTER
```
SELECT * FROM t100 INTO TABLE @DATA(lt_t100) UP TO 100 ROWS.

*Concatenate the values for one column or any other expression - Value output is Single Only
DATA(lv_string2) = REDUCE string( INIT text1 TYPE string
FOR wa IN lt_t100
NEXT text1 = text1 && ' | ' && wa-text ).
WRITE:/ lv_string2.

TYPES: BEGIN OF lty_item_prices,
         matnr TYPE matnr,
         plant TYPE werks_d,
         price TYPE i,
       END OF lty_item_prices,

       BEGIN OF lty_item_filter,
         matnr TYPE matnr,
         plant TYPE werks_d,
       END OF lty_item_filter,

       BEGIN OF lty_plant_country,
         plant TYPE werks_d,
         land1 TYPE char2,
       END OF   lty_plant_country.

TYPES: tt_item_prices   TYPE SORTED TABLE OF lty_item_prices WITH NON-UNIQUE KEY primary_key COMPONENTS plant,
       tt_plant_country TYPE TABLE OF lty_plant_country WITH DEFAULT KEY,
       tt_item_filter   TYPE TABLE OF lty_item_filter WITH DEFAULT KEY.

DATA(lt_item_prices) = VALUE tt_item_prices( ( matnr = '1010817' plant = '1000' price = 10 )
                                             ( matnr = '1010818' plant = '1000' price = 10 )
                                             ( matnr = '1010819' plant = '1000' price = 10 )
                                             ( matnr = '1010820' plant = '1000' price = 10 )
                                             ( matnr = '1010827' plant = '2000' price = 20 )
                                             ( matnr = '1010828' plant = '2000' price = 20 )
                                             ( matnr = '1010829' plant = '2000' price = 20 )
                                             ( matnr = '1010830' plant = '2000' price = 20 )
                                           ).

DATA(lt_plant_country) = VALUE tt_plant_country( ( plant = '1000' land1 = 'IN' )
                                                 ( plant = '1000' land1 = 'US' )
                                               ).

*Get Sum of prices for all the plants in India- Value output is Single
DATA(lv_sum_india) = REDUCE i( INIT sum TYPE i
                               FOR wa_plant   IN lt_plant_country WHERE ( land1 = 'IN' )
                               FOR prices  IN lt_item_prices WHERE ( plant = wa_plant-plant )
                               NEXT sum = sum + prices-price ).

WRITE:/ lv_sum_india.
```
