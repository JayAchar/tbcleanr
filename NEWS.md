# tbcleanr 0.1.3.4
#### 18 Feb 2019
## Bug fix

* `adm_subset.default()` failed to pass add arg to `adm_subset()` when applying class object. 
* prevent duplication of admission object class addition in `adm_classr()`.

---
# tbcleanr 0.1.3.3
#### 13 Feb 2019
## Major changes

* New function added: `response_weight_cleanr()` cleans weight monitoring data.
* `adm_classr()` removed from NAMESPACE and incorporated into default methods.
* Updates to package tests.
* New vignetted added: `Workflow - admission file` is designed to give instructions on how to use the package to process admission data sets. 

---

# tbcleanr 0.1.3.2
#### 05 Feb 2019
## Minor changes

* New function added: `adherence_classr()` is designed to detect and allocate 
object class to facilitate analysis by data entry software.

---

# tbcleanr 0.1.3.1
#### 04 Feb 2019
## Minor changes

* All output variables converted to lower case in `change_cleanr.koch6()`

---

# tbcleanr 0.1.3.0
#### 03 Feb 2019
## Major changes

* Added `change_cleanr()` to clean change data sets from EpiInfo and Koch 6

---

# tbcleanr 0.1.3.9000
#### 27 Jan 2019
## Major changes

* Added `change_classr()` to assign object classes to change data imports

---

# tbcleanr 0.1.2.0
#### 21 Jan 2019
## Major changes

* Added `txhistory()` to clean variables for previous treatment history
* Added `recorded_dst()` to clean programme recorded baseline DST
* 'cdhistory', 'cdstrainprofil' and 'cdstrainconf' variables included in`adm_subset.koch6()`
* 'DIPRO' variable included in `adm_subset.epiinfo()`

---

# tbcleanr 0.1.1.8
#### 17 Jan 2019
## Minor changes

* Bug fix in `yn_binary_fixer()` - add message and error when 
logical input encountered

---

# tbcleanr 0.1.1.7
#### 9 Jan 2019
## Minor changes

* Added a `NEWS.md` file to track changes to the package.
* Internal package improvements - tests for yn_binary_fixer()