# tbcleanr 0.1.3.9001
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