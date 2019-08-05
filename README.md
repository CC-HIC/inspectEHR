# inspectEHR

inspectEHR is a data wrangling, cleaning and reporting tool for CC-HIC. It is designed to run with the CC-HIC EAV table structure (which at present exists in PostgreSQL and SQLite flavours). We are about to undergo a major rewrite to a OHDSI CDM version 6, so this package will be in flux. Please see the `R` vignettes for further details on how to use the package to perform the most common data wrangling tasks.

## Data Cleaning Rules

### Episode Level
- Internal consistency check of episode
  - Each episode has a unique identifier
  - Each episode has a start date
  - Each episode has an end date defined by one of:
  	- End of episode date time
  	- Death date time
  	- Or: is flagged as an "open" episode

### Event Level 1 (Episode and Event Characterisation)
- Events occur within their episode (with a buffer of 48 hours)
- Events fall within range validation checks - see `qref`
- Events are not duplicates

### Event Level 2 (Restrictions)
- Co-occurance events:
	  - When systolic and diastolic BP are recorded together, diastolic is lower than systolic
		  
### Event Level 3 (Statistical)
- Events that are known to follow a particular distribution, conform to this distribution.
- KS testing checks for significant deviation of a distribution applied as a pairwise testing between ICUs.
