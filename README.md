# Getting Started

## Data Links

* [Website put together by the Invisible Institute visualizing their complaint data](https://cpdp.co/).
* [Github repository of the code and data available from the Invisible Institute](https://github.com/invinst/chicago-police-data/tree/master/data/unified_data).
* [Code Ocean repository for the Ba et al. 2021 paper on diversity in policing](https://codeocean.com/capsule/8907164/tree/v1).

## Merge process - Ba et al. 2021

> "Non-standard shifts are dropped (retaining only first–third watches), as are absences, shifts with assigned special duties (e.g. protest detail, station security, training), and double- or triple-duty days in which a single officer serves multiple shifts." page 8 of Appendix Section S1.5 in Ba et al. 2021.

I did not find any evidence in their code that they did this.

> "For stops, arrests, and uses of force, we drop all events that occur outside of the reported patrol start/stop times, eliminating off-duty activity." page 8 of Appendix Section S1.5 in Ba et al. 2021.

1. Right join the outcome (i.e. stops, arrests, or uses of force) and officer assignments using officer_id and date. I.e. keep every assignment and only outcomes which occurred during the start date of the shift.
2. Keep any outcomes which occurred during the hours of the shift.
    * Round the outcome time to the lowest hour.
    * Round the shift start time to the lowest hour.
    * Round the shift end time to the highest hour.
3. Right join outcomes and assignments using officer_id and date (incremented by one day). This captures outcomes which occurred the **following day** during an overnight shift.
4. Keep any outcomes which occurred during the hours of the shift using the same logic as in step 2.
5. Merge the data sets (outcomes merged by date and outcomes merged by the next day) by row. 
    * Keep only distinct entries. Shift assignments which had no outcomes will have duplicate entries since they will appear as having had no outcomes on the focal day **and** the next day (since we also check the next day due to overnight shifts).
    * Filter out any shift assignments which matched with outcomes on one date but not the other. For example, a non-overnight shift will obviously not match with any outcomes the following day. As a result, there will be an entry for that shift assignment with no outcomes associated with it. However, there will be other entries for that shift assignment with correctly matched outcomes. As a result, the shift assignment entry in question will be erroneous.
    
### Notes

* It will occasionally be the case that an outcome will match to multiple shift assignments. How can an outcome match to more than one shift assignment?
    * The officer in question could have had overlapping shift assignments, and the outcome occurred during the intersection of their shifts.
    * One of the shifts had a missing start time or end time, and the outcome matched with that shift (in addition to matching with at least one another shift assignment).
* It will occasionally be the case that not all shift assignments are retained in the merging process despite using a right join. Why aren't all shift assignments retained? Ba et al. (2021) have it so that any shift assignment which had an outcome on the same day **but** did not occur during the shift itself is counted as missing and not retained.
* Ba et al. (2021) implicitly assume that if an outcome and a shift assignment match on the **date** but the shift assignment is missing its **start time and end time** then the outcome **should still** match that shift assignment.

## Merge process - Risi

1. Conduct a right non-equi join on stops and officer assignments using officer_id, time of the stop, the start time of the shift, and the end time of the shift. I.e. Keep every shift assignment and keep those stops which occurred exactly between the start time and end time for a particular shift.
2. **Differences from Ba et al. 2021**
    * Any shift with a missing start time and end time will not match with any stops. Ba et al. 2021 will match a shift with a missing start time and end time with any stop that occurred on the same day(s) as the shift.
    * The stop must have occurred exactly between the start time and end time of the shift. Ba et al. 2021 use rounding to create a window of time around the shift start and end times in which a stop may have occurred and would still count as happening during the shift.
    * If a shift assignment has a stop which occurs the same day, but the stop does not occur during the shift times, the shift is recorded as having no stops rather being recorded as *missing*.
    * Any stops which are matched to multiple shift assignment are dropped. **All duplicate entries** are dropped.
    