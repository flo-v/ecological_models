# ecological modelling

This repo contains ecological hunter-prey interaction models:
* **orca_sealion.R** replicates the code used in Wolf & Mangel, 2007
* sparrow_hawk_great_tit*.R files, which apply a similar modelling approach to Sparrow-Hawk-Great-Tit interactions
  * **sparrow_hawk_great_tit_attention_attack.R**:   
    Shows (similar to the orca-sealion model) that the more additional false attacks hawks fly, the more the great tits are forced to cut down on anti-predator behavior. The model splits the day into multiple time steps (eg. 12) and runs for x amount of days (eg. 180). It differs between light (hunting takes place) and night (no food but still metabolism costs) hours (night hours are half of the light hours).
  * **sparrow_hawk_great_tit_day_blocks.R**:  
    Further developed "sparrow_hawk_great_tit_attention_attack.R", which includes that false attacks are only flown in specific light hours (blocks). The results show that false attacks flown in the morning force great tits to pay least attention (anti-predator behaviour), compared to false attacks at other times.
