


# Mixed

  - optimisation over an extended period of time
  - market is defined by: {position, price}
  - production is defined by: {position, capacity_volume, current_level, production_rate, contract_price}
  - vessel is defined by: {position, volume, speed, boil_off_rate, shipping_cost}
  - at any date, each vessel can be in one of possible states: {idle, loading, shipping}
  - each decision is defined by: {choose a vessel, choose a production site, choose a market,
  move the vessel from its current position to the chosen production site, load a certain amount of gas at contract price,
  move the vessel to the chosen market, unload a certain amount of gas at the market price}
  - given a time-frame, list all feasible decisions ? The list of feasible decisions will change over time and
   it will depend on decisions made on previous dates
  - allow for re-routing of vessels once they are shipping to a given market ?
  - during a decision, as a first approximation all vessels are busy; it could be that some terminals can accommodate multiple vessels, in which case another decision can start before the remaining vessels of the current decision have terminated their trips.
  - make prices a time series
  - input: price simulations for all production sites and markets. They are a list of 2 elements: the valuation date and a `TimeSeries` representing the forward curve.

  - Generate a forward curve for each date of the optimization period<br/>
    On each date, identify the best possible action<br/>
    Update the log according to the best action for the day<br/>
    Update cashflows relative to that day. The concept of rolling intrinsic is very different as cashflows are not immediate; they will be negative at all dates except for the unloading dates.<br/>
    Implement a flag (for vessel/production/market) to be used for _locking_ a plan until its completion.  

    At each valuation date, the system `$production` and `$market` are updated with the corresponding forward curves, which will be used to calculate the cashflows.
  - Current strategy: execute the best plan at each date when there are vessels available.

  - What to do with terminal capacity for markets ? Zero it after vessel unloading ?
  - Boil-off of empty vessel ?
  - Add `Missing[]` when making list of possible plans ?
  - Need to update the status of each component of a trip/plan as soon as its role has completed.
  - Need to integrate possible plans and $states.
  - Order of possible trips is also important.
  - Criteria to define a trip acceptable:

      - vessel available;
      - production available by the time vessel reaches it;
      - market available by the time vessel reaches it.

## States

  - `$states` is an `Association` index by dates.

# Pseudo code

```mathematica
val[pStart_, pEnd_, granularity_] = Module[{timeSteps},
    timeSteps = DateRange[DatePlus[pStart, granularity], pEnd, granularity];
    vDate = pStart;
    updateProductionInventory;
    While[vDate < pEnd,
      makeDecision;
      updateLogPlanAndCashflows; ⟵
      updateProductionInventory;
      vDate = earliestDateIdleVessel;
    ]
  ]
```

# Parameters

|name|value|
|----|------|
|daily production|3588|
|limit load|110000|
|cargo size|145000|
|loading rate|8000|
|boil-off rate|0.12%|


# Junk
