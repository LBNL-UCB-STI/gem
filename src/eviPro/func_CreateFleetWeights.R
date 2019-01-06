create_fleet_weights <- function() {
  
  # PEV Type
  pev_weights <- data.table(
    name = c('PHEV20','PHEV50','BEV100','BEV250'),
    weight = c(0.3,0.4,0.2,0.1)
  )
  
  # Preferred Location
  pref_weights <- data.table(
    name = c('PrefHome','PrefWork'),
    weight = c(0.7,0.3)
  )
  
  # Home Charging
  home_weights <- data.table(
    name = c('HomeL1','HomeL2','HomeL3'),
    weight = c(0.2,0.8,0)
  )
  
  # Work Charging
  work_weights <- data.table(
    name = c('WorkL1','WorkL2'),
    weight = c(0.1,0.9)
  )
  
  # Public Charging
  public_weights <- data.table(
    name = c('Public50kW','Public150kW','Public400kW'),
    weight = c(0.8,0.2,0)
  )
  
  return(list(pev_weights=pev_weights,pref_weights=pref_weights,home_weights=home_weights,work_weights=work_weights,public_weights=public_weights))
}
