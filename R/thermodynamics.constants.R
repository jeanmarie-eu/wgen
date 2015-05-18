thermodynamics.constants<-list(K=273.14999999999998,       #Kelvin
                            R=8.3144621,                #J/K/mol       #Gas constant
							g=9.8076,                   #m/s^2         #Earth's gravitational acceleration
                            H_v=2501000,                #J/kg          #Heat of vaporization of water
                            p0=1000,                    #1013.25       #Reference pressure
							R_sd=287.058,               #J/kg/K        #Specific gas constant of dry air
							R_sw=461.5,                 #J/kg/K        #Specific gas constant of water vapour
							C_pd=1004.6,                #J/kg/K        #The specific heat of dry air at constant pressure
							eps=287.058/461.5)    #The dimensionless ratio of the specific gas constant of dry air to the specific gas constant for water vapor
                            #eps=thermodynamics.R_sd/thermodynamics.R_sw
										