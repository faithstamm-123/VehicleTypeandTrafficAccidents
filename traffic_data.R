# Assignment 10 - Shiny App
# Faith Stamm
# Vehicle Types & Fatal Traffic Accidents in 2024

# Load original vehicle data
vehicles <- read.csv("vehicle.csv")

# Keep the columns needed for the app
vehicles_clean <- vehicles[
  , c("STATENAME", "MAKENAME", "MOD_YEAR",
      "BODY_TYPNAME", "MONTHNAME")
]

# Start with Other Vehicles as the default category
# Hatchbacks, convertibles, and other unlisted types stay here.
vehicles_clean$BODY_GROUP <- "Other Vehicles"

# Sedans
sedan_types <- c(
  "4-door sedan, hardtop",
  "Sedan/Hardtop, number of doors unknown"
)

# Coupes
coupe_types <- c(
  "2-door sedan,hardtop,coupe",
  "3-door coupe"
)

# SUVs
suv_types <- c(
  "Compact Utility (Utility Vehicle Categories \"Small\" and \"Midsize\")",
  "Large utility (ANSI D16.1 Utility Vehicle Categories and \"Full Size\" and \"Large\")",
  "Utility station wagon (includes suburban limousines, Suburban, Travellall, Grand Wagoneer)",
  "Utility Vehicle, Unknown body type"
)

# Trucks (mostly pickup trucks)
pickup_types <- c(
  "Auto-based pickup (includes E1 Camino, Caballero, Ranchero, SSR, G8-ST, Subaru Brat, Rabbit Pickup)",
  "Light Pickup",
  "Medium/heavy Pickup (GVWR greater than 10,000 lbs.)",
  "Unknown (pickup style) light conventional truck type"
)

# Vans
van_types <- c(
  "Large Van-Includes van-based buses (B150-B350, Sportsman, Royal Maxiwagon, Ram, Tradesman,...)",
  "Minivan (Chrysler Town and Country, Caravan, Grand Caravan, Voyager, Voyager, Honda-Odyssey, ...)",
  "Other van type (Hi-Cube Van, Kary)",
  "Step-van or walk-in van (GVWR less than or equal to 10,000 lbs.)",
  "Unknown van type"
)

# Motorcycles
motorcycle_types <- c(
  "Moped",
  "Motor Scooter",
  "Off-road Motorcycle",
  "Other motored cycle type (mini-bikes, pocket motorcycles \"pocket bikes\")",
  "Three-wheel Motorcycle (2 Rear Wheels)",
  "Two Wheel Motorcycle (excluding motor scooters)",
  "Unenclosed Three Wheel Motorcycle / Unenclosed Autocycle (1 Rear Wheel)",
  "Unknown motored cycle type",
  "Unknown Three Wheel Motorcycle Type"
)

# Large Trucks
truck_types <- c(
  "Cab Chassis Based (includes Rescue Vehicle, Light Stake, Dump, and Tow Truck)",
  "Single-unit straight truck or Cab-Chassis (GVWR greater than 26,000 lbs.)",
  "Single-unit straight truck or Cab-Chassis (GVWR range 10,001 to 19,500 lbs.)",
  "Single-unit straight truck or Cab-Chassis (GVWR range 19,501 to 26,000 lbs.)",
  "Single-unit straight truck or Cab-Chassis (GVWR unknown)",
  "Step van (GVWR greater than 10,000 lbs.)",
  "Truck-tractor (Cab only, or with any number of trailing unit; any weight)",
  "Unknown if single unit or combination unit Heavy Truck (GVWR greater than 26,000 lbs.)",
  "Unknown medium/heavy truck type"
)

# Buses
bus_types <- c(
  "Cross Country/Intercity Bus",
  "Other Bus Type",
  "School Bus",
  "Transit Bus (City Bus)",
  "Unknown Bus Type",
  "Van-Based Bus GVWR greater than 10,000 lbs."
)

# Assign simplified vehicle body type categories

# Sedans
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% sedan_types
] <- "Sedans"

# Coupes
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% coupe_types
] <- "Coupes"

# SUVs
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% suv_types
] <- "SUVs"

# Trucks
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% pickup_types
] <- "Trucks"

# Vans
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% van_types
] <- "Vans"

# Motorcycles
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% motorcycle_types
] <- "Motorcycles"

# Large Trucks
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% truck_types
] <- "Large Trucks"

# Buses
vehicles_clean$BODY_GROUP[
  vehicles_clean$BODY_TYPNAME %in% bus_types
] <- "Buses"

# Hatchbacks and remaining types stay in Other Vehicles

# Save the cleaned data
saveRDS(vehicles_clean, "vehicles_clean.rds")


# Display the number of vehicles in each category
print(table(vehicles_clean$BODY_GROUP))
