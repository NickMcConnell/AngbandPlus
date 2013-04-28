set loc en

# Window title
::msgcat::mcset $loc Equipment
::msgcat::mcset $loc Inventory
::msgcat::mcset $loc Floor
::msgcat::mcset $loc Items
if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Quiver
}

# Window
::msgcat::mcset $loc Inscription:
::msgcat::mcset $loc Displayed
::msgcat::mcset $loc Total
::msgcat::mcset $loc Threshold
::msgcat::mcset $loc Limit
::msgcat::mcset $loc "%d item"
::msgcat::mcset $loc "%d items"
::msgcat::mcset $loc weight1_lb "%s lb"
::msgcat::mcset $loc weight2_lb "%s/%s lb"
::msgcat::mcset $loc weight3_lb "%s/%s/%s lb"
::msgcat::mcset $loc weight1_kg "%s kg"
::msgcat::mcset $loc weight2_kg "%s/%s kg"
::msgcat::mcset $loc weight3_kg "%s/%s/%s kg"

# Usage
::msgcat::mcset $loc Activate
::msgcat::mcset $loc Remove
::msgcat::mcset $loc Browse
::msgcat::mcset $loc Fire
::msgcat::mcset $loc Refuel
::msgcat::mcset $loc Eat
::msgcat::mcset $loc Drink
::msgcat::mcset $loc Read
::msgcat::mcset $loc Jam
::msgcat::mcset $loc Use
::msgcat::mcset $loc Zap
::msgcat::mcset $loc Aim
::msgcat::mcset $loc Wield

# Popup
::msgcat::mcset $loc *Destroy*
::msgcat::mcset $loc Sell
::msgcat::mcset $loc Drop
::msgcat::mcset $loc "Drop One"
::msgcat::mcset $loc "Drop Some"
::msgcat::mcset $loc "Drop All"
::msgcat::mcset $loc Throw
::msgcat::mcset $loc "Select This Item"
::msgcat::mcset $loc Inspect
::msgcat::mcset $loc Inscribe
::msgcat::mcset $loc Uninscribe

# Options
::msgcat::mcset $loc "Show Flavors"
::msgcat::mcset $loc "Show Icons"
::msgcat::mcset $loc "Show Labels"
::msgcat::mcset $loc "Show Weights"
::msgcat::mcset $loc "Use Metric"
::msgcat::mcset $loc "Always On Top"
::msgcat::mcset $loc "Graphics Mode"
::msgcat::mcset $loc "List Mode"

# Statusbar
::msgcat::mcset $loc status-total-threshold-limit "Total/Threshold/Limit"
::msgcat::mcset $loc status-weight-equip "Weight of equipment items"
::msgcat::mcset $loc status-number-inven "Number of inventory items"
::msgcat::mcset $loc status-weight-inven "Weight of inventory items"
