if {[Global config,prefix] eq "dg32"} {

Menu "Bridge"
Plugin BridgePencil,1 bridge.tcl "Stone" mode=icon,select=no
Plugin BridgePencil,2 "" "Wood" mode=icon,select=no
Plugin BridgePencil,3 "" "Wood 2" mode=icon,select=no
Stop

Menu "Building"
Plugin BldgRect,Red building.tcl "Red" mode=icon,select=no
Plugin BldgRect,Thatch "" "Thatch" mode=icon,select=no
Stop

Menu "Edge Pencil"
Plugin EdgePencil,Grass "edge.tcl" "Grass" mode=icon,select=no
Plugin EdgePencil,WaterGrass "" "Water Grass" mode=icon,select=no
Plugin EdgePencil,WaterSandGrass "" "Water Sand Grass" mode=icon,select=no
Plugin EdgePencil,Sand "" "Sand" mode=icon,select=no
Plugin EdgePencil,WaterSand "" "Water Sand" mode=icon,select=no
Plugin EdgePencil,SandGrass "" "Sand Grass" mode=icon,select=no
Stop

Menu "Edge Rect"
Plugin EdgeRect,Grass "edge.tcl" "Grass" mode=icon,select=no
Plugin EdgeRect,WaterGrass "" "Water Grass" mode=icon,select=no
Plugin EdgeRect,WaterSandGrass "" "Water Sand Grass" mode=icon,select=no
Plugin EdgeRect,Sand "" "Sand" mode=icon,select=no
Plugin EdgeRect,WaterSand "" "Water Sand" mode=icon,select=no
Plugin EdgeRect,SandGrass "" "Sand Grass" mode=icon,select=no
Stop

Menu "Edge Rect Fill"
Plugin EdgeRect,Grass,1 "edge.tcl" "Grass" mode=icon,select=no
Plugin EdgeRect,WaterGrass,1 "" "Water Grass" mode=icon,select=no
Plugin EdgeRect,WaterSandGrass,1 "" "Water Sand Grass" mode=icon,select=no
Plugin EdgeRect,Sand,1 "" "Sand" mode=icon,select=no
Plugin EdgeRect,WaterSand,1 "" "Water Sand" mode=icon,select=no
Plugin EdgeRect,SandGrass,1 "" "Sand Grass" mode=icon,select=no
Stop

Menu "Path Pencil"
Plugin PathPencil,Sand "" "Sand" mode=icon,select=no
Plugin PathPencil,Stone "" "Stone" mode=icon,select=no
Plugin PathPencil,Water "path.tcl" "Water" mode=icon,select=no
Stop

Menu "Shadow"
Plugin ShadowAdd shadow.tcl "Add" mode=icon,select=yes
Plugin ShadowRemove "" "Remove" mode=icon,select=yes
Stop

# dg32
}
