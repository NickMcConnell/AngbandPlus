/**
 * \file list-materials.h
 * \brief List of object materials
 */
//@ add flags, etc?
/* Materials are defined as
 * MATERIAL(Name, Density (g/cc), Cost (only meaningful relative to others))
 * 
 * Costs are prime to avoid obvious patterns
 * ...maybe densities should be also, though this would be less obvious as weights are
 * normally not seen so precisely.
 */
MATERIAL( GENERIC, "generic", 1000, 42 )			// for objects without a specified material
MATERIAL( PLASTIC, "plastic", 1150, 101 ) 			// nylon
MATERIAL( ALUMINIUM, "aluminium", 2780, 331 )  		// 2024
MATERIAL( STEEL, "steel", 7860, 1709 ) 				// Stainless 316
MATERIAL( COPPER, "copper", 8500, 1321 )			// various brasses and bronzes are around this - much variation - pure metal is 8940
MATERIAL( LEAD, "lead", 11340, 787 )				// pure
MATERIAL( GOLD, "gold", 19320, 14779 )				// pure
MATERIAL( SILVER, "silver", 10490, 4801 )			// pure
MATERIAL( TITANIUM, "titanium", 4420, 2593 )		// Ti-6Al-4V
MATERIAL( UNOBTAINIUM, "unobtainium", 1420, 17737 )	// 101% pure
MATERIAL( LEATHER, "leather", 860, 409 )
MATERIAL( WOOD, "wood", 740, 163 ) 					// American Red Oak
MATERIAL( GLASS, "glass", 2230, 139 )				// borosilicate
MATERIAL( STONE, "stone", 2700, 223 )				// granite - flint is similar
MATERIAL( CLOTH, "cloth", 400, 149 )				// felt - much variation
MATERIAL( PAPER, "paper", 800, 73 )					// printing paper - much variation
MATERIAL( WAX, "wax", 900, 91 )						// paraffin wax
MATERIAL( ORGANIC, "organic", 950, 83 )				// pork (lol - but this is a catch all category...)
MATERIAL( MAX, NULL, 0, 0 )
