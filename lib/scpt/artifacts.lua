new_artifact

	name = "of Galadriel"
	desc = 
		"A small crystal phial, with the light of Earendil's Star contained inside.",
		"Its light is imperishable, and near it darkness cannot endure.",
	
	
	base_object = { TV_LITE, SV_LITE_GALADRIEL }
	level = 20 rarity = 10 weight = 10 cost = 10000
	
	flags =
		
		ACTIVATE = getter.hardcore("light") HIDE_TYPE = 1 INSTA_ART = 1 LITE = 3
		LITE_SUN = 1 LUCK = 4 SEARCH = 4
	

}

