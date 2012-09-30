%%% Artifact activation functions %%%

define act_narthanc()
{
	if (info("fire bolt (9d8) every 8+d8 turns")) return 0;
	message("Your dagger is covered in fire...");
	bolt(GF_FIRE, dice(9,8));
	return dice(1, 8) + 8;
}

define act_nimthanc()
{
	if (info("frost bolt (6d8) every 7+d7 turns")) return 0;
	message("Your dagger is covered in frost...");
	bolt(GF_COLD, dice(6,8));
	return dice(1, 7) + 7;
}

define act_dethanc()
{
	if (info("lightning bolt(4d8) every 6+d6 turns")) return 0;
	message("Your dagger is covered in sparks...");
	bolt(GF_ELEC, dice(4,8));
	return dice(1, 6) + 6;
}

define act_rilia()
{
	if (info("stinking cloud (12) every 4+d4 turns")) return 0;
	message("Your dagger throbs deep green...");
	ball(3, GF_POIS, 12);
	return dice(1, 4) + 4;
}

define act_belangil()
{
	if (info("frost ball (48) every 5+d5 turns")) return 0;
	message("Your dagger is covered in frost...");
	ball(2, GF_COLD, 48);
	return dice(1, 5) + 5;
}

define act_dal()
{
	if (info("remove fear and cure poison every 5 turns")) return 0;
	message("You feel energy flow through your feet...");
	set_field(FL_FEAR, 0);
	set_field(FL_POIS, 0);
	return 5;
}

define act_ringil()
{
	if (info("frost ball (100) every 300 turns")) return 0;
	message("Your sword glows an intense blue...");
	ball(2, GF_COLD, 100);
	return 300;
}

define act_anduril()
{
	if (info("fire ball (72) every 400 turns")) return 0;
	message("Your sword glows an intense red...");
	ball(2, GF_FIRE, 72);
	return 400;
}

define act_firestar()
{
	if (info("large fire ball (72) every 100 turns")) return 0;
	message("Your morningstar rages in fire...");
	ball(3, GF_FIRE, 72);
	return 100;
}

define act_feanor()
{
	if (info("haste self (20+d20 turns) every 200 turns")) return 0;
	if (get_field(FL_FAST) == 0)
	{
		set_field(FL_FAST, dice(1, 20) + 20);
	}
	else
	{
		add_field(FL_FAST, 5);
	}
	return 200;
}

define act_theoden()
{
	if (info("drain life (120) every 400 turns")) return 0;
	message("The blade of your axe glows black...");
	bolt(GF_OLD_DRAIN, 120);
	return 400;
}

define act_turmil()
{
	if (info("drain life (90) every 70 turns")) return 0;
	message("The head of your hammer glows white...");
	bolt(GF_OLD_DRAIN, 90);
	return 70;
}

define act_caspanion()
{
	if (info("door and trap destruction every 10 turns")) return 0;
	message("Your armor glows bright red...");
	burst(1, GF_KILL_DOOR, 0, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE);
	return 10;
}

define act_avavir()
{
	if (info("word of recall every 200 turns")) return 0;
	recall(dice(1, 20) + 15);
	return 200;
}

define act_taratol()
{
	if (info("haste self (20+d20 turns) every 100+d100 turns")) return 0;
	if (get_field(FL_FAST) == 0)
	{
		set_field(FL_FAST, dice(1, 20) + 20);
	}
	else
	{
		add_field(FL_FAST, 5);
	}
	return dice(1, 100) + 100;
}

define act_eriril()
{
	if (info("identify every 10 turns")) return 0;
	misc(MI_ID);
	return 10;
}

define act_olorin()
{
	if (info("probing every 20 turns")) return 0;
	misc(MI_PROBING);
	return 20;
}

define act_eonwe()
{
	if (info("mass genocide every 1000 turns")) return 0;
	message("Your axe lets out a long, shrill note...");
	misc(MI_MASS_GENO);
	return 1000;
}

define act_lotharang()
{
	if (info("cure wounds (4d8) every 3+d3 turns")) return 0;
	message("Your battle axe radiates deep purple...");
	heal(dice(4,8));
	set_field(FL_CUT, (get_field(FL_CUT) / 2) - 50);
	return dice(1, 3) + 3;
}

define act_cubragol()
{
	if (info("fire branding of bolts every 999 turns")) return 0;
	misc(MI_BRAND_BOLT);
	return 999;
}

define act_arunruth()
{
	if (info("frost bolt (12d8) every 500 turns")) return 0;
	message("Your sword glows a pale blue...");
	bolt(GF_COLD, dice(12,8));
	return 500;
}

define act_aeglos()
{
	if (info("frost ball (100) every 500 turns")) return 0;
	message("Your spear glows a bright white...");
	ball(2, GF_COLD, 100);
	return 500;
}

define act_orome()
{
	if (info("stone to mud every 5 turns")) return 0;
	message("Your spear pulsates...");
	beam(GF_KILL_WALL, 20 + dice(1, 30),
	     PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, 100);
	return 5;
}

define act_soulkeeper()
{
	if (info("heal (1000) every 888 turns")) return 0;
	message("Your armor glows a bright white...");
	message("You feel much better...");
	heal(1000);
	set_field(FL_CUT, 0);
	return 888;
}

define act_belegennon()
{
	if (info("phase door every 2 turns")) return 0;
	teleport_self(10);
	return 2;
}

define act_celeborn()
{
	if (info("genocide every 500 turns")) return 0;
	misc(MI_GENOCIDE);
	return 500;
}

define act_luthien()
{
	if (info("restore life levels every 450 turns")) return 0;
	misc(MI_RES_LEV);
	return 450;
}

define act_ulmo()
{
	if (info("teleport away every 150 turns")) return 0;
	message("Your trident glows deep red...");
	beam(GF_AWAY_ALL, MAX_SIGHT * 5, PROJECT_KILL, 100);
	return 150;
}

define act_colluin()
{
	if (info("resistance (20+d20 turns) every 111 turns")) return 0;
	message("Your cloak glows many colours...");
	add_field(FL_OPP_ACID, dice(1, 20) + 20);
	add_field(FL_OPP_ELEC, dice(1, 20) + 20);
	add_field(FL_OPP_FIRE, dice(1, 20) + 20);
	add_field(FL_OPP_COLD, dice(1, 20) + 20);
	add_field(FL_OPP_POIS, dice(1, 20) + 20);
	return 111;
}

define act_holcolleth()
{
	if (info("Sleep II every 55 turns")) return 0;
	message("Your cloak glows deep blue...");
	burst(1, GF_OLD_SLEEP, plev(), PROJECT_KILL | PROJECT_HIDE);
	return 55;
}

define act_thingol()
{
	if (info("recharge item I every 70 turns")) return 0;
	message("You hear a low humming noise...");
	recharge(60);
	return 70;
}

define act_colannon()
{
	if (info("teleport every 45 turns")) return 0;
	teleport_self(100);
	return 45;
}

define act_totila()
{
	if (info("confuse monster every 15 turns")) return 0;
	message("Your flail glows in scintillating colours...");
	bolt(GF_OLD_CONF, 20);
	return 15;
}

define act_cammithrim()
{
	if (info("magic missile (2d6) every 2 turns")) return 0;
	message("Your gloves glow extremely brightly...");
	bolt(GF_MISSILE, dice(2,6));
	return 2;
}

define act_paurhach()
{
	if (info("fire bolt (9d8) every 8+d8 turns")) return 0;
	message("Your gauntlets are covered in fire...");
	bolt(GF_FIRE, dice(9,8));
	return dice(1, 8) + 8;
}

define act_paurnimmen()
{
	if (info("frost bolt (6d8) every 7+d7 turns")) return 0;
	message("Your gauntlets are covered in frost...");
	bolt(GF_COLD, dice(6,8));
	return dice(1, 7) + 7;
}

define act_pauraegen()
{
	if (info("lightning bolt (4d8) every 6+d6 turns")) return 0;
	message("Your gauntlets are covered in sparks...");
	bolt(GF_ELEC, dice(4,8));
	return dice(1, 6) + 6;
}

define act_paurnen()
{
	if (info("acid bolt (5d8) every 5+d5 turns")) return 0;
	message("Your gauntlets look very acidic...");
	bolt(GF_ACID, dice(5,8));
	return dice(1, 5) + 5;
}

define act_fingolfin()
{
	if (info("a magical arrow (150) ever 90+d90 turns")) return 0;
	message("Magical spikes appear on your cesti...");
	bolt(GF_ARROW, 150);
	return dice(1, 90) + 90;
}

define act_holhenneth()
{
	if (info("detection every 55+d55 turns")) return 0;
	message("An image forms in your mind...");
	detect(DET_ALL);
	return dice(1, 55) + 55;
}

define act_gondor()
{
	if (info("heal(500) every 500 turns")) return 0;
	message("You feel a warm tingling inside...");
	heal(500);
	set_field(FL_CUT, 0);
	return 500;
}

define act_razorback()
{
	if (info("star ball (150) every 1000 turns")) return 0;
	message("You are surrounded by lightning!");
	misc(MI_RAZORBACK);
	return 1000;
}

define act_bladeturner()
{
	if (info("berserk rage, bless, and resistance every 400 turns"))
		return 0;
	message("Your armor glows many colours...");
	heal(30);
	set_field(FL_FEAR, 0);
	add_field(FL_SHERO, dice(1, 50) + 50);
	add_field(FL_BLESS, dice(1, 50) + 50);
	add_field(FL_OPP_ACID, dice(1, 50) + 50);
	add_field(FL_OPP_ELEC, dice(1, 50) + 50);
	add_field(FL_OPP_FIRE, dice(1, 50) + 50);
	add_field(FL_OPP_COLD, dice(1, 50) + 50);
	add_field(FL_OPP_POIS, dice(1, 50) + 50);
	return 400;
}


define act_galadriel()
{
	if (info("illumination every 10+d10 turns")) return 0;
	message("The phial wells with clear light...");
	burst(3, GF_LITE_WEAK, dice(2, 15), PROJECT_GRID | PROJECT_KILL);
	misc(MI_LITE_ROOM);
	return dice(1, 10) + 10;
}

define act_elendil()
{
	if (info("magic mapping every 50+d50 turns")) return 0;
	message("The star shines brightly...");
	misc(MI_MAP_AREA);
	return dice(1, 50) + 50;
}

define act_thrain()
{
	if (info("clairvoyance every 100+d100 turns")) return 0;
	message("The stone glows a deep green...");
	misc(MI_WIZ_LITE);
	detect(DET_DOORS);
	detect(DET_TRAPS);
	return dice(1, 100) + 100;
}


define act_ingwe()
{
	if (info("dispel evil (x5) every 300+d300 turns")) return 0;
	message("An aura of good floods the area...");
	los(GF_DISP_EVIL, plev() * 5);
	return dice(1, 300) + 300;
}

define act_carlammas()
{
	if (info("protection from evil every 225+d225 turns")) return 0;
	message("The amulet lets out a shrill wail...");
	add_field(FL_PROTEVIL, dice(1, 25) + 3 * plev());
	return dice(1, 225) + 225;
}


define act_tulkas()
{
	if (info("haste self (75+d75 turns) every 150+d150 turns")) return 0;
	message("The ring glows brightly...");
	if (get_field(FL_FAST) == 0)
	{
		set_field(FL_FAST, dice(1, 75) + 75);
	}
	else
	{
		add_field(FL_FAST, 5);
	}
	return dice(1, 150) + 150;
}

define act_narya()
{
	if (info("large fire ball (120) every 225+d225 turns")) return 0;
	message("The ring glows deep red...");
	ball(3, GF_FIRE, 120);
	return dice(1, 225) + 225;
}

define act_nenya()
{
	if (info("large frost ball (200) every 325+d325 turns")) return 0;
	message("The ring glows bright white...");
	ball(3, GF_COLD, 200);
	return dice(1, 325) + 325;
}

define act_vilya()
{
	if (info("large lightning ball (250) every 425+d425 turns")) return 0;
	message("The ring glows deep blue...");
	ball(3, GF_ELEC, 250);
	return dice(1, 425) + 425;
}

define act_power()
{
	if (info("bizarre things every 450+d450 turns")) return 0;
	message("The ring glows intensely black...");
	misc(MI_RING_POWER);
	return dice(1, 450) + 450;
}

define act_dsm_blue()
{
	if (info("breathe lightning (100) every 450+d450 turns")) return 0;
	message_delayed("You breathe lightning.");
	ball(2, GF_ELEC, 100);
	return dice(1, 450) + 450;
}

define act_dsm_white()
{
	if (info("breathe frost (110) every 450+d450 turns")) return 0;
	message_delayed("You breathe frost.");
	ball(2, GF_COLD, 110);
	return dice(1, 450) + 450;
}

define act_dsm_black()
{
	if (info("breathe acid (130) every 450+d450 turns")) return 0;
	message_delayed("You breathe acid.");
	ball(2, GF_ACID, 130);
	return dice(1, 450) + 450;
}

define act_dsm_green()
{
	if (info("breathe poison gas (150) every 450+d450 turns")) return 0;
	message_delayed("You breathe poison gas.");
	ball(2, GF_POIS, 150);
	return dice(1, 450) + 450;
}

define act_dsm_red()
{
	if (info("breathe fire (200) every 450+d450 turns")) return 0;
	message_delayed("You breathe fire.");
	ball(2, GF_FIRE, 200);
	return dice(1, 450) + 450;
}

define act_dsm_multihued()
{
	variable typ, descr;

	if (info("breathe multihued (250) every 225+d225 turns")) return 0;
	switch(dice(1, 5))
		{ case 1: typ = GF_ELEC; descr = "lightning";}
		{ case 2: typ = GF_COLD; descr = "frost";}
		{ case 3: typ = GF_ACID; descr = "acid";}
		{ case 4: typ = GF_POIS; descr = "poison gas";}
		{ case 5: typ = GF_FIRE; descr = "fire";}
	message_delayed(Sprintf("You breathe %s.", descr, 1));
	ball(2, typ, 250);
	return dice(1, 225) + 225;
}

define act_dsm_bronze()
{
	if (info("breathe confusion (120) every 450+d450 turns")) return 0;
	message_delayed("You breathe confusion.");
	ball(2, GF_CONFUSION, 120);
	return dice(1, 450) + 450;
}

define act_dsm_gold()
{
	if (info("breathe sound (130) every 450+d450 turns")) return 0;
	message_delayed("You breathe sound.");
	ball(2, GF_SOUND, 130);
	return dice(1, 450) + 450;
}

define act_dsm_chaos()
{
	variable typ, descr;

	if (info("breathe chaos/disenchantment (220) every 300+d300 turns"))
		return 0;
	switch(dice(1, 2))
		{ case 1: typ = GF_CHAOS; descr = "chaos";}
		{ case 2: typ = GF_DISENCHANT; descr = "disenchantment";}
	message_delayed(Sprintf("You breathe %s.", descr, 1));
	ball(2, typ, 220);
	return dice(1, 300) + 300;
}

define act_dsm_law()
{
	variable typ, descr;

	if (info("breathe sound/shards (230) every 300+d300 turns")) return 0;
	switch(dice(1, 2))
		{ case 1: typ = GF_SOUND; descr = "sound";}
		{ case 2: typ = GF_SHARDS; descr = "shards";}
	message_delayed(Sprintf("You breathe %s.", descr, 1));
	ball(2, typ, 230);
	return dice(1, 300) + 300;
}

define act_dsm_balance()
{
	variable typ, descr;

	if (info("breathe balance (250) every 300+d300 turns")) return 0;
	switch(dice(1, 4))
		{ case 1: typ = GF_CHAOS; descr = "chaos";}
		{ case 2: typ = GF_DISENCHANT; descr = "disenchantment";}
		{ case 3: typ = GF_SOUND; descr = "sound";}
		{ case 4: typ = GF_SHARDS; descr = "shards";}
	message_delayed(Sprintf("You breathe %s.", descr, 1));
	ball(2, typ, 250);
	return dice(1, 300) + 300;
}

define act_dsm_shining()
{
	variable typ, descr;

	if (info("breathe light/darkness (200) every 300+d300 turns"))
		return 0;
	switch(dice(1, 2))
		{ case 1: typ = GF_LITE; descr = "light";}
		{ case 2: typ = GF_DARK; descr = "darkness";}
	message_delayed(Sprintf("You breathe %s.", descr, 1));
	ball(2, typ, 250);
	return dice(1, 300) + 300;
}

define act_dsm_power()
{
	if (info("breathe the elements (300) every 300+d300 turns")) return 0;
	message_delayed("You breathe the elements.");
	ball(2, GF_MISSILE, 300);
	return dice(1, 300) + 300;
}



%%% Spell effect functions %%%

% A helper function to allow us to inline some of the priest healing spells
define hp_stun_cut(hp, stun, cut)
{
	heal(hp); set_field(FL_STUN, stun); set_field(FL_CUT, cut);
}

% A helper function to allow us to inline some of the priest dispelling spells
define priest_dispel(typ, dam)
{
	!if (info(Sprintf(" dam d%d", dam, 1)))
		los(typ, dam);
}

% A helper function to allow us to inline some of the mage/priest teleport
% spells
define mage_priest_teleport(range)
{
	!if (info(Sprintf(" range %d", range, 1)))
		teleport_self(range);
}

% A helper function to allow us to inline some of the mage bolt/beam spells
define mage_bolt(n, s, typ, db)
{
	variable beamprob = plev();

	if (pclass() != CLASS_MAGE) beamprob = beamprob / 2;
	!if (info(Sprintf(" dam %dd%d", n, s, 2)))
		beam(typ, dice(n, s), PROJECT_KILL, beamprob + db);
}

% A helper function to allow us to inline some of the mage ball spells
define mage_ball(dam, typ, rad)
{
	!if (info(Sprintf(" dam %d", dam, 1)))
		ball(rad, typ, dam);
}

% A helper function to allow us to inline the two mage "haste self" spells
define mage_haste(base, boost, nudge)
{
	!if (info(Sprintf(" dur %d+d%d", base, boost, 2)))
	{
		if (get_field(FL_FAST) == 0)
		{
			set_field(FL_FAST, base + dice(1, boost));
		}
		else
		{
			add_field(FL_FAST, dice(1, nudge));
		}
	}
}

define spell_lite_area()
{
	if (info("")) return 0;
	burst(plev() / 10 + 1, GF_LITE_WEAK, dice(2, plev() / 2),
	      PROJECT_KILL | PROJECT_GRID);
	misc(MI_LITE_ROOM);
	return 0;
}

define spell_spear_of_light()
{
	if (info(" dam 6d8")) return 0;
	beam(GF_LITE_WEAK, dice(6, 8), PROJECT_GRID | PROJECT_KILL, 100);
	return 0;
}

define spell_turn_stone_to_mud()
{
	if (info("")) return 0;
	beam(GF_KILL_WALL, 20 + dice(1, 30),
	     PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, 100);
	return 0;
}

define spell_resistance()
{
	if (info(" dur 20+d20")) return 0;
	add_field(FL_OPP_ACID, dice(1, 20) + 20);
	add_field(FL_OPP_ELEC, dice(1, 20) + 20);
	add_field(FL_OPP_FIRE, dice(1, 20) + 20);
	add_field(FL_OPP_COLD, dice(1, 20) + 20);
	add_field(FL_OPP_POIS, dice(1, 20) + 20);
	return 0;
}

define spell_heroism()
{
	if (info(" dur 25+d25")) return 0;
	heal(10);
	add_field(FL_HERO, dice(1, 25) + 25);
	set_field(FL_FEAR, 0);
	return 0;
}

define spell_berserker()
{
	if (info(" dur 25+d25")) return 0;
	heal(30);
	add_field(FL_SHERO, dice(1, 25) + 25);
	set_field(FL_FEAR, 0);
	return 0;
	return 0;
}

define spell_cure_serious_wounds()
{
	if (info(" heal 4d10")) return 0;
	heal(dice(4, 10));
	set_field(FL_CUT, get_field(FL_CUT) / 2 - 20);
	return 0;
}

define spell_resist_heat_and_cold()
{
	if (info(" dur 10+d10")) return 0;
	add_field(FL_OPP_FIRE, dice(1, 10) + 10);
	add_field(FL_OPP_COLD, dice(1, 10) + 10);
	return 0;
}

define spell_orb_of_draining()
{
	variable base = plev() / 2, rad = 2;

	if (plev() >= 30) rad = 3;
	if (pclass() != CLASS_PRIEST) base = base / 2;
	base = base + plev();
	if (info(Sprintf(" dam %d+3d6", base, 1))) return 0;
	ball(rad, GF_HOLY_FIRE, base + dice(3, 6));
	return 0;
}

define spell_protection_from_evil()
{
	variable base = 3 * plev();

	if (info(Sprintf(" dur %d+d25", base, 1))) return 0;
	add_field(FL_PROTEVIL, base + dice(1, 25));
	return 0;
}

define spell_holy_word()
{
	if (info(" heal 1000")) return 0;
	priest_dispel(GF_DISP_EVIL, plev() * 4);
	heal(1000);
	set_field(FL_FEAR, 0);
	set_field(FL_POIS, 0);
	set_field(FL_STUN, 0);
	set_field(FL_CUT, 0);
	return 0;
}

define spell_restoration()
{
	if (info("")) return 0;
	restore_stat(A_STR);
	restore_stat(A_INT);
	restore_stat(A_WIS);
	restore_stat(A_DEX);
	restore_stat(A_CON);
	restore_stat(A_CHR);
	return 0;
}
