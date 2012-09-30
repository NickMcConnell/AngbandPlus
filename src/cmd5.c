/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define tval2realm(A) ((A) - TV_LIFE_BOOK + 1)

static const char *spell_tips[MAX_REALM][32] =
{
#ifdef JP
	{
		"近くの邪悪なモンスターを感知する。",
		"怪我と体力を少し回復させる。",
		"一定時間、命中率とACにボーナスを得る。",
		"恐怖を取り除く。",
		"光源が照らしている範囲か部屋全体を永久に明るくする。",
		"近くの全ての罠と扉と階段を感知する。",
		"怪我と体力を中程度回復させる。",
		"満腹にする。",
		"光線を放つ。光を嫌うモンスターに効果がある。",
		"アイテムにかかった弱い呪いを解除する。",

		"体力を大幅に回復させ、負傷と朦朧状態も全快する。",
		"聖なる力をもつ宝珠を放つ。邪悪なモンスターに対して大きなダメージを与えるが、善良なモンスターには効果がない。",
		"周辺の地形を感知し、近くの罠、扉、階段、全ての見えるモンスターを感知する。",
		"一定時間、炎、冷気に対する耐性を得る。装備による耐性に累積する。",
		"モンスター1体を魅了する。抵抗されると無効。",
		"邪悪なモンスターの攻撃を防ぐバリアを張る。",
		"極めて強力な回復呪文で、負傷と朦朧状態も全快する。",
		"視界内の全ての邪悪なモンスターをテレポートさせる。抵抗されると無効。",
		"閃光の球を放つ。",
		"自分のいる床の上に、モンスターが通り抜けたり召喚されたりすることができなくなるルーンを描く。",

		"一定時間、ヒーロー気分になる。",
		"アイテムにかかった強力な呪いを解除する。",
		"一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。",
		"視界内の全ての邪悪なモンスターにダメージを与える。",
		"視界内の全てのモンスターを魅了する。抵抗されると無効。",
		"視界内の邪悪な存在に大きなダメージを与え、体力を回復し、毒、恐怖、朦朧状態、負傷から全快する。",
		"武器を祝福する。強力な呪いのかかった武器には抵抗される。アーティファクトを祝福しようとして失敗すると劣化する。",
		"すべてのステータスと経験値を回復する。",
		"自分を中心とした光の球を発生させる。さらに、その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。",
		"アイテムの持つ能力を完全に知る。",
		"最強の治癒の魔法で、負傷と朦朧状態も全快する。",
		"隣接するモンスターに聖なるダメージを与え、視界内のモンスターにダメージ、減速、朦朧、混乱、恐怖、眠りを与える。さらに体力を回復し、恐怖を取り除き、一定時間狂戦士化し、加速する。",
	},
	{
		"弱い魔法の矢を放つ。",
		"近くの全ての見えるモンスターを感知する。",
		"近距離のテレポートをする。",
		"近くの全ての扉と罠を感知する。",
		"光源が照らしている範囲か部屋全体を永久に明るくする。",
		"一直線上の全ての罠と扉を破壊する。",
		"遠距離のテレポートをする。",
		"満腹になる。",
		"魔法の球を放つ。",
		"地上にいるときはダンジョンの最深階へ、ダンジョンにいるときは地上へと移動する。",

		"壁を溶かして床にする。",
		"スタッフ/ワンドの充填回数を増やすか、充填中のロッドの充填時間を減らす。",
		"周辺の地形を感知する。",
		"アイテムを1つ識別する。レベルが高いとアイテムの能力を完全に知ることができる。",
		"一定時間、テレパシー能力を得る。",
		"純粋な魔力のビームを放つ。",
		"一定時間、加速する。",
		"モンスター1体をテレポートさせる。抵抗されると無効。",
		"ロケットを発射する。",
		"指定した文字のモンスターを現在の階から消し去る。抵抗されると無効。",

		"モンスターの残り体力、最大体力、スピードを知る。",
		"瞬時に上か下の階にテレポートする。",
		"離れた位置のアイテムを移動させる。",
		"近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。",
		"周辺のアイテム、モンスター、地形を破壊する。",
		"その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。さらに、一定時間テレパシー能力を得る。",
		"視界内のモンスターをテレポートさせる。",
		"短距離内の指定した場所にテレポートする。",
		"アイテム1つをお金に変える。",
		"自分の周囲にいるモンスターを現在の階から消し去る。抵抗されると無効。",
		"非常に強力で巨大な純粋な魔力の球を放つ。",
		"一定時間、壁を通り抜けることができ受けるダメージが軽減される幽体の状態に変身する。"
	},
	{
		"一定時間、回復力が増強される。",
		"3方向に対して攻撃する。",
		"一定時間、ヒーロー気分になる。",
		"指定した場所に向かって高速移動を行う。",
		"一定時間、電撃、炎、冷気に対する耐性を得る。装備による耐性に累積する。",
		"一定時間、視界内の全てのモンスターを視認できる。",
		"攻撃した後、反対側に抜ける。",
		"フォースのボルトを放つ。",
		"一定時間、ACを上昇させる。",
		"アイテムにかかった弱い呪いを解除する。",

		"狂戦士化し、恐怖を除去する。",
		"周囲のダンジョンを揺らし、壁と床をランダムに入れ変える。",
		"素早く相手に近寄り、攻撃を繰り出す。",
		"一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。",
		"弾/矢/ボルトを強い威力で発射する。",
		"切れ味のある攻撃を繰り出す。",
		"一定時間、加速する。",
		"全方向に対して攻撃する。",
		"遅鈍のビームを放つ。",
		"一定時間、魔法防御力とACが上がり、混乱と盲目の耐性、反射能力、麻痺知らず、浮遊を得る。",

		"武器・防具にかけられたあらゆる魔力を完全に解除する。",
		"邪悪なモンスターの攻撃を防ぐバリアを張る。",
		"狂戦士化し、恐怖を除去し、加速する。",
		"アイテムを酸・電撃・火炎・冷気で傷つかないよう加工する。",
		"モンスターを朦朧とさせ、恐怖させる攻撃をする。",
		"武器の命中率修正とダメージ修正を強化する。",
		"鎧の防御修正を強化する。",
		"無敵のバリアすら切り裂く攻撃を行う。",
		"1ターンで2度攻撃を行う。",
		"遅鈍のブレスと同等の攻撃を行う。",
		"武器に滅邪の属性をつける。",
		"一定時間、あらゆる耐性を付け、ACと魔法防御能力を上昇させる。"
	},
	{
		"体力や傷を少し回復させる。",
		"祝福により攻撃精度と防御力が上がる。",
		"視界内のモンスターの動きを遅くする。",
		"炎のオーラを身にまとい、回復速度が速くなる。",
		"視界内のモンスターに微弱量の毒のダメージを与える。",
		"術者の腕力を上昇させる。",
		"装備している武器を呪う。",
		"周囲の邪悪なモンスターを感知する。",
		"暗黒への耐性を得る。",
		"氷のオーラを身にまとい、防御力が上昇する。",

		"体力や傷を多少回復させる。",
		"1体のモンスターを隣接するマスへ呼び寄せる。",
		"呪文詠唱を中止することなく、薬の効果を得ることができる。",
		"視界内のモンスターに微弱量の生命力吸収のダメージを与える。与えたダメージの分、体力が回復する。",
		"武器の攻撃力を上げる。切れ味を得、呪いに応じて与えるダメージが上昇し、善良なモンスターに対するダメージが2倍になる。",
		"地獄への耐性を得る。",
		"攻撃した際モンスターを混乱させる。",
		"術者の腕力、器用さ、耐久力を上昇させる。攻撃回数の上限を 1 増加させる。",
		"視界内のモンスターをテレポートさせる。",
		"視界内のモンスターのテレポートを阻害するバリアを張る。",

		"その階の増殖するモンスターの増殖を阻止する。",
		"体力や傷を回復させる。",
		"狂戦士化する。",
		"魔法の道具に魔力を再充填する。",
		"呪われた武器の呪いを吸収して魔力を回復する。",
		"経験値を徐々に復活し、減少した能力値を回復させる。",
		"吸血属性で攻撃する。",
		"視界内のモンスターを朦朧とさせる。",
		"モンスターの隣のマスに瞬間移動する。",
		"打撃や魔法で受けたダメージを、攻撃元のモンスターにも与える。",
		"視界内のモンスターの魔法を阻害するバリアを張る。",
		"数ターン後に、それまで受けたダメージに応じた威力の魔法の弾を、対象のモンスターがその時いた位置に放つ。",
	}
#else
	{
		"Detects all evil monsters in your vicinity.",
		"Heals cut and HP a little.",
		"Gives bonus to hit and AC for a few turns.",
		"Removes fear.",
		"Lights up nearby area and the inside of a room permanently.",
		"Detects traps, doors, and stairs in your vicinity.",
		"Heals cut and HP more.",
		"Satisfies hunger.",
		"Fires a beam of light which damages to light-sensitive monsters.",
		"Removes normal curses from equipped items.",

		"Heals cut, stun and HP greatly.",
		"Fires a ball with holy power. Hurts evil monsters greatly, but don't effect good monsters.",
		"Maps nearby area. Detects all monsters, traps, doors and stairs.",
		"Gives resistance to fire and cold. These resistances can be added to which from equipment for more powerful resistances.",
		"Attempts to charm a monster.",
		"Gives aura which protect you from evil monster's physical attack.",
		"Much powerful healing magic, and heals cut and stun completely.",
		"Teleports all evil monsters in sight away unless resisted.",
		"Fires a ball of light.",
		"Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.",

		"Removes fear, and gives bonus to hit and 10 more HP for a while.",
		"Removes normal and heavy curse from equipped items.",
		"Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.",
		"Damages all evil monsters in sight.",
		"Attempts to charm all monsters in sight.",
		"Damages all evil monsters in sight, heals HP somewhat, and completely heals poison, fear, stun and cut status.",
		"Blesses a weapon. Heavy cursed weapons resist it. Artifacts are disenchanted when blessing is failed."
		"Restores all stats and experience.",
		"Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.",
		"*Identifies* an item.",
		"The greatest healing magic. Heals all HP, cut and stun.",
		"Damages all adjacent monsters with holy power. Damages and attempt to slow, stun, confuse, scare and freeze all monsters in sight. And heals HP.",
	},
	{
		"Fires a weak bolt of magic.",
		"Detects all monsters in your vicinity unless invisible.",
		"Teleport short distance.",
		"Detects traps, doors, and stairs in your vicinity.",
		"Lights up nearby area and the inside of a room permanently.",
		"Fires a beam which destroy traps and doors.",
		"Teleport long distance.",
		"Satisfies hunger.",
		"Fires a ball of magic.",
		"Recalls player from dungeon to town, or from town to the deepest level of dungeon.",

		"Turns one rock square to mud.",
		"Recharges staffs, wands or rods.",
		"Maps nearby area.",
		"Identifies an item. Or *Identifies* an item at lev 35.",
		"Gives telepathy for a while.",
		"Fires a beam of magic.",
		"Hastes you for a while.",
		"Teleports all monsters on the line away unless resisted.",
		"Fires a magic rocket.",
		"Eliminates an entire class of monster, exhausting you.  Powerful or unique monsters may resist.",

		"Proves all monsters' alignment, HP, speed and their true character.",
		"Teleport to up or down stairs in a moment.",
		"Pulls a distant item close to you.",
		"Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.",
		"Destroy everything in nearby area.",
		"Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.",
		"Teleports all monsters in sight away unless resisted.",
		"Teleport to given location.",
		"Turns an item into 1/3 of its value in gold.",
		"Eliminates all nearby monsters, exhausting you.  Powerful or unique monsters may be able to resist.",
		"Fires an extremely powerful huge ball of pure mana.",
		"Becomes wraith form which gives ability to pass walls and makes all damages half."
	},
	{
		"Gives regeneration ability for a while.",
		"Attacks in 3 directions in one time.",
		"Removes fear, and gives bonus to hit and 10 more HP for a while.",
		"Steps close to a monster.",
		"Gives resistance to fire, cold and electricity for a while. These resistances can be added to which from equipment for more powerful resistances.",
		"Gives see everything in your sight for a while.",
		"Attacks monster with your weapons normally, then move through counter side of the monster.",
		"Fire a bolt of force.",
		"Gives bonus to AC for a while.",
		"Removes normal curses from equipped items.",

		"Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.",
		"Shakes dungeon structure, and results in random swapping of floors and walls.",
		"Steps close to a monster and attacks at a time.",
		"Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.",
		"Fire a shot/allow/bolt strongly.",
		"Attempts to attack with vorpal hit.",
		"Hastes you for a while.",
		"Attacks all adjacent monsters.",
		"Fire a beam of inertia.",
		"Gives resistance to magic, bonus to AC, resistance to confusion, blindness, reflection, free action and levitation for a while.",

		"Removes all magics completely from any weapon or armor.",
		"Gives aura which protect you from evil monster's physical attack.",
		"Gives another bonus to hit and HP, immunity to fear for a while. Hastes you. But decreases AC.",
		"Makes an equipment acid-proof.",
		"Smashing a monster terribly.",
		"Attempts to increase +to-hit, +to-dam of a weapon.",
		"Attempts to increase +AC of an armor.",
		"Attacks a monster and penetrates its barrier.",
		"Double attacks at a time.",
		"Fire a breath of inertia.",
		"Makes current weapon an evil slayer.",
		"Gives ultimate resistance, bonus to AC and speed."
	},
	{
		"Heals cut and HP a little.",
		"Attempts to increase +to_hit of a weapon and AC",
		"Makes all monsters slow in your sight.",
		"Gives fire aura and regeneration.",
		"Deals few damages of poison to all monsters in your sight.",
		"Attempts to increase your strength.",
		"Curses your weapon.",
		"Detects evil monsters.",
		"Gives resistance to dark.",
		"Gives fire aura and bonus to AC.",

		"Heals cut and HP more.",
		"Teleports a monster close to you.",
		"Quaffs a potion without canceling of casting a spell.",
		"Deals few dameges of drain life to all monsters in your sight.",
		"Gives vorpal ability to your weapon. Increases damages by your weapon acccording to curse of your weapon.",
		"Gives resistance to nether.",
		"Confuses a monster when you attack.",
		"Attempts to increases your strength, dexterity and constitusion.",
		"Teleport all monsters away in your sight.",
		"Obstructs all teleportations by monsters in your sight.",

		"Obstructs all multiplying by monsters in entire floor.",
		"Heals cut and HP greatry.",
		"Makes you berserk.",
		"Recharges a magic device.",
		"Drains curse on your weapon and heals SP a little.",
		"Restores life energy and status.",
		"Gives vampiric ability to your weapon.",
		"Stuns all monsters in your sight.",
		"Teleports you close to a monster.",
		"Returns same damage which you got to the monster which damaged you.",
		"Obstructs all magic spell of monsters in your sight.",
		"Fires magic ball to revenge after few turns.",
	},
#endif
};

byte get_modified_smana(magic_type *s_ptr)
{
	int tmp_mana;

	if (!s_ptr) return 0;
	tmp_mana = s_ptr->smana;

	if (p_ptr->dec_mana) tmp_mana = tmp_mana * 3 / 4;
	if (tmp_mana < 1) tmp_mana = 1;

	return (byte)tmp_mana;
}

/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */

static int get_spell(int *sn, cptr prompt, int sval, bool known, int use_realm)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	byte        spells[32];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
#ifdef JP
	char        jverb_buf[128];
	cptr        p = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	cptr        p = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(spells[i], known, use_realm - 1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Build a prompt (accept all spells) */
#ifdef JP
	jverb( prompt, jverb_buf, JVERB_AND );
	(void) strnfmt(out_val, 78, "(%^s:%c-%c, '*'で一覧, ESCで中断) どの%sを%^sますか? ",
		p, I2A(0), I2A(num - 1), p, jverb_buf );
#else
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);
#endif

	/* Get a spell from the user */
	choice = always_show_list ? ESCAPE : 1;
	while (!flag)
	{
		if( choice==ESCAPE ) choice = ' '; 
		else if( !get_com(out_val, &choice) )break; 

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 20, use_realm - 1);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}


		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known, use_realm - 1))
		{
			bell();
#ifdef JP
			msg_format("その%sを%sことはできません。", p, prompt);
#else
			msg_format("You may not %s that %s.", prompt, p);
#endif
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Prompt */
#ifdef JP
			jverb( prompt, jverb_buf, JVERB_AND);
			/* 英日切り替え機能に対応 */
			(void)strnfmt(tmp_val, 78, "%s(MP%d, 失敗率%d%%)を%sますか? ",
				spell_names[use_realm -1][spell % 32],
				get_modified_smana(s_ptr), spell_chance(spell, use_realm -1), jverb_buf);
#else
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm - 1][spell % 32],
				get_modified_smana(s_ptr), spell_chance(spell, use_realm - 1));
#endif

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();


	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int		item, sval, use_realm, j, line;
	int		spell = -1;
	int		num = 0;

	byte		spells[32];
	char		temp[62 * 4];

	object_type	*o_ptr;

	cptr q, s;

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
#ifdef JP
		msg_print("本を読むことができない！");
#else
		msg_print("You cannot read books!");
#endif
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの本を読みますか? ";
	s = "読める本がない。";
#else
	q = "Browse which book? ";
	s = "You have no books that you can read.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Access the item's sval */
	sval = o_ptr->sval;

	use_realm = tval2realm(o_ptr->tval);

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Save the screen */
	screen_save();

	/* Clear the top line */
	prt("", 0, 0);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
#ifdef JP
		if (!get_spell(&spell, "読む", o_ptr->sval, TRUE, use_realm))
#else
		if (!get_spell(&spell, "browse", o_ptr->sval, TRUE, use_realm))
#endif
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Display a list of spells */
			print_spells(spells, num, 1, 15, use_realm - 1);

			/* Notify that there's nothing to see, and wait. */
#ifdef JP
			prt("読める呪文がない。", 0, 0);
#else
			prt("No spells to browse.", 0, 0);
#endif
			(void)inkey();

			/* Restore the screen */
			screen_load();

			return;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, 18, 255);
		Term_erase(14, 17, 255);
		Term_erase(14, 16, 255);
		Term_erase(14, 15, 255);

		roff_to_buf(spell_tips[use_realm - 1][spell], 62, temp, sizeof(temp));
		for(j = 0, line = 15; temp[j]; j += (1 + strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
	}

	/* Restore the screen */
	screen_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	increment = 0;

	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

#ifdef JP
	cptr p = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	cptr p = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif

	object_type *o_ptr;

	cptr q, s;

	if (!p_ptr->realm1)
	{
#ifdef JP
		msg_print("本を読むことができない！");
#else
		msg_print("You cannot read books!");
#endif
		return;
	}

	if (p_ptr->blind || no_lite())
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You cannot see!");
#endif
		return;
	}

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて読めない！");
#else
		msg_print("You are too confused!");
#endif
		return;
	}

	if (!(p_ptr->new_spells))
	{
#ifdef JP
		msg_format("新しい%sを覚えることはできない！", p);
#else
		msg_format("You cannot learn any new %ss!", p);
#endif
		return;
	}

#ifdef JP
	if( p_ptr->new_spells < 10 )
		msg_format("あと %d つの%sを学べる。", p_ptr->new_spells, p);
	else
		msg_format("あと %d 個の%sを学べる。", p_ptr->new_spells, p);
#else
	msg_format("You can learn %d new %s%s.", p_ptr->new_spells, p,
		(p_ptr->new_spells == 1?"":"s"));
#endif
	msg_print(NULL);


	/* Restrict choices to "useful" books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの本から学びますか? ";
	s = "読める本がない。";
#else
	q = "Study which book? ";
	s = "You have no books that you can read.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Access the item's sval */
	sval = o_ptr->sval;

#if 0
	if (o_ptr->tval == REALM2_BOOK) increment = 32;
#endif

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_type != ST_PRAYER)
	{
		/* Ask for a spell, allow cancel */
#ifdef JP
		if (!get_spell(&spell, "学ぶ", sval, FALSE, tval2realm(o_ptr->tval))
		    && (spell == -1)) return;
#else
		if (!get_spell(&spell, "study", sval, FALSE, tval2realm(o_ptr->tval))
		    && (spell == -1)) return;
#endif
	}

	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check spells in the book */
			if ((fake_spell_flags[sval] & (1L << spell)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE,
					(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1))) continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (randint0(k) == 0) gift = spell;
			}
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
#ifdef JP
		msg_format("その本には学ぶべき%sがない。", p);
#else
		msg_format("You cannot learn any %ss in that book.", p);
#endif
		/* Abort */
		return;
	}


	/* Take a turn */
	energy_use = 100;

	if (increment) spell += increment;

	/* Learn the spell */
	if (spell < 32)
	{
		spell_learned1 |= (1L << spell);
	}
	else
	{
		spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	spell_order[i] = spell;

	/* Mention the result */
#ifdef JP
	/* 英日切り替え機能に対応 */
		msg_format("%sの%sを学んだ。",
			    spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32] ,p);
#else
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32]);
#endif

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
#ifdef JP
			if( p_ptr->new_spells < 10 )
				msg_format("あと %d つの%sを学べる。", p_ptr->new_spells, p);
			else
				msg_format("あと %d 個の%sを学べる。", p_ptr->new_spells, p);
#else
		msg_format("You can learn %d more %s%s.",
			p_ptr->new_spells, p,
			(p_ptr->new_spells != 1) ? "s" : "");
#endif
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}


static bool cast_life_spell(int spell)
{
	int	dir;
	int	plev = p_ptr->lev;

	switch (spell)
	{
	case 0: /* Detect Evil */
		(void)detect_monsters_evil(DETECT_RAD_DEFAULT);
		break;
	case 1: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 2: /* Bless */
		(void)set_blessed(p_ptr->blessed + randint1(12) + 12);
		break;
	case 3: /* Remove Fear */
		(void)set_afraid(0);
		break;
	case 4: /* Call Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 5: /* Detect Traps + Secret Doors */
		(void)detect_traps(DETECT_RAD_DEFAULT);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		break;
	case 6: /* Cure Medium Wounds */
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 20);
		break;
	case 7: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 8: /* Holy Lite */
		if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
		msg_print("聖なる光線が現れた。");
#else
		msg_print("A line of sunlight appears.");
#endif
		(void)lite_line(dir);
		break;
	case 9: /* Remove Curse */
		if (remove_curse())
		{
#ifdef JP
			msg_print("誰かに見守られているような気がする。");
#else
			msg_print("You feel as if someone is watching over you.");
#endif
		}
		break;
	case 10: /* Cure Critical Wounds */
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 11: /* Orb or Draining */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_HOLY_FIRE, dir,
			  (damroll(3, 6) + plev +
			  (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
			  ((plev < 30) ? 2 : 3));
		break;
	case 12: /* Nature Awareness -- downgraded */
		map_area(DETECT_RAD_MAP);
		(void)detect_traps(DETECT_RAD_DEFAULT);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 13: /* Resistance Heat & Cold */
		(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20);
		break;
	case 14: /* Charm Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_monster(dir, plev);
		break;
	case 15: /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * p_ptr->lev);
		break;
	case 16: /* Healing */
		(void)hp_player(300);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 17: /* Banishment */
		if (banish_evil(100))
		{
#ifdef JP
			msg_print("神の御力が邪悪を打ち払った！");
#else
			msg_print("The power of your god banishes evil!");
#endif
		}
		break;
	case 18: /* Star Burst */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_LITE, dir, (100 + plev * 2), 3);
		break;
	case 19: /* Glyph of Warding */
		warding_glyph();
		break;
	case 20: /* Heroism */
		(void)set_hero(p_ptr->hero + randint1(25) + 25);
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 21: /* Dispel Curse */
		if (remove_all_curse())
		{
#ifdef JP
			msg_print("誰かに見守られているような気がする。");
#else
			msg_print("You feel as if someone is watching over you.");
#endif
		}
		break;
	case 22: /* Resistance True */
		(void)set_oppose_acid(p_ptr->oppose_acid + randint1(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint1(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20);
		(void)set_oppose_pois(p_ptr->oppose_pois + randint1(20) + 20);
		break;
	case 23: /* Dispel Evil */
		(void)dispel_evil(plev * 4);
		break;
	case 24: /* 'Day of the Dove' */
		charm_monsters(plev * 2);
		break;
	case 25: /* Holy Word */
		(void)dispel_evil(plev * 4);
		(void)hp_player(500);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 26: /* Bless Weapon */
		return bless_weapon();
	case 27: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 28: /* Nanka(神聖なる光) */
		project(0, 8, py, px, 150, GF_LITE, PROJECT_KILL | PROJECT_ITEM);
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		wiz_lite();
		break;
	case 29: /* Holy Vision */
		return identify_fully();
	case 30: /* Healing True */
		(void)hp_player(1000);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 31: /* Divine Intervention */
		project(0, 1, py, px, 600, GF_HOLY_FIRE, PROJECT_KILL);
		dispel_monsters(plev * 4);
		slow_monsters();
		stun_monsters(plev * 4);
		confuse_monsters(plev * 4);
		turn_monsters(plev * 4);
		stasis_monsters(plev * 4);
		(void)set_shero(p_ptr->shero + randint1(25) + 25);
		(void)hp_player(300);
		if (!p_ptr->fast)
		{   /* Haste */
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		(void)set_afraid(0);
		break;
	default:
#ifdef JP
		msg_format("あなたは不明なライフの呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Life spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_sorcery_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else beam = plev / 2;

	switch (spell)
	{
	case 0: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 1: /* Detect Monsters */
		(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
		break;
	case 2: /* Phase Door */
		teleport_player(10);
		break;
	case 3: /* Detect Doors and Traps */
		(void)detect_traps(DETECT_RAD_DEFAULT);
		(void)detect_doors(DETECT_RAD_DEFAULT);
		(void)detect_stairs(DETECT_RAD_DEFAULT);
		break;
	case 4: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 5: /* Trap & Door Destruction */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)destroy_door(dir);
		break;
	case 6: /* Teleport Self */
		teleport_player(plev * 5);
		break;
	case 7: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 8: /* Manaburst */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_MISSILE, dir,
			  (damroll(3, 5) + plev +
			   (plev / ((p_ptr->pclass == CLASS_MAGE) ? 2 : 4))),
			   (plev < 30) ? 2 : 3);
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
		break;
	case 9: /* Word of Recall */
		word_of_recall();
		break;
	case 10: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 11: /* Recharging */
		return recharge(plev * 4);
	case 12: /* Magic Mapping */
		map_area(DETECT_RAD_MAP);
		break;
	case 13: /* Identify */
		if (plev < 35) return ident_spell();
		else return identify_fully();
	case 14: /* Sense Minds */
		(void)set_tim_esp(p_ptr->tim_esp + randint1(30) + 25);
		break;
	case 15: /* Doom Bolt -- always beam in 2.0.7 or later */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
		break;
	case 16: /* Haste Self */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 17: /* Teleport Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 18: /* Magic Rocket */
		if (!get_aim_dir(&dir)) return FALSE;

#ifdef JP
		msg_print("ロケット発射！");
#else
		msg_print("You launch a rocket!");
#endif
		sound(SOUND_ROCKET);
		fire_ball(GF_ROCKET, dir, (120 + plev * 2), 2);
		break;
	case 19: /* Genocide */
		(void)genocide(plev + 50);
		break;
	case 20: /* Probing */
		probing();
		break;
	case 21: /* Teleport Level */
		(void)teleport_player_level();
		break;
	case 22: /* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;

		fetch(dir, plev * 15 / 10, FALSE);
		break;
	case 23: /* Detection */
		(void)detect_all(DETECT_RAD_DEFAULT);
		break;
	case 24: /* Word of Destruction */
		destroy_area(py, px, 15, TRUE);
		break;
	case 25: /* Clairvoyance */
		wiz_lite();
		if (!p_ptr->telepathy)
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint1(30) + 25);
		}
		break;
	case 26: /* Banish Monsters */
		banish_monsters(MAX_SIGHT * 5);
		break;
	case 27: /* Dimension Door */
#ifdef JP
		msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
		msg_print("You open a dimensional gate. Choose a destination.");
#endif
		return dimension_door();
	case 28: /* Alchemy */
		return alchemy();
	case 29: /* Mass Genocide */
		(void)mass_genocide(plev + 50);
		break;
	case 30: /* Mana Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_MANA, dir, 300 + (plev * 5), 4);
		break;
	case 31: /* Wraithform */
		set_wraith_form(p_ptr->tim_wraith + randint1(plev / 2) + (plev / 2));
		break;
	default:
#ifdef JP
		msg_format("あなたは不明なソーサリーの呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Sorcery spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_musou_spell(int spell)
{
	int	    dir;
	int	    plev = p_ptr->lev;
	int	    x, y;

	switch (spell)
	{
	case 0: /* Regenerate */
		(void)set_tim_regen(p_ptr->tim_regen + 80 + randint1(80));
		break;
	case 1: /* 3way attack */
	{
		int cdir;
		if (!get_rep_dir(&dir)) return FALSE;

		if (dir == 5) return FALSE;
		for (cdir = 0;cdir < 8; cdir++)
		{
			if (cdd[cdir] == dir) break;
		}
		if (cdir == 8) return FALSE;
		y = py + ddy_cdd[cdir];
		x = px + ddx_cdd[cdir];
		if (cave[y][x].m_idx)
			py_attack(y, x);
		else
#ifdef JP
			msg_print("攻撃は空を切った。");
#else
			msg_print("You attack the empty air.");
#endif
		y = py + ddy_cdd[(cdir + 7) % 8];
		x = px + ddx_cdd[(cdir + 7) % 8];
		if (cave[y][x].m_idx)
			py_attack(y, x);
		else
#ifdef JP
			msg_print("攻撃は空を切った。");
#else
			msg_print("You attack the empty air.");
#endif
		y = py + ddy_cdd[(cdir + 1) % 8];
		x = px + ddx_cdd[(cdir + 1) % 8];
		if (cave[y][x].m_idx)
			py_attack(y, x);
		else
#ifdef JP
			msg_print("攻撃は空を切った。");
#else
			msg_print("You attack the empty air.");
#endif
		break;
	}
	case 2: /* Heroism */
		(void)set_hero(p_ptr->hero + randint1(25) + 25);
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 3: /* Jumping */
		project_length = 5;
		if (!get_aim_dir(&dir)) return FALSE;

		project_hook(GF_JUMP, dir, 0, PROJECT_STOP);
		project_length = 0;
		break;
	case 4: /* Resist Environment */
		(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint1(20) + 20);
		break;
	case 5: /* Radar Eye */
		(void)set_tim_radar(p_ptr->tim_radar + randint1(25) + 30);
		break;
	case 6: /* Charge */
		return charge_monster();
	case 7: /* Slash Wind */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt(GF_FORCE, dir, damroll(3 + ((plev - 5) / 4), 8));
		break;
	case 8: /* Stone Skin */
		(void)set_shield(p_ptr->shield + randint1(25) + 30);
		break;
	case 9: /* Remove Curse */
		if (remove_curse())
		{
#ifdef JP
			msg_print("誰かに見守られているような気がする。");
#else
			msg_print("You feel as if someone is watching over you.");
#endif
		}
		break;
	case 10: /* Berserk */
		(void)set_shero(p_ptr->shero + randint1(25) + 25);
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 11: /* Earthquake */
		earthquake(py, px, 10);
		break;
	case 12: /* Jump and Attack */
		project_length = 5;
		if (!get_aim_dir(&dir)) return FALSE;

		project_hook(GF_JUMP_ATTACK, dir, 0, PROJECT_STOP | PROJECT_KILL);
		project_length = 0;
		break;
	case 13: /* Resistance True */
		(void)set_oppose_acid(p_ptr->oppose_acid + randint1(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint1(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20);
		(void)set_oppose_pois(p_ptr->oppose_pois + randint1(20) + 20);
		break;
	case 14: /* Aura Arrow */
		superb_shot = TRUE;
		command_cmd = 'f';
		(void)do_cmd_fire();
		superb_shot = FALSE;
		break;
	case 15: /* Fast Slicing */
		if (!get_rep_dir(&dir)) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		if(cave[y][x].m_idx)
		{
			py_attack_special(y, x, ATTACK_VORPAL);
		}
		else
		{
#ifdef JP
			msg_print("その方向にはモンスターはいません。");
#else
			msg_print("You don't see any monster in this direction");
#endif
			return FALSE;
		}
		break;
	case 16: /* Haste Self */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 17: /* Whirlwind Attack */
		{
			int y, x;
			cave_type       *c_ptr;
			monster_type    *m_ptr;

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
					py_attack(y, x);
			}
		}
		break;
	case 18: /* Nanka(疾風斬) */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(GF_INERTIA, dir, (plev * 3 / 2) + randint1(plev * 2));
		break;
	case 19: /* Magical Armour */
		(void)set_magicdef(p_ptr->magicdef + randint1(20) + 20);
		break;
	case 20: /* Remove Enchantment */
		if(!mundane_spell(TRUE)) return FALSE;
		break;
	case 21: /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * p_ptr->lev);
		break;
	case 22: /* Battle Frenzy */
		(void)set_shero(p_ptr->shero + randint1(25) + 25);
		(void)hp_player(30);
		(void)set_afraid(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + (plev / 2)) + (plev / 2));
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 23: /* Coating */
		(void)coat_equip();
		break;
	case 24: /* Bombing Attack */
		if (!get_rep_dir(&dir)) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		if(cave[y][x].m_idx)
		{
			py_attack_special(y, x, ATTACK_BAKUSIN);
		}
		else
		{
#ifdef JP
			msg_print("その方向にはモンスターはいません。");
#else
			msg_print("You don't see any monster in this direction");
#endif
			return FALSE;
		}
		break;
	case 25: /* Enchant Weapon */
		return enchant_spell(randint0(4) + 1, randint0(4) + 1, 0);
	case 26: /* Enchant Armour */
		return enchant_spell(0, 0, randint0(3) + 2);
	case 27: /* Nanka(闘気の剣) */
		if (!get_rep_dir(&dir)) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		if(cave[y][x].m_idx)
		{
			py_attack_special(y, x, ATTACK_TOUKI);
		}
		else
		{
#ifdef JP
			msg_print("その方向にはモンスターはいません。");
#else
			msg_print("You don't see any monster in this direction");
#endif
			return FALSE;
		}
		break;
	case 28: /* Nanka(隼斬り) */
		if (!get_rep_dir(&dir)) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		if(cave[y][x].m_idx)
		{
			py_attack(y, x);
		}
		else
		{
#ifdef JP
			msg_print("その方向にはモンスターはいません。");
#else
			msg_print("You don't see any monster in this direction");
#endif
			return FALSE;
		}
		if (cave[y][x].m_idx)
		{
			handle_stuff();
			py_attack(y, x);
		}
		break;
	case 29: /* Nanka(竜巻斬) */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_INERTIA, dir, 175+randint1(plev*2), -3);
		break;
	case 30: /* Nanka(滅邪の刃) */
		brand_weapon(5);
		break;
	case 31: /* Nanka(無双) */
	{
		int v = randint1(25) + 25;
		(void)set_hero(p_ptr->hero + v);
		(void)set_shero(p_ptr->shero + v);
		(void)hp_player(30);
		(void)set_afraid(0);
		if (p_ptr->fast < v)
		{
			(void)set_fast(v);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		(void)set_shield(p_ptr->shield + v);
		(void)set_oppose_acid(p_ptr->oppose_acid + v);
		(void)set_oppose_elec(p_ptr->oppose_elec + v);
		(void)set_oppose_fire(p_ptr->oppose_fire + v);
		(void)set_oppose_cold(p_ptr->oppose_cold + v);
		(void)set_oppose_pois(p_ptr->oppose_pois + v);
		(void)set_musou(p_ptr->musou + v);
		break;
	}
	default:
#ifdef JP
		msg_format("あなたは不明な無双の呪文 %d を唱えた。", spell);
#else
		msg_format("You cast an unknown Musou spell: %d.", spell);
#endif
		msg_print(NULL);
	}

	return TRUE;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, realm;
	int	chance;
	int	increment = 0;
	int	use_realm;
	bool cast;
	byte use_mana;

#ifdef JP
	const cptr prayer = ((mp_ptr->spell_type == ST_PRAYER) ? "祈り" : "呪文");
#else
	const cptr prayer = ((mp_ptr->spell_type == ST_PRAYER) ? "prayer" : "spell");
#endif

	object_type	*o_ptr;

	magic_type	*s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->realm1)
	{
#ifdef JP
		msg_print("呪文を唱えられない！");
#else
		msg_print("You cannot cast spells!");
#endif
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You cannot see!");
#endif
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて学べない！");
#else
		msg_print("You are too confused!");
#endif
		return;
	}

	/* Restrict choices to spell books */
	item_tester_tval = TV_LIFE_BOOK;

	/* Get an item */
#ifdef JP
	q = "どの呪文書を使いますか? ";
	s = "呪文書がない！";
#else
	q = "Use which book? ";
	s = "You have no spell books!";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Access the item's sval */
	sval = o_ptr->sval;

#if 0
	if (o_ptr->tval == REALM2_BOOK) increment = 32;
#endif

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;

	/* Ask for a spell */
#ifdef JP
	if (!get_spell(&spell,  
		((mp_ptr->spell_type == ST_PRAYER) ? "詠唱する" : "唱える"),
		sval, TRUE, realm))
	{
		if (spell == -2) msg_format("その本には知っている%sがない。", prayer);
		return;
	}
#else
	if (!get_spell(&spell, ((mp_ptr->spell_type == ST_PRAYER) ? "recite" : "cast"),
		sval, TRUE, realm))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}
#endif



	/* Access the spell */
	use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

	s_ptr = &mp_ptr->info[use_realm - 1][spell];
	use_mana = get_modified_smana(s_ptr);

	/* Verify "dangerous" spells */
	if (use_mana > p_ptr->csp)
	{
		/* Warning */
#ifdef JP
		msg_format("その%sを%sのに十分なマジックポイントがない。",prayer,
			((mp_ptr->spell_type == ST_PRAYER) ? "詠唱する" : "唱える"));
#else
		msg_format("You do not have enough mana to %s this %s.",
			((mp_ptr->spell_type == ST_PRAYER) ? "recite" : "cast"),
			prayer);
#endif
		if (!over_exert) return;

		/* Verify */
#ifdef JP
		if (!get_check("それでも挑戦しますか? ")) return;
#else
		if (!get_check("Attempt it anyway? ")) return;
#endif
	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

#ifdef JP
		msg_format("%sをうまく唱えられなかった！", prayer);
#else
		msg_format("You failed to get the %s off!", prayer);
#endif
		sound(SOUND_FAIL);
	}

	/* Process spell */
	else
	{
		/* Spells.  */
		switch (realm)
		{
		case REALM_LIFE: /* * LIFE * */
			cast = cast_life_spell(spell);
			break;
		case REALM_SORCERY: /* * SORCERY * */
			cast = cast_sorcery_spell(spell);
			break;
		case REALM_MUSOU: /* * MUSOU * */
			cast = cast_musou_spell(spell);
			break;
		case REALM_MAGIC: /* * MAGIC * */
			cast = cast_magic_spell(spell);	/* MAGIC */
			break;
		default:
			cast = FALSE;
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(increment ?
		    (spell_worked2 & (1L << spell)) :
		    (spell_worked1 & (1L << spell))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (realm == p_ptr->realm1)
			{
				spell_worked1 |= (1L << spell);
			}
			else
			{
				spell_worked2 |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);
		}
	}

	/* Take a turn */
	if (!((realm == REALM_MUSOU) && (spell == 13)))
		energy_use = 100;
	else /* Aura Arrow */
	{
		object_type *j_ptr = &inventory[INVEN_BOW];

		if (j_ptr->tval)
		{
			int thits = p_ptr->num_fire;

			energy_use = bow_energy(j_ptr->sval);
			energy_use = (energy_use * 100 / thits);
		}
		else
		{
			energy_use = 100;
		}
	}

	/* Sufficient mana */
	if (use_mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= use_mana;
	}

	/* Over-exert the player */
	else
	{
		int oops = use_mana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
#ifdef JP
		msg_print("精神を集中しすぎて気を失ってしまった！");
#else
		msg_print("You faint from the effort!");
#endif

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
#ifdef JP
			msg_print("体を悪くしてしまった！");
#else
			msg_print("You have damaged your health!");
#endif

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint1(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msg_print("Praying is not used in Zangband. Use magic spell casting instead.");
}



int calculate_upkeep(void)
{
	int i;

	monster_type    *m_ptr;
	monster_race    *r_ptr;

	int old_total_friends = total_friends; 
	s32b old_friend_align = friend_align;

	/* Clear some variables */
	total_friends = 0;
	total_friend_levels = 0;
	friend_align = 0;

	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			total_friends++;
			if (r_ptr->flags1 & (RF1_UNIQUE))
				total_friend_levels += (r_ptr->level + 5) * 10;
			else
				total_friend_levels += r_ptr->level;

			/* Determine pet alignment */
			if (r_ptr->flags3 & RF3_GOOD)
			{
				friend_align += r_ptr->level;
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				friend_align -= r_ptr->level;
			}
		}
	}

	if (old_friend_align != friend_align) p_ptr->update |= (PU_BONUS);
	
	if (total_friends)
	{
#ifdef TRACK_FRIENDS
		if (wizard)
#ifdef JP
			msg_format("友好的モンスター: %d 体", total_friends);
#else
			msg_format("Total friends: %d.", total_friends);
#endif

#endif /* TRACK_FRIENDS */

		if (total_friends)
		{
			int upkeep_factor = (total_friend_levels - (p_ptr->lev * 80 / cp_ptr->pet_upkeep_div));

			if (upkeep_factor > 1000) upkeep_factor = 1000;
			else if (upkeep_factor < 0) upkeep_factor = 0;

#ifdef TRACK_FRIENDS
			if (wizard)
#ifdef JP
			msg_format("レベル %d, 維持ＭＰ %d", total_friend_levels, upkeep_factor);
#else
			msg_format("Levels %d, upkeep %d", total_friend_levels, upkeep_factor);
#endif

#endif /* TRACK_FRIENDS */

			return upkeep_factor;
		}
	}

	return 0;
}

/*
 * Dismiss pets
 */
void do_cmd_pet_dismiss(void)
{
	int pet_ctr;
	bool all_pets = FALSE;
	int Dismissed = 0;
	monster_type *m_ptr;

#ifdef JP
	if (get_check("すべてのペットを放しますか？")) all_pets = TRUE;
#else
	if (get_check("Dismiss all pets? ")) all_pets = TRUE;
#endif

	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			bool delete_this = FALSE;

			if (all_pets)
				delete_this = TRUE;
			else
			{
				char friend_name[80], check_friend[80];
				monster_desc(friend_name, m_ptr, 0x80);

				/* Hack -- health bar for this monster */
				health_track(pet_ctr);

				/* Hack -- handle stuff */
				handle_stuff();

#ifdef JP
				sprintf(check_friend, "%sを放しますか？ [Yes/No/All]", friend_name);
#else
				sprintf(check_friend, "Dismiss %s? [Yes/No/All]", friend_name);
#endif
				prt(check_friend, 0, 0);

				if (m_ptr->ml)
					move_cursor_relative(m_ptr->fy, m_ptr->fx);

				while (TRUE)
				{
					int ch = inkey();

					if ((ch == 'Y') || (ch == 'y'))
					{
						delete_this = TRUE;
						break;
					}
					else if ((ch == 'A') || (ch == 'a'))
					{
						delete_this = TRUE;
						all_pets = TRUE;
						break;
					}
					else if ((ch == ESCAPE) || (ch == 'N') || (ch == 'n'))
						break;

					bell();
				}
			}

			if (delete_this)
			{
				delete_monster_idx(pet_ctr);
				Dismissed++;
			}
		}
	}

	Term_erase(0, 0, 255);
#ifdef JP
	msg_format("%d 匹のペットを放しました。", Dismissed);
#else
	msg_format("You have dismissed %d pet%s.", Dismissed,
		(Dismissed == 1 ? "" : "s"));
#endif
	return;
}

		
/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	int			i = 0;
	int			num;
	int			powers[36];
	cptr			power_desc[36];
	bool			flag, redraw;
	int			ask;
	char			choice;
	char			out_val[160];
	int			pets = 0, pet_ctr;
	monster_type	*m_ptr;

	int mode = 0;

	byte y = 1, x = 0;
	int ctr = 0;
	char buf[160];

	num = 0;

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
	}

	if (pets)
	{
#ifdef JP
		power_desc[num] = "ペットを放す";
#else
		power_desc[num] = "dismiss pets";
#endif
		powers[num++] = PET_DISMISS;
	}

#ifdef JP
	power_desc[num] = "近くにいろ";
#else
	power_desc[num] = "stay close";
#endif

	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
	powers[num++] = PET_STAY_CLOSE;

#ifdef JP
	power_desc[num] = "ついて来い";
#else
	power_desc[num] = "follow me";
#endif

	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
	powers[num++] = PET_FOLLOW_ME;

#ifdef JP
	power_desc[num] = "敵を見つけて倒せ";
#else
	power_desc[num] = "seek and destroy";
#endif

	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
	powers[num++] = PET_SEEK_AND_DESTROY;

#ifdef JP
	power_desc[num] = "少し離れていろ";
#else
	power_desc[num] = "give me space";
#endif

	if (p_ptr->pet_follow_distance == PET_SPACE_DIST) mode = num;
	powers[num++] = PET_ALLOW_SPACE;

#ifdef JP
	power_desc[num] = "離れていろ";
#else
	power_desc[num] = "stay away";
#endif

	if (p_ptr->pet_follow_distance == PET_AWAY_DIST) mode = num;
	powers[num++] = PET_STAY_AWAY;

	if (p_ptr->pet_open_doors)
	{
#ifdef JP
		power_desc[num] = "ドアを開けさせる";
#else
		power_desc[num] = "pets may open doors";
#endif
	}
	else
	{
#ifdef JP
		power_desc[num] = "ドアを開けさせない";
#else
		power_desc[num] = "pets may not open doors";
#endif
	}
	powers[num++] = PET_OPEN_DOORS;

	if (p_ptr->pet_pickup_items)
	{
#ifdef JP
		power_desc[num] = "アイテムを拾わせる";
#else
		power_desc[num] = "pets may pick up items";
#endif
	}
	else
	{
#ifdef JP
		power_desc[num] = "アイテムを拾わせない";
#else
		power_desc[num] = "pets may not pick up items";
#endif

	}
	powers[num++] = PET_TAKE_ITEMS;

	/* Nothing chosen yet */
	flag = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
#ifdef JP
		strnfmt(out_val, 78, "(コマンド %c-%c、'*'=一覧、ESC=終了) コマンドを選んでください:",
#else
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
#endif
			I2A(0), I2A(num - 1));
	}
	else
	{
#ifdef JP
		strnfmt(out_val, 78, "(コマンド %c-%c、'*'=一覧、ESC=終了) コマンドを選んでください:",
#else
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
#endif
			I2A(0), '0' + num - 27);
	}

	/* Show list */
	redraw = TRUE;

	/* Save the screen */
	Term_save();

	prt("", y++, x);

	while (ctr < num)
	{
		sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
		prt(buf, y + ctr, x);
		ctr++;
	}

	if (ctr < 17)
	{
		prt("", y + ctr, x);
	}
	else
	{
		prt("", y + 17, x);
	}

	/* Get a command from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				y = 1;
				x = 0;
				ctr = 0;

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt("", y++, x);

				while (ctr < num)
				{
					sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
					prt(buf, y + ctr, x);
					ctr++;
				}

				if (ctr < 17)
				{
					prt("", y + ctr, x);
				}
				else
				{
					prt("", y + 17, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Prompt */
#ifdef JP
			strnfmt(buf, 78, "%sを使いますか？ ", power_desc[i]);
#else
			strnfmt(buf, 78, "Use %s? ", power_desc[i]);
#endif

			/* Belay that order */
			if (!get_check(buf)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}

	switch (powers[i])
	{
		case PET_DISMISS: /* Dismiss pets */
		{
			do_cmd_pet_dismiss();
			break;
		}
		/* Call pets */
		case PET_STAY_CLOSE:
		{
			p_ptr->pet_follow_distance = PET_CLOSE_DIST;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
			break;
		}
		/* "Seek and destoy" */
		case PET_SEEK_AND_DESTROY:
		{
			p_ptr->pet_follow_distance = PET_DESTROY_DIST;
			break;
		}
		/* "Give me space" */
		case PET_ALLOW_SPACE:
		{
			p_ptr->pet_follow_distance = PET_SPACE_DIST;
			break;
		}
		/* "Stay away" */
		case PET_STAY_AWAY:
		{
			p_ptr->pet_follow_distance = PET_AWAY_DIST;
			break;
		}
		/* flag - allow pets to open doors */
		case PET_OPEN_DOORS:
		{
			p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
			break;
		}
		/* flag - allow pets to pickup items */
		case PET_TAKE_ITEMS:
		{
			p_ptr->pet_pickup_items = !p_ptr->pet_pickup_items;

			/* Drop objects being carried by pets */
			if (!p_ptr->pet_pickup_items)
			{
				for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
				{
					/* Access the monster */
					m_ptr = &m_list[pet_ctr];

					if (is_pet(m_ptr))
					{
						monster_drop_carried_objects(m_ptr);
					}
				}
			}

			break;
		}
	}
}
