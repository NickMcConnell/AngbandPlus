/* File: store.c */

/* Purpose: Store commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


#ifdef JP
/* 下の方から移動してきました */
static int cur_store_num = 0;
static int store_top = 0;
static store_type *st_ptr = NULL;
static owner_type *ot_ptr = NULL;
#endif
static s16b old_town_num;
static s16b cur_town_num;
#define RUMOR_CHANCE 8

#define MAX_COMMENT_1	6

static cptr comment_1[MAX_COMMENT_1] =
{
#ifdef JP
	"オーケーだ。",
	"結構だ。",
	"そうしよう！",
	"賛成だ！",
	"よし！",
	"わかった！"
#else
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
#endif

};

#ifdef JP
/* ブラックマーケット追加メッセージ（承諾） */
static cptr comment_1_B[MAX_COMMENT_1] = {
	"まあ、それでいいや。",
	"今日はそれで勘弁してやる。",
	"分かったよ。",
	"しょうがない。",
	"それで我慢するよ。",
	"こんなもんだろう。"
};
#endif
#define MAX_COMMENT_2A	2

static cptr comment_2a[MAX_COMMENT_2A] =
{
#ifdef JP
	"私の忍耐力を試しているのかい？ $%s が最後だ。",
	"我慢にも限度があるぞ。 $%s が最後だ。"
#else
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
#endif

};

#define MAX_COMMENT_2B	12

static cptr comment_2b[MAX_COMMENT_2B] =
{
#ifdef JP
	" $%s ぐらいは出さなきゃダメだよ。",
	" $%s なら受け取ってもいいが。",
	"ハ！ $%s 以下はないね。",
	"何て奴だ！ $%s 以下はあり得ないぞ。",
	"それじゃ少なすぎる！ $%s は欲しいところだ。",
	"バカにしている！ $%s はもらわないと。",
	"嘘だろう！ $%s でどうだい？",
	"おいおい！ $%s を考えてくれないか？",
	"1000匹のオークのノミに苦しめられるがいい！ $%s だ。",
	"お前の大切なものに災いあれ！ $%s でどうだ。",
	"モルゴスに賞味されるがいい！本当は $%s なんだろう？",
	"お前の母親はオーガか！ $%s は出すつもりなんだろ？"
#else
	"I can take no less than %s gold pieces.",
	"I will accept no less than %s gold pieces.",
	"Ha!  No less than %s gold pieces.",
	"You knave!  No less than %s gold pieces.",
	"That's a pittance!  I want %s gold pieces.",
	"That's an insult!  I want %s gold pieces.",
	"As if!  How about %s gold pieces?",
	"My arse!  How about %s gold pieces?",
	"May the fleas of 1000 orcs molest you!  Try %s gold pieces.",
	"May your most favourite parts go moldy!  Try %s gold pieces.",
	"May Morgoth find you tasty!  Perhaps %s gold pieces?",
	"Your mother was an Ogre!  Perhaps %s gold pieces?"
#endif

};

#ifdef JP
/* ブラックマーケット用追加メッセージ（売るとき） */
static cptr comment_2b_B[MAX_COMMENT_2B] = {
	"いくら俺様がお人好しとはいえ $%s が限界だね。嫌なら帰りな。",
	"金がないのかい、あんた？まずは家に帰って $%s 揃えてきな。",
	"物の価値が分からん奴だな。これは $%s が普通なんだよ。",
	"俺の付けた値段に文句があるのか？ $%s が限界だ。",
	"ひょっとして新手の冗談かい？ $%s 持ってないなら帰りな。",
	"うちは他の店とは違うんだよ。$%s ぐらいは出しな。",
	"買う気がないなら帰りな。 $%s だと言っているんだ。",
	"話にならないね。 $%s くらい持っているんだろ？",
	"は？なんだそりゃ？ $%s の間違いか、ひょっとして？",
	"出口はあっちだよ。それとも $%s 出せるのかい、あんたに。",
	"命知らずな奴だな。 $%s 出せば今日の所は勘弁してやるよ。",
	"うちの店は貧乏人お断りだ。 $%s ぐらい出せないのかい？"
};
#endif
#define MAX_COMMENT_3A	2

static cptr comment_3a[MAX_COMMENT_3A] =
{
#ifdef JP
	"私の忍耐力を試しているのかい？ $%s が最後だ。",
	"我慢にも限度があるぞ。 $%s が最後だ。"
#else
	"You try my patience.  %s is final.",
	"My patience grows thin.  %s is final."
#endif

};


#define MAX_COMMENT_3B	12

static cptr comment_3b[MAX_COMMENT_3B] =
{
#ifdef JP
	"本音を言うと $%s でいいんだろ？",
	" $%s でどうだい？",
	" $%s ぐらいなら出してもいいが。",
	" $%s 以上払うなんて考えられないね。",
	"まあ落ちついて。 $%s でどうだい？",
	"そのガラクタなら $%s で引き取るよ。",
	"それじゃ高すぎる！ $%s がいいとこだろ。",
	"どうせいらないんだろ！ $%s でいいだろ？",
	"だめだめ！ $%s がずっとお似合いだよ。",
	"バカにしている！ $%s がせいぜいだ。",
	" $%s なら嬉しいところだがなあ。",
	" $%s 、それ以上はビタ一文出さないよ！"
#else
	"Perhaps %s gold pieces?",
	"How about %s gold pieces?",
	"I will pay no more than %s gold pieces.",
	"I can afford no more than %s gold pieces.",
	"Be reasonable.  How about %s gold pieces?",
	"I'll buy it as scrap for %s gold pieces.",
	"That is too much!  How about %s gold pieces?",
	"That looks war surplus!  Say %s gold pieces?",
	"Never!  %s is more like it.",
	"That's an insult!  %s is more like it.",
	"%s gold pieces and be thankful for it!",
	"%s gold pieces and not a copper more!"
#endif

};

#ifdef JP
/* ブラックマーケット用追加メッセージ（買い取り） */
static cptr comment_3b_B[MAX_COMMENT_3B] = {
	" $%s ってところだね。そのどうしようもないガラクタは。",
	"この俺が $%s って言っているんだから、その通りにした方が身のためだぞ。",
	"俺の優しさに甘えるのもいい加減にしておけ。 $%s だ。",
	"その品なら $%s で売ってくれているがね、常識ある紳士はみんな。",
	"こりゃまた、がめつい奴だな。いくら俺が温厚とはいえ $%s が限界だ。",
	" $%s だ。別に俺はそんなガラクタ欲しくはないんだから。",
	"俺の鑑定額が気に入らないのか？ $%s 、嫌なら帰りな。",
	" $%s で引き取ってやるよ。喜んで受け取りな、貧乏人。",
	"物の価値が分からん奴は始末におえんな。それは $%s なんだよ。",
	"そんなに金が欲しいのか、あんた？ $%s で満足できんのか？",
	"入る店間違えてんじゃないのか？ $%s で嫌なら他をあたってくれ。",
	"俺の言い値にケチをつける奴がいるとは！ その度胸に免じて $%s だ。"
};
#endif
#define MAX_COMMENT_4A	4

static cptr comment_4a[MAX_COMMENT_4A] =
{
#ifdef JP
	"もうたくさんだ！何度も私をわずらわせないでくれ！",
	"うがー！一日の我慢の限度を超えている！",
	"もういい！時間の無駄以外のなにものでもない！",
	"もうやってられないよ！顔も見たくない！"
#else
	"Enough!  You have abused me once too often!",
	"Arghhh!  I have had enough abuse for one day!",
	"That does it!  You shall waste my time no more!",
	"This is getting nowhere!  I'm going to Londis!"
#endif

};

#ifdef JP
/* ブラックマーケット用追加メッセージ（怒りの頂点） */
static cptr comment_4a_B[MAX_COMMENT_4A] = {
	"なめやがって！温厚な俺様でも限界があるってことを知れ！",
	"俺をここまで怒らせて...命があるだけでもありがたいと思え！",
	"ふざけてるのか！冷やかしなら相手を見てからにしろ！",
	"いいかげんにしろ！今度こんなまねしたらただじゃおかねえぞ！"
};
#endif
#define MAX_COMMENT_4B	4

static cptr comment_4b[MAX_COMMENT_4B] =
{
#ifdef JP
	"店から出て行け！",
	"俺の前から消え失せろ！",
	"どっかに行っちまえ！",
	"出ろ、出ろ、出て行け！"
#else
	"Leave my store!",
	"Get out of my sight!",
	"Begone, you scoundrel!",
	"Out, out, out!"
#endif

};

#ifdef JP
/* ブラックマーケット用追加メッセージ（追い出し） */
static cptr comment_4b_B[MAX_COMMENT_4B] = {
	"二度とうちに来るんじゃねえ！！",
	"とっとと、どっかへ失せろ！！",
	"今すぐ消え失せろ！！",
	"出ていけ！出ていけ！！"
};
#endif
#define MAX_COMMENT_5	8

static cptr comment_5[MAX_COMMENT_5] =
{
#ifdef JP
	"考え直してくれ。",
	"そりゃおかしい！",
	"もっと真面目に言ってくれ！",
	"交渉する気があるのかい？",
	"冷やかしに来たのか！",
	"悪い冗談だ！",
	"我慢くらべかい。",
	"ふーむ、良い天気だ。"
#else
	"Try again.",
	"Ridiculous!",
	"You will have to do better than that!",
	"Do you wish to do business or not?",
	"You've got to be kidding!",
	"You'd better be kidding!",
	"You try my patience.",
	"Hmmm, nice weather we're having."
#endif

};

#ifdef JP
/* ブラックマーケット用追加メッセージ（怒り） */
static cptr comment_5_B[MAX_COMMENT_5] = {
	"時間の無駄だな、これは。",
	"厄介なお客様だな！",
	"話して分かる相手じゃなさそうだ。",
	"痛い目にあいたいらしいな！",
	"なんて強欲な奴だ！",
	"話にならん輩だ！",
	"どうしようもない貧乏人だ！",
	"喧嘩を売っているのか？"
};
#endif
#define MAX_COMMENT_6	4

static cptr comment_6[MAX_COMMENT_6] =
{
#ifdef JP
	"どうやら聞き間違えたらしい。",
	"失礼、よく聞こえなかったよ。",
	"すまない、何だって？",
	"悪い、もう一度言ってくれる？"
#else
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?"
#endif

};



/*
 * Successful haggle.
 */
static void say_comment_1(void)
{
	char rumour[1024];

#ifdef JP
	/* ブラックマーケットのときは別のメッセージを出す */
	if (cur_store_num == STORE_BLACK)
	{
		msg_print(comment_1_B[randint0(MAX_COMMENT_1)]);
	}
	else
	{
		msg_print(comment_1[randint0(MAX_COMMENT_1)]);
	}
#else
	msg_print(comment_1[randint0(MAX_COMMENT_1)]);
#endif


	if (one_in_(RUMOR_CHANCE))
	{
#ifdef JP
		msg_print("店主は耳うちした:");
#else
		msg_print("The shopkeeper whispers something into your ear:");
#endif


#ifdef JP
if (!get_rnd_line_jonly("rumors_j.txt", 0, rumour, 10))
#else
		if (!get_rnd_line("rumors.txt", 0, rumour))
#endif

			msg_print(rumour);
	}
}


/*
 * Continue haggling (player is buying)
 */
static void say_comment_2(s32b value, int annoyed)
{
	char	tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long)value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		msg_format(comment_2a[randint0(MAX_COMMENT_2A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
#ifdef JP
		/* ブラックマーケットの時は別のメッセージを出す */
		if ( cur_store_num == STORE_BLACK ){
			msg_format(comment_2b_B[randint0(MAX_COMMENT_2B)], tmp_val);
		}
		else{
		msg_format(comment_2b[randint0(MAX_COMMENT_2B)], tmp_val);
	}
#else
		msg_format(comment_2b[randint0(MAX_COMMENT_2B)], tmp_val);
#endif

	}
}


/*
 * Continue haggling (player is selling)
 */
static void say_comment_3(s32b value, int annoyed)
{
	char	tmp_val[80];

	/* Prepare a string to insert */
	sprintf(tmp_val, "%ld", (long)value);

	/* Final offer */
	if (annoyed > 0)
	{
		/* Formatted message */
		msg_format(comment_3a[randint0(MAX_COMMENT_3A)], tmp_val);
	}

	/* Normal offer */
	else
	{
		/* Formatted message */
#ifdef JP
		/* ブラックマーケットの時は別のメッセージを出す */
		if ( cur_store_num == STORE_BLACK ){
			msg_format(comment_3b_B[randint0(MAX_COMMENT_3B)], tmp_val);
		}
		else{
		msg_format(comment_3b[randint0(MAX_COMMENT_3B)], tmp_val);
	}
#else
		msg_format(comment_3b[randint0(MAX_COMMENT_3B)], tmp_val);
#endif

	}
}


/*
 * Kick 'da bum out.					-RAK-
 */
static void say_comment_4(void)
{
#ifdef JP
	/* ブラックマーケットの時は別のメッセージを出す */
	if (cur_store_num == STORE_BLACK)
	{
		msg_print(comment_4a_B[randint0(MAX_COMMENT_4A)]);
		msg_print(comment_4b_B[randint0(MAX_COMMENT_4B)]);
	}
	else
	{
		msg_print(comment_4a[randint0(MAX_COMMENT_4A)]);
		msg_print(comment_4b[randint0(MAX_COMMENT_4B)]);
	}
#else
	msg_print(comment_4a[randint0(MAX_COMMENT_4A)]);
	msg_print(comment_4b[randint0(MAX_COMMENT_4B)]);
#endif

}


/*
 * You are insulting me
 */
static void say_comment_5(void)
{
#ifdef JP
	/* ブラックマーケットの時は別のメッセージを出す */
	if (cur_store_num == STORE_BLACK)
	{
		msg_print(comment_5_B[randint0(MAX_COMMENT_5)]);
	}
	else
	{
		msg_print(comment_5[randint0(MAX_COMMENT_5)]);
	}
#else
	msg_print(comment_5[randint0(MAX_COMMENT_5)]);
#endif

}


/*
 * That makes no sense.
 */
static void say_comment_6(void)
{
	msg_print(comment_6[randint0(MAX_COMMENT_6)]);
}



/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A	4

static cptr comment_7a[MAX_COMMENT_7A] =
{
#ifdef JP
	"うわああぁぁ！",
	"なんてこった！",
	"誰かがむせび泣く声が聞こえる...。",
	"店主が悔しげにわめいている！"
#else
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!"
#endif

};

#define MAX_COMMENT_7B	4

static cptr comment_7b[MAX_COMMENT_7B] =
{
#ifdef JP
	"くそう！",
	"この悪魔め！",
	"店主が恨めしそうに見ている。",
	"店主が睨んでいる。"
#else
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
#endif

};

#define MAX_COMMENT_7C	4

static cptr comment_7c[MAX_COMMENT_7C] =
{
#ifdef JP
	"すばらしい！",
	"君が天使に見えるよ！",
	"店主がクスクス笑っている。",
	"店主が大声で笑っている。"
#else
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
#endif

};

#define MAX_COMMENT_7D	4

static cptr comment_7d[MAX_COMMENT_7D] =
{
#ifdef JP
	"やっほぅ！",
	"こんなおいしい思いをしたら、真面目に働けなくなるなぁ。",
	"店主は嬉しくて跳ね回っている。",
	"店主は満面に笑みをたたえている。"
#else
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully."
#endif

};


/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
	/* Item was worthless, but we bought it */
	if ((value <= 0) && (price > value))
	{
		/* Comment */
		msg_print(comment_7a[randint0(MAX_COMMENT_7A)]);

		change_your_alignment(ALI_GNE, -2);

		/* Sound */
		sound(SOUND_STORE1);
	}

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
	{
		/* Comment */
		msg_print(comment_7b[randint0(MAX_COMMENT_7B)]);

		change_your_alignment(ALI_GNE, -1);

		/* Sound */
		sound(SOUND_STORE2);
	}

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
	{
		/* Comment */
		msg_print(comment_7c[randint0(MAX_COMMENT_7C)]);

		change_your_alignment(ALI_GNE, 1);

		/* Sound */
		sound(SOUND_STORE3);
	}

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
	{
		/* Comment */
		msg_print(comment_7d[randint0(MAX_COMMENT_7D)]);

		change_your_alignment(ALI_GNE, 2);

		/* Sound */
		sound(SOUND_STORE4);
	}
}





#ifdef JP
/* 日本語版の場合は上の方に移動してあります */
#else
/*
 * We store the current "store number" here so everyone can access it
 */
static int cur_store_num = 7;

/*
 * We store the current "store page" here so everyone can access it
 */
static int store_top = 0;

/*
 * We store the current "store pointer" here so everyone can access it
 */
static store_type *st_ptr = NULL;

/*
 * We store the current "owner type" here so everyone can access it
 */
static owner_type *ot_ptr = NULL;
#endif







/*
 * Buying and selling adjustments for chaos frame.
 * Entry[abs(chaos frame / 10)] gives the basic "cost inflation".
 * cfgold_adj_p[] is for positive, cfgold_adj_n[] is for negative.
 */
static byte cfgold_adj_p[] =
{
	100, /*   0 -   9 */
	100, /*  10 -  19 */
	 99, /*  20 -  29 */
	 99, /*  30 -  39 */
	 98, /*  40 -  49 */
	 98, /*  50 -  59 */
	 97, /*  60 -  69 */
	 97, /*  70 -  79 */
	 96, /*  80 -  89 */
	 96, /*  90 -  99 */
	 95, /* 100 - 109 */
	 95, /* 110 - 119 */
	 94, /* 120 - 129 */
	 94, /* 130 - 139 */
	 93, /* 140 - 149 */
	 93, /* 150 - 159 */
	 92, /* 160 - 169 */
	 92, /* 170 - 179 */
	 91, /* 180 - 189 */
	 91, /* 190 - 199 */
	 90, /* 200 - 209 */
	 89, /* 210 - 219 */
	 88, /* 220 - 229 */
	 87, /* 230 - 239 */
	 86, /* 240 - 249 */
	 85, /* 250 - 259 */
	 84, /* 260 - 269 */
	 83, /* 270 - 279 */
	 82, /* 280 - 289 */
	 81, /* 290 - 299 */
	 80, /* 300 */
};

static byte cfgold_adj_n[] =
{
	100, /*    0 -   -9 */
	100, /*  -10 -  -19 */
	101, /*  -20 -  -29 */
	101, /*  -30 -  -39 */
	102, /*  -40 -  -49 */
	102, /*  -50 -  -59 */
	103, /*  -60 -  -69 */
	103, /*  -70 -  -79 */
	104, /*  -80 -  -89 */
	104, /*  -90 -  -99 */
	106, /* -100 - -109 */
	106, /* -110 - -119 */
	108, /* -120 - -129 */
	108, /* -130 - -139 */
	110, /* -140 - -149 */
	110, /* -150 - -159 */
	112, /* -160 - -169 */
	112, /* -170 - -179 */
	114, /* -180 - -189 */
	114, /* -190 - -199 */
	116, /* -200 - -209 */
	118, /* -210 - -219 */
	120, /* -220 - -229 */
	122, /* -230 - -239 */
	125, /* -240 - -249 */
	130, /* -250 - -259 */
	130, /* -260 - -269 */
	130, /* -270 - -279 */
	130, /* -280 - -289 */
	130, /* -290 - -299 */
	130, /* -300 */
};




/*
 * Determine the price of an item (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * item, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges twice as much as it should.
 *
 * Charisma adjustment runs from 80 to 130
 * Racial adjustment runs from 95 to 130
 *
 * Since greed/charisma/racial adjustments are centered at 100, we need
 * to adjust (by 200) to extract a usable multiplier.  Note that the
 * "greed" value is always something (?).
 */
static s32b price_item(object_type *o_ptr, int greed, bool flip)
{
	int 	factor = 100;
	int 	adjust;
	s32b	price;


	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return (0L);


	/* Compute the chaos frame factor */
	if (!dun_level && p_ptr->town_num)
	{
		int ethnic = town[p_ptr->town_num].ethnic;
		if (ethnic != NO_ETHNICITY)
		{
			int tmp_cf = chaos_frame[ethnic];
			if (tmp_cf >= 0) factor = cfgold_adj_p[tmp_cf / 10];
			else factor = cfgold_adj_n[(0 - tmp_cf) / 10];
		}
	}

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];


	/* Shop is buying */
	if (flip)
	{
		/* Adjust for greed */
		adjust = 100 + (300 - (greed + factor));

		/* Never get "silly" */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (cur_store_num == STORE_BLACK)
			price = price / 2;
	}

	/* Shop is selling */
	else
	{
		/* Adjust for greed */
		adjust = 100 + ((greed + factor) - 300);

		/* Never get "silly" */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (cur_store_num == STORE_BLACK)
			price = price * 2;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Note -- Never become "free" */
	if (price <= 0L) return (1L);

	/* Return the price */
	return (price);
}


/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
static void mass_produce(object_type *o_ptr)
{
	int size = 1;
	int discount = 0;

	s32b cost = object_value(o_ptr);


	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Food */
		case TV_FOOD:
		{
			if ((o_ptr->sval == SV_FOOD_EGG) || (o_ptr->sval == SV_FOOD_BIG_EGG) ||
			    (o_ptr->sval == SV_FOOD_SPECIAL_EGG) || (o_ptr->sval == SV_FOOD_ROTTEN_EGG))
			{
				size = 1;
				break;
			}
		}
		/* Fall through */
		/* Flasks, and Lites */
		case TV_FLASK:
		case TV_LITE:
		{
			if (cost <= 5L) size += damroll(3, 5);
			if (cost <= 20L) size += damroll(3, 5);
			if (cost <= 50L) size += damroll(2, 2);
			break;
		}

		case TV_POTION:
		case TV_SCROLL:
		{
			if (cost <= 60L) size += damroll(3, 5);
			if (cost <= 240L) size += damroll(1, 5);
			if (o_ptr->sval == SV_SCROLL_STAR_IDENTIFY) size += damroll(3, 5);
			if (o_ptr->sval == SV_SCROLL_STAR_REMOVE_CURSE) size += damroll(1, 4);
			break;
		}

		case TV_MAGERY_BOOK:
		case TV_FIRE_BOOK:
		case TV_AQUA_BOOK:
		case TV_EARTH_BOOK:
		case TV_WIND_BOOK:
		case TV_HOLY_BOOK:
		case TV_DEATH_BOOK:
		case TV_SYMBIOTIC_BOOK:
		case TV_WITCH_BOOK:
		case TV_DRAKONITE_BOOK:
		case TV_CRUSADE_BOOK:
		{
			if (cost <= 50L) size += damroll(2, 3);
			if (cost <= 500L) size += damroll(1, 3);
			break;
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_GLOVES:
		case TV_BOOTS:
		case TV_CLOAK:
		case TV_HELM:
		case TV_CROWN:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		case TV_BOW:
		{
			if (o_ptr->art_name) break;
			if (o_ptr->name2) break;
			if (cost <= 10L) size += damroll(3, 5);
			if (cost <= 100L) size += damroll(3, 5);
			break;
		}

		case TV_SPIKE:
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ROCKET:
		case TV_ARROW:
		case TV_BOLT:
		{
			if (cost <= 5L) size += damroll(5, 5);
			if (cost <= 50L) size += damroll(5, 5);
			if (cost <= 500L) size += damroll(5, 5);
			break;
		}

		case TV_FIGURINE:
		{
			if (cost <= 100L) size += damroll(2, 2);
			if (cost <= 1000L) size += damroll(2, 2);
			break;
		}

		case TV_STATUE:
		case TV_CARD:
		case TV_TRUMP:
		{
			size = 1;
			break;
		}

		/*
		 * Because many rods (and a few wands and staffs) are useful mainly
		 * in quantity, the Black Market will occasionally have a bunch of
		 * one kind. -LM-
		 */
		case TV_ROD:
		case TV_WAND:
		case TV_STAFF:
		{
			if ((cur_store_num == STORE_BLACK) && one_in_(3))
			{
				if (cost < 1601L) size += damroll(1, 5);
				else if (cost < 3201L) size += damroll(1, 3);
			}

			/* Ensure that mass-produced rods and wands get the correct pvals. */
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
			{
				o_ptr->pval *= size;
			}
			break;
		}
	}


	/* Pick a discount */
	if (cost < 5)
	{
		discount = 0;
	}
	else if (one_in_(25))
	{
		discount = 25;
	}
	else if (one_in_(150))
	{
		discount = 50;
	}
	else if (one_in_(300))
	{
		discount = 75;
	}
	else if (one_in_(500))
	{
		discount = 90;
	}


	if (o_ptr->tval == TV_FOOD)
	{
		switch (o_ptr->sval)
		{
		case SV_FOOD_EGG:
		case SV_FOOD_BIG_EGG:
		case SV_FOOD_SPECIAL_EGG:
		case SV_FOOD_ROTTEN_EGG:
			if (cheat_peek && discount)
			{
#ifdef JP
				msg_print("卵は値引きなし。");
#else
				msg_print("No discount on eggs.");
#endif
			}
			discount = 0;
			break;
		}
	}

	if (o_ptr->art_name)
	{
		if (cheat_peek && discount)
		{
#ifdef JP
			msg_print("ランダムアーティファクトは値引きなし。");
#else
			msg_print("No discount on random artifacts.");
#endif

		}
		discount = 0;
	}

	/* Save the discount */
	o_ptr->discount = discount;

	/* Save the total pile size */
	o_ptr->number = size - (size * discount / 100);
}



/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(object_type *o_ptr, object_type *j_ptr)
{
	int i;

	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return FALSE;

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return FALSE;

	/* Different charges (etc) cannot be stacked, unless wands or rods. */
	if ((o_ptr->pval != j_ptr->pval) && (o_ptr->tval != TV_WAND) && (o_ptr->tval != TV_ROD)) return FALSE;

	/* Require many identical values */
	for (i = 0; i < A_MAX; i++)
	{
		if (o_ptr->to_stat[i] != j_ptr->to_stat[i]) return FALSE;
	}
	for (i = 0; i < OB_MAX; i++)
	{
		if (o_ptr->to_misc[i] != j_ptr->to_misc[i]) return FALSE;
	}
	if (o_ptr->to_h != j_ptr->to_h) return FALSE;
	if (o_ptr->to_d != j_ptr->to_d) return FALSE;
	if (o_ptr->to_a != j_ptr->to_a) return FALSE;

	/* Require identical "artifact" names */
	if (o_ptr->name1 != j_ptr->name1) return FALSE;

	/* Require identical "ego-item" names */
	if (o_ptr->name2 != j_ptr->name2) return FALSE;

	/* Random artifacts don't stack !*/
	if (o_ptr->art_name || j_ptr->art_name) return FALSE;

	/* Hack -- Identical art_flags! */
	for (i = 0; i < TR_FLAG_SIZE; i++)
		if (o_ptr->art_flags[i] != j_ptr->art_flags[i]) return FALSE;

	/* Hack -- Never stack "powerful" items */
	if (o_ptr->xtra1 || j_ptr->xtra1) return FALSE;

	/* Require identical added essence  */
	if (o_ptr->xtra3 != j_ptr->xtra3) return FALSE;
	if (o_ptr->xtra4 != j_ptr->xtra4) return FALSE;

	/* Hack -- Never stack recharging items */
	if (o_ptr->timeout || j_ptr->timeout) return FALSE;

	/* Require many identical values */
	if (o_ptr->ac != j_ptr->ac)   return FALSE;
	if (o_ptr->dd != j_ptr->dd)   return FALSE;
	if (o_ptr->ds != j_ptr->ds)   return FALSE;

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST) return FALSE;
	if (o_ptr->tval == TV_STATUE) return FALSE;
	if (o_ptr->tval == TV_TRUMP) return FALSE;

	/* Hack -- Never stack eggs */
	if (o_ptr->tval == TV_FOOD)
	{
		switch (o_ptr->sval)
		{
		case SV_FOOD_EGG:
		case SV_FOOD_BIG_EGG:
		case SV_FOOD_SPECIAL_EGG:
		case SV_FOOD_ROTTEN_EGG:
			return FALSE;
		}
	}

	/* Require matching discounts */
	if (o_ptr->discount != j_ptr->discount) return FALSE;

	/* They match, so they must be similar */
	return TRUE;
}


/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int max_num = (o_ptr->tval == TV_ROD) ?
		MIN(99, MAX_SHORT / k_info[o_ptr->k_idx].pval) : 99;
	int total = o_ptr->number + j_ptr->number;
	int diff = (total > max_num) ? total - max_num : 0;

	/* Combine quantity, lose excess items */
	o_ptr->number = (total > max_num) ? max_num : total;

	/* Hack -- if rods are stacking, add the pvals (maximum timeouts) together. -LM- */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval * (j_ptr->number - diff) / j_ptr->number;
	}

	/* Hack -- if wands are stacking, combine the charges. -LM- */
	if (o_ptr->tval == TV_WAND)
	{
		o_ptr->pval += j_ptr->pval * (j_ptr->number - diff) / j_ptr->number;
	}
}


/*
 * Check to see if the shop will be carrying too many objects	-RAK-
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.	Before, one could "nuke" potions this way.
 *
 * Return value is now int:
 *  0 : No space
 * -1 : Can be combined to existing slot.
 *  1 : Cannot be combined but there are empty spaces.
 */
static int store_check_num(object_type *o_ptr)
{
	int 	   i;
	object_type *j_ptr;

	/* The "home" acts like the player */
	if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM))
	{
		bool old_stack_force_notes = stack_force_notes;
		bool old_stack_force_costs = stack_force_costs;

		if (cur_store_num != STORE_HOME)
		{
			stack_force_notes = FALSE;
			stack_force_costs = FALSE;
		}

		/* Check all the items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing item */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (object_similar(j_ptr, o_ptr))
			{
				if (cur_store_num != STORE_HOME)
				{
					stack_force_notes = old_stack_force_notes;
					stack_force_costs = old_stack_force_costs;
				}

				return -1;
			}
		}

		if (cur_store_num != STORE_HOME)
		{
			stack_force_notes = old_stack_force_notes;
			stack_force_costs = old_stack_force_costs;
		}
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing item */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (store_object_similar(j_ptr, o_ptr)) return -1;
		}
	}

	/* Free space is always usable */
	/*
	 * オプション powerup_home が設定されていると
	 * 我が家が 20 ページまで使える
	 */
	if ((cur_store_num == STORE_HOME) && ( powerup_home == FALSE )) {
		if (st_ptr->stock_num < ((st_ptr->stock_size) / 10)) {
			return 1;
		}
	}
	else{
		if (st_ptr->stock_num < st_ptr->stock_size) {
			return 1;
		}
	}

	/* But there was no room at the inn... */
	return 0;
}


static bool is_blessed(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];
	object_flags(o_ptr, flgs);
	if (object_is_known(o_ptr) && have_flag(flgs, TR_BLESSED)) return (TRUE);
	else return (FALSE);
}



/*
 * Determine if the current store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(object_type *o_ptr)
{
	/* Hack -- The Home is simple */
	if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM)) return (TRUE);

	/* Switch on the store */
	switch (cur_store_num)
	{
		/* General Store */
		case STORE_GENERAL:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_FOOD:
				case TV_LITE:
				case TV_FLASK:
				case TV_SPIKE:
				case TV_BULLET:
				case TV_ROUND:
				case TV_SHELL:
				case TV_ROCKET:
				case TV_ARROW:
				case TV_BOLT:
				case TV_DIGGING:
				case TV_CLOAK:
				case TV_BOTTLE: /* 'Green', recycling Angband */
				case TV_FIGURINE:
				case TV_STATUE:
				case TV_CARD:
				case TV_TRUMP:
				case TV_SCRATCH_CARD:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Armoury */
		case STORE_ARMOURY:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_CROWN:
				case TV_HELM:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Weapon Shop */
		case STORE_WEAPON:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BULLET:
				case TV_ROUND:
				case TV_SHELL:
				case TV_ROCKET:
				case TV_BOLT:
				case TV_ARROW:
				case TV_BOW:
				case TV_DIGGING:
				case TV_POLEARM:
				case TV_SWORD:
				break;
				case TV_HAFTED:
				{
					if(o_ptr->sval == SV_WIZSTAFF) return (FALSE);
				}
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Temple */
		case STORE_TEMPLE:
		{
			u32b flgs[TR_FLAG_SIZE];
			object_flags(o_ptr, flgs);

			/* Hack - Temple doesn't buy unholy items */
			if (object_is_known(o_ptr) && have_flag(flgs, TR_UNHOLY)) return FALSE;

			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SCROLL:
				case TV_POTION:
				{
					break;
				}
				case TV_HAFTED:
				{
					if (o_ptr->sval == SV_SINAI) return (FALSE);
					break;
				}
				case TV_FIGURINE:
				case TV_STATUE:
				{
					monster_race *r_ptr = &r_info[o_ptr->pval];

					/* Decline evil & chaotic */
					if (!(r_ptr->flags3 & RF3_EVIL) && !(r_ptr->flags7 & RF7_CHAOTIC))
					{
						/* Accept good */
						if (r_ptr->flags3 & RF3_GOOD) break;

						/* Accept animals */
						if (r_ptr->flags3 & RF3_ANIMAL) break;

						/* Accept mimics */
						if (my_strchr("?!", r_ptr->d_char)) break;
					}
				}
				case TV_POLEARM:
				case TV_SWORD:
				{
					if (is_blessed(o_ptr)) break;
				}
				default:
				return (FALSE);
			}
			break;
		}

		/* Alchemist */
		case STORE_ALCHEMIST:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_STONE:
				case TV_SCROLL:
				case TV_POTION:
				case TV_TAROT:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Magic Shop */
		case STORE_MAGIC:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_STONE:
				case TV_AMULET:
				case TV_RING:
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				case TV_SCROLL:
				case TV_POTION:
				case TV_FIGURINE:
				case TV_TAROT:
				case TV_TRUMP:
				break;
				case TV_HAFTED:
				{
					if(o_ptr->sval == SV_WIZSTAFF) break;
					else return (FALSE);
				}
				default:
				return (FALSE);
			}
			break;
		}
		/* Bookstore Shop */
		case STORE_BOOK:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_MAGERY_BOOK:
				case TV_FIRE_BOOK:
				case TV_AQUA_BOOK:
				case TV_EARTH_BOOK:
				case TV_WIND_BOOK:
				case TV_HOLY_BOOK:
				case TV_DEATH_BOOK:
				case TV_SYMBIOTIC_BOOK:
				case TV_WITCH_BOOK:
				case TV_DRAKONITE_BOOK:
				case TV_CRUSADE_BOOK:
					break;
				default:
					return (FALSE);
			}
			break;
		}
	}

	/* XXX XXX XXX Ignore "worthless" items */
	if (object_value(o_ptr) <= 0) return (FALSE);

	/* Assume okay */
	return (TRUE);
}



/*
 * Add the item "o_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
	int 				slot;
	s32b			   value, j_value;
	int 	i;
	object_type *j_ptr;
	bool old_stack_force_notes = stack_force_notes;
	bool old_stack_force_costs = stack_force_costs;

	if (cur_store_num != STORE_HOME)
	{
		stack_force_notes = FALSE;
		stack_force_costs = FALSE;
	}

	/* Check each existing item (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing item */
		j_ptr = &st_ptr->stock[slot];

		/* The home acts just like the player */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr);

			if (cur_store_num != STORE_HOME)
			{
				stack_force_notes = old_stack_force_notes;
				stack_force_costs = old_stack_force_costs;
			}

			/* All done */
			return (slot);
		}
	}

	if (cur_store_num != STORE_HOME)
	{
		stack_force_notes = old_stack_force_notes;
		stack_force_costs = old_stack_force_costs;
	}

	/* No space? */
	/*
	 * 隠し機能: オプション powerup_home が設定されていると
	 *           我が家が 20 ページまで使える
	 */
	/* No space? */
	if ( powerup_home == TRUE) {
		if (st_ptr->stock_num >= st_ptr->stock_size) {
			return (-1);
		}
	}
	else{
		if (st_ptr->stock_num >= ((st_ptr->stock_size) / 10)) {
			return (-1);
		}
	}


	/* Determine the "value" of the item */
	value = object_value(o_ptr);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that item */
		j_ptr = &st_ptr->stock[slot];

		/* Hack -- readable books always come first */
		if ((o_ptr->tval == mp_ptr->spell_book) &&
			(j_ptr->tval != mp_ptr->spell_book)) break;
		if ((j_ptr->tval == mp_ptr->spell_book) &&
			(o_ptr->tval != mp_ptr->spell_book)) continue;

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Can happen in the home */
		if (!object_is_aware(o_ptr)) continue;
		if (!object_is_aware(j_ptr)) break;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Objects in the home can be unknown */
		if (!object_is_known(o_ptr)) continue;
		if (!object_is_known(j_ptr)) break;

		/*
		 * Hack:  otherwise identical rods sort by
		 * increasing recharge time --dsb
		 */
		if (o_ptr->tval == TV_ROD)
		{
			if (o_ptr->pval < j_ptr->pval) break;
			if (o_ptr->pval > j_ptr->pval) continue;
		}
		if ((o_ptr->tval == TV_CORPSE) || (o_ptr->tval == TV_FIGURINE) || (o_ptr->tval == TV_STATUE))
		{
			if (r_info[o_ptr->pval].level < r_info[j_ptr->pval].level) break;
			if ((r_info[o_ptr->pval].level == r_info[j_ptr->pval].level) && (o_ptr->pval < j_ptr->pval)) break;
		}

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		st_ptr->stock[i] = st_ptr->stock[i-1];
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Insert the new item */
	st_ptr->stock[slot] = *o_ptr;

	/* Return the location */
	return (slot);
}


/*
 * Add the item "o_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.	Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(object_type *o_ptr)
{
	int 	i, slot;
	s32b	value, j_value;
	object_type *j_ptr;


	/* Evaluate the object */
	value = object_value(o_ptr);

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (-1);

	/* All store items are fully *identified* */
	o_ptr->ident |= IDENT_MENTAL;

	/* Erase the inscription */
	o_ptr->inscription = 0;

	/* Erase the "feeling" */
	o_ptr->feeling = FEEL_NONE;

	/* Check each existing item (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing item */
		j_ptr = &st_ptr->stock[slot];

		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Hack -- extra items disappear */
			store_object_absorb(j_ptr, o_ptr);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that item */
		j_ptr = &st_ptr->stock[slot];

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/*
		 * Hack:  otherwise identical rods sort by
		 * increasing recharge time --dsb
		 */
		if (o_ptr->tval == TV_ROD)
		{
			if (o_ptr->pval < j_ptr->pval) break;
			if (o_ptr->pval > j_ptr->pval) continue;
		}

		/* Evaluate that slot */
		j_value = object_value(j_ptr);

		/* Objects sort by decreasing value */
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		st_ptr->stock[i] = st_ptr->stock[i-1];
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Insert the new item */
	st_ptr->stock[slot] = *o_ptr;

	/* Return the location */
	return (slot);
}


/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.	This can result in zero items.
 */
static void store_item_increase(int item, int num)
{
	int 		cnt;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &st_ptr->stock[item];

	/* Verify the number */
	cnt = o_ptr->number + num;
	if (cnt > 255) cnt = 255;
	else if (cnt < 0) cnt = 0;
	num = cnt - o_ptr->number;

	/* Save the new number */
	o_ptr->number += num;
}


/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(int item)
{
	int 		j;
	object_type *o_ptr;

	/* Get the item */
	o_ptr = &st_ptr->stock[item];

	/* Must exist */
	if (!o_ptr->k_idx) return;

	/* Must have no items */
	if (o_ptr->number) return;

	/* One less item */
	st_ptr->stock_num--;

	/* Slide everyone */
	for (j = item; j < st_ptr->stock_num; j++)
	{
		st_ptr->stock[j] = st_ptr->stock[j + 1];
	}

	/* Nuke the final slot */
	object_wipe(&st_ptr->stock[j]);
}


/*
 * This function will keep 'crap' out of the black market.
 * Crap is defined as any item that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_crap(object_type *o_ptr)
{
	int 	i, j;

	/* Ego items are never crap */
	if (o_ptr->name2) return (FALSE);

	/* Good items are never crap */
	if (o_ptr->to_a > 0) return (FALSE);
	if (o_ptr->to_h > 0) return (FALSE);
	if (o_ptr->to_d > 0) return (FALSE);

	/* Check all stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		if (i == STORE_HOME) continue;
		if (i == STORE_MUSEUM) continue;

		/* Check every item in the store */
		for (j = 0; j < town[p_ptr->town_num].store[i].stock_num; j++)
		{
			object_type *j_ptr = &town[p_ptr->town_num].store[i].stock[j];

			/* Duplicate item "type", assume crappy */
			if (o_ptr->k_idx == j_ptr->k_idx) return (TRUE);
		}
	}

	/* Assume okay */
	return (FALSE);
}


/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete(void)
{
	int what, num;

	/* Pick a random slot */
	what = randint0(st_ptr->stock_num);

	/* Determine how many items are here */
	num = st_ptr->stock[what].number;

	/* Hack -- sometimes, only destroy half the items */
	if (randint0(100) < 50) num = (num + 1) / 2;

	/* Hack -- sometimes, only destroy a single item */
	if (randint0(100) < 50) num = 1;

	/* Hack -- decrement the maximum timeouts and total charges of rods and wands. -LM- */
	if ((st_ptr->stock[what].tval == TV_ROD) || (st_ptr->stock[what].tval == TV_WAND))
	{
		st_ptr->stock[what].pval -= num * st_ptr->stock[what].pval / st_ptr->stock[what].number;
	}

	/* Actually destroy (part of) the item */
	store_item_increase(what, -num);
	store_item_optimize(what);
}


/*
 * Creates a random item and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 * Currently, "normal" stores use a pre-built array.
 *
 * Note -- the "level" given to "obj_get_num()" is a "favored"
 * level, that is, there is a much higher chance of getting
 * items with a level approaching that of the given level...
 *
 * Should we check for "permission" to have the given item?
 */
static void store_create(void)
{
	int i, tries, level;

	object_type forge;
	object_type *q_ptr;


	/* Paranoia -- no room left */
	if (st_ptr->stock_num >= st_ptr->stock_size) return;


	/* Hack -- consider up to four items */
	for (tries = 0; tries < 4; tries++)
	{
		/* Black Market */
		if ((cur_store_num == STORE_BLACK) ||
			((p_ptr->town_num == TOWN_BARMAMUTHA) && (quest[QUEST_BARMAMUTHA_C].status == QUEST_STATUS_FINISHED) && (cur_store_num != STORE_BOOK)))
		{
			/* Pick a level for object/magic */
			level = 25 + randint0(25) + dun_level;

			/* Random item (usually of given level) */
			i = get_obj_num(level);

			/* Handle failure */
			if (!i) continue;
		}

		/* Normal Store */
		else
		{
			/* Hack -- Pick an item to sell */
			i = st_ptr->table[randint0(st_ptr->table_num)];

			/* Hack -- fake level for apply_magic() */
			level = rand_range(1, STORE_OBJ_LEVEL);
		}


		/* Get local object */
		q_ptr = &forge;

		/* Create a new object of the chosen kind */
		object_prep(q_ptr, i);

		/* Apply some "low-level" magic (no artifacts) */
		apply_magic(q_ptr, level, (p_ptr->town_num != NO_TOWN) ? 0L : AMF_GREAT);

		/* Require valid object */
		if (!store_will_buy(q_ptr)) continue;

		/* Hack -- Charge lite's */
		if (q_ptr->tval == TV_LITE)
		{
			if (q_ptr->sval == SV_LITE_TORCH) q_ptr->xtra4 = FUEL_TORCH / 2;
			if (q_ptr->sval == SV_LITE_LANTERN) q_ptr->xtra4 = FUEL_LAMP / 2;
		}


		/* The item is "known" */
		object_known(q_ptr);

		/* Mark it storebought */
		q_ptr->ident |= IDENT_STORE;

		/* Mega-Hack -- no chests in stores */
		if (q_ptr->tval == TV_CHEST) continue;

		/* Prune the black market */
		if (cur_store_num == STORE_BLACK)
		{
			/* Hack -- No "crappy" items */
			if (black_market_crap(q_ptr)) continue;

			/* Hack -- No "cheap" items */
			if (object_value(q_ptr) < 10) continue;

			/* No "worthless" items */
			/* if (object_value(q_ptr) <= 0) continue; */
		}

		/* Prune normal stores */
		else
		{
			/* No "worthless" items */
			if (object_value(q_ptr) <= 0) continue;
		}


		/* Mass produce and/or Apply discount */
		mass_produce(q_ptr);

		/* Attempt to carry the (known) item */
		(void)store_carry(q_ptr);

		/* Definitely done */
		break;
	}
}



/*
 * Eliminate need to bargain if player has haggled well in the past
 */
static bool noneedtobargain(s32b minprice)
{
	s32b good = st_ptr->good_buy;
	s32b bad = st_ptr->bad_buy;

	/* Cheap items are "boring" */
	if (minprice < 10L) return (TRUE);

	/* Perfect haggling */
	if (good == MAX_SHORT) return (TRUE);

	/* Reward good haggles, punish bad haggles, notice price */
	if (good > ((3 * bad) + (5 + (minprice/50)))) return (TRUE);

	/* Return the flag */
	return (FALSE);
}


/*
 * Update the bargain info
 */
static void updatebargain(s32b price, s32b minprice, int num)
{
	/* Hack -- auto-haggle */
	if (!manual_haggle) return;

	/* Cheap items are "boring" */
	if ((minprice/num) < 10L) return;

	/* Count the successful haggles */
	if (price == minprice)
	{
		/* Just count the good haggles */
		if (st_ptr->good_buy < MAX_SHORT)
		{
			st_ptr->good_buy++;
		}
	}

	/* Count the failed haggles */
	else
	{
		/* Just count the bad haggles */
		if (st_ptr->bad_buy < MAX_SHORT)
		{
			st_ptr->bad_buy++;
		}
	}
}



/*
 * Re-displays a single store entry
 */
static void display_entry(int pos)
{
	int 		i, cur_col;
	object_type 	*o_ptr;
	s32b		x;

	char		o_name[MAX_NLEN];
	char		out_val[160];


	int maxwid = 75;

	/* Get the item */
	o_ptr = &st_ptr->stock[pos];

	/* Get the "offset" */
	i = (pos % 12);

	/* Label it, clear the line --(-- */
	(void)sprintf(out_val, "%c) ", I2A(i));
	prt(out_val, i+6, 0);

	cur_col = 3;
	if (show_item_graph)
	{
		byte a = object_attr(o_ptr);
		char c = object_char(o_ptr);

#ifdef AMIGA
		if (a & 0x80)
			a |= 0x40;
#endif

		Term_draw(cur_col, i + 6, a, c);
		if (use_bigtile)
		{
			cur_col++;
			if (a & 0x80)
				Term_draw(cur_col, i + 6, 255, -1);
		}
		cur_col += 2;
	}

	/* Describe an item in the home */
	if ((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM))
	{
		maxwid = 75;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 10;

		/* Describe the object */
		object_desc(o_name, o_ptr, 0);
		o_name[maxwid] = '\0';
		c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, cur_col);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
#ifdef JP
			sprintf(out_val, "%3d.%1d kg", lbtokg1(wgt) , lbtokg2(wgt) );
			put_str(out_val, i+6, 67);
#else
			(void)sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(out_val, i+6, 68);
#endif

		}
	}

	/* Describe an item (fully) in a store */
	else
	{
		/* Must leave room for the "price" */
		maxwid = 65;

		/* Leave room for weights, if necessary -DRS- */
		if (show_weights) maxwid -= 7;

		/* Describe the object (fully) */
		object_desc(o_name, o_ptr, 0);
		o_name[maxwid] = '\0';
		c_put_str(tval_to_attr[o_ptr->tval], o_name, i+6, cur_col);

		/* Show weights */
		if (show_weights)
		{
			/* Only show the weight of an individual item */
			int wgt = o_ptr->weight;
#ifdef JP
			sprintf(out_val, "%3d.%1d", lbtokg1(wgt) , lbtokg2(wgt) );
			put_str(out_val, i+6, 60);
#else
			(void)sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
			put_str(out_val, i+6, 61);
#endif

		}

		/* Display a "fixed" cost */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

			/* Actually draw the price (not fixed) */
#ifdef JP
			(void)sprintf(out_val, "%9ld固", (long)x);
#else
			(void)sprintf(out_val, "%9ld F", (long)x);
#endif

			put_str(out_val, i+6, 68);
		}

		/* Display a "taxed" cost */
		else if (!manual_haggle)
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

			/* Hack -- Apply Sales Tax if needed */
			if (!noneedtobargain(x)) x += x / 10;

			/* Actually draw the price (with tax) */
			(void)sprintf(out_val, "%9ld  ", (long)x);
			put_str(out_val, i+6, 68);
		}

		/* Display a "haggle" cost */
		else
		{
			/* Extrect the "maximum" price */
			x = price_item(o_ptr, ot_ptr->max_inflate, FALSE);

			/* Actually draw the price (not fixed) */
			(void)sprintf(out_val, "%9ld  ", (long)x);
			put_str(out_val, i+6, 68);
		}
	}
}


/*
 * Displays a store's inventory 		-RAK-
 * All prices are listed as "per individual object".  -BEN-
 */
static void display_inventory(void)
{
	int i, k;

	/* Display the next 12 items */
	for (k = 0; k < 12; k++)
	{
		/* Do not display "dead" items */
		if (store_top + k >= st_ptr->stock_num) break;

		/* Display that line */
		display_entry(store_top + k);
	}

	/* Erase the extra lines and the "more" prompt */
	for (i = k; i < 13; i++) prt("", i + 6, 0);

	/* Assume "no current page" */
#ifdef JP
	put_str("          ", 5, 20);
#else
	put_str("        ", 5, 20);
#endif


	/* Visual reminder of "more items" */
	if (st_ptr->stock_num > 12)
	{
		/* Show "more" reminder (after the last item) */
#ifdef JP
		prt("-続く-", k + 6, 3);
#else
		prt("-more-", k + 6, 3);
#endif


		/* Indicate the "current page" */
		/* Trailing spaces are to display (Page xx) and (Page x) */
#ifdef JP
		put_str(format("(%dページ)  ", store_top/12 + 1), 5, 20);
#else
		put_str(format("(Page %d)  ", store_top/12 + 1), 5, 20);
#endif

	}
}


/*
 * Displays players gold					-RAK-
 */
static void store_prt_gold(void)
{
	char out_val[64];

#ifdef JP
	prt("手持ちのお金: ", 19, 53);
#else
	prt("Gold Remaining: ", 19, 53);
#endif


	sprintf(out_val, "%9ld", (long)p_ptr->au_sum);
	prt(out_val, 19, 68);
}


static s32b max_cost_fix(void)
{
	int factor = 100;

	/* Compute the chaos frame factor */
	if (!dun_level && p_ptr->town_num)
	{
		int ethnic = town[p_ptr->town_num].ethnic;
		if (ethnic != NO_ETHNICITY)
		{
			int tmp_cf = chaos_frame[ethnic];
			if (tmp_cf >= 0) factor = cfgold_adj_p[tmp_cf / 10];
			else factor = cfgold_adj_n[(0 - tmp_cf) / 10];
		}
	}

	/* Add in the charisma factor */
	factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];

	return (300 - factor) * ot_ptr->max_cost / 100;
}

/*
 * Displays store (after clearing screen)		-RAK-
 */
static void display_store(void)
{
	char buf[80];


	/* Clear screen */
	Term_clear();

	/* The "Home" is special */
	if (cur_store_num == STORE_HOME)
	{
		/* Put the owner name */
#ifdef JP
		put_str("我が家", 3, 31);
#else
		put_str("Your Home", 3, 30);
#endif


		/* Label the item descriptions */
#ifdef JP
		put_str("アイテムの一覧", 5, 4);
#else
		put_str("Item Description", 5, 3);
#endif


		/* If showing weights, show label */
		if (show_weights)
		{
#ifdef JP
			put_str("重さ", 5, 72);
#else
			put_str("Weight", 5, 70);
#endif

		}
	}

	/* The "Home" is special */
	else if (cur_store_num == STORE_MUSEUM)
	{
		/* Put the owner name */
#ifdef JP
		put_str("博物館", 3, 31);
#else
		put_str("Museum", 3, 30);
#endif


		/* Label the item descriptions */
#ifdef JP
		put_str("アイテムの一覧", 5, 4);
#else
		put_str("Item Description", 5, 3);
#endif


		/* If showing weights, show label */
		if (show_weights)
		{
#ifdef JP
			put_str("重さ", 5, 72);
#else
			put_str("Weight", 5, 70);
#endif

		}
	}

	/* Normal stores */
	else
	{
		cptr store_name = f_name + f_info[FEAT_SHOP_HEAD + cur_store_num].name;
		cptr owner_name = ot_ptr->owner_name;
		cptr race_name = p_name + race_info[ot_ptr->owner_race].name;

		if (p_ptr->town_num == NO_TOWN)
		{
			store_name = f_name + f_info[FEAT_DENEB_SHOP].name;
#ifdef JP
			owner_name = "魔女デネブ";
#else
			owner_name = "Deneb, the Witch";
#endif
			race_name = p_name + race_info[RACE_HUMAN].name;
		}

		/* Put the owner name and race */
		sprintf(buf, "%s (%s)", owner_name, race_name);
		put_str(buf, 3, 10);

		/* Show the max price in the store (above prices) */
		sprintf(buf, "%s (%ld)", store_name, max_cost_fix());
		prt(buf, 3, 50);

		/* Label the item descriptions */
#ifdef JP
		put_str("商品の一覧", 5, 7);
#else
		put_str("Item Description", 5, 3);
#endif


		/* If showing weights, show label */
		if (show_weights)
		{
#ifdef JP
			put_str("重さ", 5, 62);
#else
			put_str("Weight", 5, 60);
#endif

		}

		/* Label the asking price (in stores) */
#ifdef JP
		put_str("価格", 5, 73);
#else
		put_str("Price", 5, 72);
#endif

	}

	/* Display the current gold */
	store_prt_gold();

	/* Draw in the inventory */
	display_inventory();
}



/*
 * Get the ID of a store item and return its value	-RAK-
 */
static int get_stock(int *com_val, cptr pmt, int i, int j)
{
	char	command;

	char	out_val[160];

	/* Get the item index */
	if (repeat_pull(com_val))
	{
		/* Verify the item */
		if ((*com_val >= i) && (*com_val <= j))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
#ifdef JP
	(void)sprintf(out_val, "(%s:%c-%c, ESCで中断) %s",
	        (((cur_store_num == STORE_HOME) || (cur_store_num == STORE_MUSEUM)) ? "アイテム" : "商品"), 
				  I2A(i), I2A(j), pmt);
#else
	(void)sprintf(out_val, "(Items %c-%c, ESC to exit) %s",
				  I2A(i), I2A(j), pmt);
#endif


	/* Ask until done */
	while (TRUE)
	{
		int k;

		/* Escape */
		if (!get_com(out_val, &command, FALSE)) break;

		/* Convert */
		k = (islower(command) ? A2I(command) : -1);

		/* Legal responses */
		if ((k >= i) && (k <= j))
		{
			*com_val = k;
			break;
		}

		/* Oops */
		bell();
	}

	/* Clear the prompt */
	prt("", 0, 0);

	/* Cancel */
	if (command == ESCAPE) return (FALSE);

	repeat_push(*com_val);

	/* Success */
	return (TRUE);
}


/*
 * Increase the insult counter and get angry if too many -RAK-
 */
static int increase_insults(void)
{
	/* Increase insults */
	st_ptr->insult_cur++;

	/* Become insulted */
	if (st_ptr->insult_cur > ot_ptr->insult_max)
	{
		/* Complain */
		say_comment_4();

		/* Reset insults */
		st_ptr->insult_cur = 0;
		st_ptr->good_buy = 0;
		st_ptr->bad_buy = 0;

		/* Open tomorrow */
		st_ptr->store_open = turn + TURNS_PER_TICK*TOWN_DAWN/8 + randint1(TURNS_PER_TICK*TOWN_DAWN/8);

		/* Closed */
		return (TRUE);
	}

	/* Not closed */
	return (FALSE);
}


/*
 * Decrease insults 				-RAK-
 */
static void decrease_insults(void)
{
	/* Decrease insults */
	if (st_ptr->insult_cur) st_ptr->insult_cur--;
}


/*
 * Have insulted while haggling 			-RAK-
 */
static int haggle_insults(void)
{
	/* Increase insults */
	if (increase_insults()) return (TRUE);

	/* Display and flush insult */
	say_comment_5();

	/* Still okay */
	return (FALSE);
}


/*
 * Mega-Hack -- Enable "increments"
 */
static bool allow_inc = FALSE;

/*
 * Mega-Hack -- Last "increment" during haggling
 */
static s32b last_inc = 0L;


/*
 * Get a haggle
 */
static int get_haggle(cptr pmt, s32b *poffer, s32b price, int final)
{
	s32b		i;

	cptr		p;

	char				buf[128];
	char		out_val[160];


	/* Clear old increment if necessary */
	if (!allow_inc) last_inc = 0L;


	/* Final offer */
	if (final)
	{
#ifdef JP
		sprintf(buf, "%s [承諾] ", pmt);
#else
		sprintf(buf, "%s [accept] ", pmt);
#endif

	}

	/* Old (negative) increment, and not final */
	else if (last_inc < 0)
	{
#ifdef JP
		sprintf(buf, "%s [-$%ld] ", pmt, (long)(ABS(last_inc)));
#else
		sprintf(buf, "%s [-%ld] ", pmt, (long)(ABS(last_inc)));
#endif

	}

	/* Old (positive) increment, and not final */
	else if (last_inc > 0)
	{
#ifdef JP
		sprintf(buf, "%s [+$%ld] ", pmt, (long)(ABS(last_inc)));
#else
		sprintf(buf, "%s [+%ld] ", pmt, (long)(ABS(last_inc)));
#endif

	}

	/* Normal haggle */
	else
	{
		sprintf(buf, "%s ", pmt);
	}


	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Ask until done */
	while (TRUE)
	{
		/* Default */
		strcpy(out_val, "");

		/* Ask the user for a response */
		if (!get_string(buf, out_val, 32)) return (FALSE);

		/* Skip leading spaces */
		for (p = out_val; *p == ' '; p++) /* loop */;

		/* Empty response */
		if (*p == '\0')
		{
			/* Accept current price */
			if (final)
			{
				*poffer = price;
				last_inc = 0L;
				break;
			}

			/* Use previous increment */
			if (allow_inc && last_inc)
			{
				*poffer += last_inc;
				break;
			}
		}

		/* Normal response */
		else
		{
			/* Extract a number */
			i = atol(p);

			/* Handle "incremental" number */
			if ((*p == '+' || *p == '-'))
			{
				/* Allow increments */
				if (allow_inc)
				{
					/* Use the given "increment" */
					*poffer += i;
					last_inc = i;
					break;
				}
			}

			/* Handle normal number */
			else
			{
				/* Use the given "number" */
				*poffer = i;
				last_inc = 0L;
				break;
			}
		}

		/* Warning */
#ifdef JP
		msg_print("値がおかしいです。");
#else
		msg_print("Invalid response.");
#endif

		msg_print(NULL);
	}

	/* Success */
	return (TRUE);
}


/*
 * Receive an offer (from the player)
 *
 * Return TRUE if offer is NOT okay
 */
static bool receive_offer(cptr pmt, s32b *poffer,
                          s32b last_offer, int factor,
                          s32b price, int final)
{
	/* Haggle till done */
	while (TRUE)
	{
		/* Get a haggle (or cancel) */
		if (!get_haggle(pmt, poffer, price, final)) return (TRUE);

		/* Acceptable offer */
		if (((*poffer) * factor) >= (last_offer * factor)) break;

		/* Insult, and check for kicked out */
		if (haggle_insults()) return (TRUE);

		/* Reject offer (correctly) */
		(*poffer) = last_offer;
	}

	/* Success */
	return (FALSE);
}


/*
 * Haggling routine 				-RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool purchase_haggle(object_type *o_ptr, s32b *price)
{
	s32b			   cur_ask, final_ask;
	s32b			   last_offer, offer;
	s32b			   x1, x2, x3;
	s32b			   min_per, max_per;
	int 			   flag, loop_flag, noneed;
	int 			   annoyed = 0, final = FALSE;

	bool		cancel = FALSE;

#ifdef JP
	cptr pmt = "提示価格";
#else
	cptr		pmt = "Asking";
#endif


	char		out_val[160];


	*price = 0;


	/* Extract the starting offer and the final offer */
	cur_ask = price_item(o_ptr, ot_ptr->max_inflate, FALSE);
	final_ask = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

	/* Determine if haggling is necessary */
	noneed = noneedtobargain(final_ask);

	/* No need to haggle */
	if (noneed || !manual_haggle)
	{
		/* No need to haggle */
		if (noneed)
		{
			/* Message summary */
#ifdef JP
			msg_print("結局この金額にまとまった。");
#else
			msg_print("You eventually agree upon the price.");
#endif

			msg_print(NULL);
		}

		/* No haggle option */
		else
		{
			/* Message summary */
#ifdef JP
			msg_print("すんなりとこの金額にまとまった。");
#else
			msg_print("You quickly agree upon the price.");
#endif

			msg_print(NULL);

			/* Apply Sales Tax */
			final_ask += final_ask / 10;
		}

		/* Final price */
		cur_ask = final_ask;

		/* Go to final offer */
#ifdef JP
		pmt = "最終提示価格";
#else
		pmt = "Final Offer";
#endif

		final = TRUE;
	}


	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;


	/* Haggle parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	/* Mega-Hack -- artificial "last offer" value */
	last_offer = object_value(o_ptr) * o_ptr->number;
	last_offer = last_offer * (200 - (int)(ot_ptr->max_inflate)) / 100L;
	if (last_offer <= 0) last_offer = 1;

	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle until done */
	for (flag = FALSE; !flag; )
	{
		loop_flag = TRUE;

		while (!flag && loop_flag)
		{
			(void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
			put_str(out_val, 1, 0);
#ifdef JP
			cancel = receive_offer("提示する金額? ",
#else
			cancel = receive_offer("What do you offer? ",
#endif

			                       &offer, last_offer, 1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer > cur_ask)
			{
				say_comment_6();
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}
		}

		if (!flag)
		{
			x1 = 100 * (offer - last_offer) / (cur_ask - last_offer);
			if (x1 < min_per)
			{
				if (haggle_insults())
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per) x1 = max_per;
			}
			x2 = rand_range(x1-2, x1+2);
			x3 = ((cur_ask - offer) * x2 / 100L) + 1;
			/* don't let the price go up */
			if (x3 < 0) x3 = 0;
			cur_ask -= x3;

			/* Too little */
			if (cur_ask < final_ask)
			{
				final = TRUE;
				cur_ask = final_ask;
#ifdef JP
				pmt = "最終提示価格";
#else
				pmt = "Final Offer";
#endif

				annoyed++;
				if (annoyed > 3)
				{
					(void)(increase_insults());
					cancel = TRUE;
					flag = TRUE;
				}
			}
			else if (offer >= cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				prt("", 1, 0);
#ifdef JP
				(void)sprintf(out_val, "前回の提示金額: $%ld",
#else
				(void)sprintf(out_val, "Your last offer: %ld",
#endif

							  (long)last_offer);
				put_str(out_val, 1, 39);
				say_comment_2(cur_ask, annoyed);
			}
		}
	}

	/* Cancel */
	if (cancel) return (TRUE);

	/* Update bargaining info */
	updatebargain(*price, final_ask, o_ptr->number);

	/* Do not cancel */
	return (FALSE);
}


/*
 * Haggling routine 				-RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool sell_haggle(object_type *o_ptr, s32b *price)
{
	s32b    purse, cur_ask, final_ask;
	s32b    last_offer = 0, offer = 0;
	s32b    x1, x2, x3;
	s32b    min_per, max_per;
	int     flag, loop_flag, noneed;
	int     annoyed = 0, final = FALSE;
	bool    cancel = FALSE;
#ifdef JP
	cptr pmt = "提示金額";
#else
	cptr    pmt = "Offer";
#endif

	char    out_val[160];


	*price = 0;


	/* Obtain the starting offer and the final offer */
	cur_ask = price_item(o_ptr, ot_ptr->max_inflate, TRUE);
	final_ask = price_item(o_ptr, ot_ptr->min_inflate, TRUE);

	/* Determine if haggling is necessary */
	noneed = noneedtobargain(final_ask);

	/* Get the owner's payout limit */
	purse = max_cost_fix();

	/* No need to haggle */
	if (noneed || !manual_haggle || (final_ask >= purse))
	{
		/* Apply Sales Tax (if needed) */
		if (!manual_haggle && !noneed)
		{
			final_ask -= final_ask / 10;
		}

		/* No reason to haggle */
		if (final_ask >= purse)
		{
			/* Message */
#ifdef JP
			msg_print("即座にこの金額にまとまった。");
#else
			msg_print("You instantly agree upon the price.");
#endif

			msg_print(NULL);

			/* Offer full purse */
			final_ask = purse;
		}

		/* No need to haggle */
		else if (noneed)
		{
			/* Message */
#ifdef JP
			msg_print("結局この金額にまとまった。");
#else
			msg_print("You eventually agree upon the price.");
#endif

			msg_print(NULL);
		}

		/* No haggle option */
		else
		{
			/* Message summary */
#ifdef JP
			msg_print("すんなりとこの金額にまとまった。");
#else
			msg_print("You quickly agree upon the price.");
#endif

			msg_print(NULL);
		}

		/* Final price */
		cur_ask = final_ask;

		/* Final offer */
		final = TRUE;
#ifdef JP
		pmt = "最終提示金額";
#else
		pmt = "Final Offer";
#endif

	}

	/* Haggle for the whole pile */
	cur_ask *= o_ptr->number;
	final_ask *= o_ptr->number;


	/* XXX XXX XXX Display commands */

	/* Haggling parameters */
	min_per = ot_ptr->haggle_per;
	max_per = min_per * 3;

	/* Mega-Hack -- artificial "last offer" value */
	last_offer = object_value(o_ptr) * o_ptr->number;
	last_offer = last_offer * ot_ptr->max_inflate / 100L;

	/* No offer yet */
	offer = 0;

	/* No incremental haggling yet */
	allow_inc = FALSE;

	/* Haggle */
	for (flag = FALSE; !flag; )
	{
		while (1)
		{
			loop_flag = TRUE;

			(void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
			put_str(out_val, 1, 0);
#ifdef JP
			cancel = receive_offer("提示する価格? ",
#else
			cancel = receive_offer("What price do you ask? ",
#endif

								   &offer, last_offer, -1, cur_ask, final);

			if (cancel)
			{
				flag = TRUE;
			}
			else if (offer < cur_ask)
			{
				say_comment_6();
				/* rejected, reset offer for incremental haggling */
				offer = last_offer;
			}
			else if (offer == cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}
			else
			{
				loop_flag = FALSE;
			}

			/* Stop */
			if (flag || !loop_flag) break;
		}

		if (!flag)
		{
			x1 = 100 * (last_offer - offer) / (last_offer - cur_ask);
			if (x1 < min_per)
			{
				if (haggle_insults())
				{
					flag = TRUE;
					cancel = TRUE;
				}
			}
			else if (x1 > max_per)
			{
				x1 = x1 * 3 / 4;
				if (x1 < max_per) x1 = max_per;
			}
			x2 = rand_range(x1-2, x1+2);
			x3 = ((offer - cur_ask) * x2 / 100L) + 1;
			/* don't let the price go down */
			if (x3 < 0) x3 = 0;
			cur_ask += x3;

			if (cur_ask > final_ask)
			{
				cur_ask = final_ask;
				final = TRUE;
#ifdef JP
				pmt = "最終提示金額";
#else
				pmt = "Final Offer";
#endif

				annoyed++;
				if (annoyed > 3)
				{
					flag = TRUE;
#ifdef JP
				/* 追加 $0 で買い取られてしまうのを防止 By FIRST*/
					cancel = TRUE;
#endif
					(void)(increase_insults());
				}
			}
			else if (offer <= cur_ask)
			{
				flag = TRUE;
				*price = offer;
			}

			if (!flag)
			{
				last_offer = offer;
				allow_inc = TRUE;
				prt("", 1, 0);
				(void)sprintf(out_val,
#ifdef JP
				              "前回の提示価格 $%ld", (long)last_offer);
#else
							  "Your last bid %ld", (long)last_offer);
#endif

				put_str(out_val, 1, 39);
				say_comment_3(cur_ask, annoyed);
			}
		}
	}

	/* Cancel */
	if (cancel) return (TRUE);

	/* Update bargaining info */
	updatebargain(*price, final_ask, o_ptr->number);

	/* Do not cancel */
	return (FALSE);
}


/*
 * Buy an item from a store 			-RAK-
 */
static void store_purchase(void)
{
	int i, amt, choice;
	int item, item_new;

	s32b price, best;

	object_type forge;
	object_type *j_ptr;

	object_type *o_ptr;

	char o_name[MAX_NLEN];

	char out_val[160];


	if (cur_store_num == STORE_MUSEUM)
	{
#ifdef JP
		msg_print("博物館から取り出すことはできません。");
#else
		msg_print("Museum.");
#endif
		return;
	}

	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (cur_store_num == STORE_HOME)
#ifdef JP
			msg_print("我が家には何も置いてありません。");
#else
			msg_print("Your home is empty.");
#endif

		else
#ifdef JP
			msg_print("現在商品の在庫を切らしています。");
#else
			msg_print("I am currently out of stock.");
#endif

		return;
	}


	/* Find the number of objects on this and following pages */
	i = (st_ptr->stock_num - store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
#ifdef JP
	/* ブラックマーケットの時は別のメッセージ */
	switch (cur_store_num)
	{
		case 7:
			sprintf(out_val, "どのアイテムを取りますか? ");
			break;
		case 6:
			sprintf(out_val, "どれ? ");
			break;
		default:
			sprintf(out_val, "どの品物が欲しいんだい? ");
			break;
	}
#else
	if (cur_store_num == STORE_HOME)
	{
		sprintf(out_val, "Which item do you want to take? ");
	}
	else
	{
		sprintf(out_val, "Which item are you interested in? ");
	}
#endif


	/* Get the item number to be bought */
	if (!get_stock(&item, out_val, 0, i - 1)) return;

	/* Get the actual index */
	item = item + store_top;

	/* Get the actual item */
	o_ptr = &st_ptr->stock[item];

	/* Assume the player wants just one of them */
	amt = 1;

	/* Get local object */
	j_ptr = &forge;

	/* Get a copy of the object */
	object_copy(j_ptr, o_ptr);

	/*
	 * If a rod or wand, allocate total maximum timeouts or charges
	 * between those purchased and left on the shelf.
	 */
	reduce_charges(j_ptr, o_ptr->number - amt);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
#ifdef JP
		msg_print("そんなにアイテムを持てない。");
#else
		msg_print("You cannot carry that many different items.");
#endif

		return;
	}

	/* Determine the "best" price (per item) */
	best = price_item(j_ptr, ot_ptr->min_inflate, FALSE);

	/* Find out how many the player wants */
	if (o_ptr->number > 1)
	{
		/* Hack -- note cost of "fixed" items */
		if ((cur_store_num != STORE_HOME) &&
		    (o_ptr->ident & IDENT_FIXED))
		{
#ifdef JP
			msg_format("一つにつき $%ldです。", (long)(best));
#else
			msg_format("That costs %ld gold per item.", (long)(best));
#endif

		}

		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Get local object */
	j_ptr = &forge;

	/* Get desired object */
	object_copy(j_ptr, o_ptr);

	/*
	 * If a rod or wand, allocate total maximum timeouts or charges
	 * between those purchased and left on the shelf.
	 */
	reduce_charges(j_ptr, o_ptr->number - amt);

	/* Modify quantity */
	j_ptr->number = amt;

	/* Hack -- require room in pack */
	if (!inven_carry_okay(j_ptr))
	{
#ifdef JP
		msg_print("ザックにそのアイテムを入れる隙間がない。");
#else
		msg_print("You cannot carry that many items.");
#endif

		return;
	}

	/* Attempt to buy it */
	if (cur_store_num != STORE_HOME)
	{
		/* Fixed price, quick buy */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Assume accept */
			choice = 0;

			/* Go directly to the "best" deal */
			price = (best * j_ptr->number);
		}

		/* Haggle for it */
		else
		{
			/* Describe the object (fully) */
			object_desc(o_name, j_ptr, 0);

			/* Message */
#ifdef JP
			msg_format("%s(%c)を購入する。", o_name, I2A(item));
#else
			msg_format("Buying %s (%c).", o_name, I2A(item));
#endif

			msg_print(NULL);

			/* Haggle for a final price */
			choice = purchase_haggle(j_ptr, &price);

			/* Hack -- Got kicked out */
			if (st_ptr->store_open >= turn) return;
		}

		/* Player wants it */
		if (choice == 0)
		{
			/* Fix the item price (if "correctly" haggled) */
			if (price == (best * j_ptr->number)) o_ptr->ident |= (IDENT_FIXED);

			/* Player can afford it */
			if (p_ptr->au_sum >= price)
			{
				int idx;

				/* Say "okay" */
				say_comment_1();

				/* Make a sound */
				sound(SOUND_BUY);

				/* Be happy */
				decrease_insults();

				/* Spend the money */
				p_ptr->au_sum -= price;
				p_ptr->update |= (PU_GOLD);
				update_stuff();

				/* Update the display */
				store_prt_gold();

				/* Hack -- buying an item makes you aware of it */
				object_aware(j_ptr);

				/* Hack -- clear the "fixed" flag from the item */
				j_ptr->ident &= ~(IDENT_FIXED);

				/* Describe the transaction */
				object_desc(o_name, j_ptr, 0);

				/* Message */
#ifdef JP
				msg_format("%sを $%ldで購入しました。", o_name, (long)price);
#else
				msg_format("You bought %s for %ld gold.", o_name, (long)price);
#endif

				if (object_value_real(j_ptr) >= 500)
				{
					/* The black market is illegal! */
					if (cur_store_num == STORE_BLACK)
						change_your_alignment(ALI_GNE, -1);

					/* Chaos frame change */
					if (!dun_level && p_ptr->town_num && one_in_(5))
					{
						change_chaos_frame(town[p_ptr->town_num].ethnic, 1);

						/* Show the max price in the store (above prices) */
						prt(format("%s (%ld)", f_name + f_info[FEAT_SHOP_HEAD + cur_store_num].name, max_cost_fix()), 3, 50);
					}
				}

				strcpy(record_o_name, o_name);
				record_turn = turn;

				if (record_buy) do_cmd_write_nikki(NIKKI_BUY, 0, o_name);
				object_desc(o_name, o_ptr, OD_NAME_ONLY);
				if(record_rand_art && o_ptr->art_name)
					do_cmd_write_nikki(NIKKI_ART, 0, o_name);

				/* Erase the inscription */
				j_ptr->inscription = 0;

				/* Erase the "feeling" */
				j_ptr->feeling = FEEL_NONE;
				j_ptr->ident &= ~(IDENT_STORE);
				/* Give it to the player */
				item_new = inven_carry(j_ptr);

				/* Describe the final result */
				object_desc(o_name, &inventory[item_new], 0);

				/* Message */
#ifdef JP
		msg_format("%s(%c)を手に入れた。", o_name, index_to_label(item_new));
#else
				msg_format("You have %s (%c).",
						   o_name, index_to_label(item_new));
#endif

				/* Auto-inscription */
				idx = is_autopick(&inventory[item_new]);
				auto_inscribe_item(item_new, idx);

				/* Now, reduce the original stack's pval. */
				if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
				{
					o_ptr->pval -= j_ptr->pval;
				}

				/* Handle stuff */
				handle_stuff();

				/* Note how many slots the store used to have */
				i = st_ptr->stock_num;

				/* Remove the bought items from the store */
				store_item_increase(item, -amt);
				store_item_optimize(item);

				/* Store is empty */
				if (st_ptr->stock_num == 0)
				{
					/* Shuffle */
					if ((one_in_(STORE_SHUFFLE)) && (p_ptr->town_num != NO_TOWN))
					{
						char buf[80];
						/* Message */
#ifdef JP
						msg_print("店主は引退した。");
#else
						msg_print("The shopkeeper retires.");
#endif


						/* Shuffle the store */
						store_shuffle(cur_store_num);

						prt("",3,0);
						sprintf(buf, "%s (%s)",
							ot_ptr->owner_name, p_name + race_info[ot_ptr->owner_race].name);
						put_str(buf, 3, 10);
						sprintf(buf, "%s (%ld)",
							f_name + f_info[FEAT_SHOP_HEAD + cur_store_num].name, max_cost_fix());
						prt(buf, 3, 50);
					}

					/* Maintain */
					else
					{
						/* Message */
#ifdef JP
						msg_print("店主は新たな在庫を取り出した。");
#else
						msg_print("The shopkeeper brings out some new stock.");
#endif

					}

					/* New inventory */
					for (i = 0; i < 10; i++)
					{
						/* Maintain the store */
						store_maint(p_ptr->town_num, cur_store_num);
					}

					/* Start over */
					store_top = 0;

					/* Redraw everything */
					display_inventory();
				}

				/* The item is gone */
				else if (st_ptr->stock_num != i)
				{
					/* Pick the correct screen */
					if (store_top >= st_ptr->stock_num) store_top -= 12;

					/* Redraw everything */
					display_inventory();
				}

				/* Item is still here */
				else
				{
					/* Redraw the item */
					display_entry(item);
				}
			}

			/* Player cannot afford it */
			else
			{
				/* Simple message (no insult) */
#ifdef JP
				msg_print("お金が足りません。");
#else
				msg_print("You do not have enough gold.");
#endif

			}
		}
	}

	/* Home is much easier */
	else
	{
		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, j_ptr, amt);

		/* Give it to the player */
		item_new = inven_carry(j_ptr);

		/* Describe just the result */
		object_desc(o_name, &inventory[item_new], 0);

		/* Message */
#ifdef JP
				msg_format("%s(%c)を取った。",
#else
		msg_format("You have %s (%c).",
#endif
 o_name, index_to_label(item_new));

		/* Handle stuff */
		handle_stuff();

		/* Take note if we take the last one */
		i = st_ptr->stock_num;

		/* Remove the items from the home */
		store_item_increase(item, -amt);
		store_item_optimize(item);

		/* Hack -- Item is still here */
		if (i == st_ptr->stock_num)
		{
			/* Redraw the item */
			display_entry(item);
		}

		/* The item is gone */
		else
		{
			/* Nothing left */
			if (st_ptr->stock_num == 0) store_top = 0;

			/* Nothing left on that screen */
			else if (store_top >= st_ptr->stock_num) store_top -= 12;

			/* Redraw everything */
			display_inventory();
		}
	}

	/* Not kicked out */
	return;
}


static int get_object_ethnicity_store(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_STATUE:
		if (o_ptr->sval == SV_PHOTO)
		{
			monster_race *r_ptr = &r_info[o_ptr->pval];

			if (!(r_ptr->flags1 & RF1_UNIQUE)) return NO_ETHNICITY;
			if (r_ptr->level < 50) return NO_ETHNICITY;
		}
		break;

	case TV_CORPSE:
		return NO_ETHNICITY;
	}

	return get_object_ethnicity(o_ptr);
}


/*
 * Sell an item to the store (or home)
 */
static void store_sell(void)
{
	int choice;
	int item, item_pos;
	int amt;

	s32b price, value, dummy;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	cptr q, s;

	char o_name[MAX_NLEN];


	/* Prepare a prompt */
	if (cur_store_num == STORE_HOME)
#ifdef JP
	q = "どのアイテムを置きますか? ";
#else
		q = "Drop which item? ";
#endif

	else if (cur_store_num == STORE_MUSEUM)
#ifdef JP
	q = "どのアイテムを寄贈しますか? ";
#else
		q = "Give which item? ";
#endif

	else
#ifdef JP
		q = "どのアイテムを売りますか? ";
#else
		q = "Sell which item? ";
#endif


	item_tester_no_ryoute = TRUE;
	/* Only allow items the store will buy */
	item_tester_hook = store_will_buy;

	/* Get an item */
	/* 我が家でおかしなメッセージが出るオリジナルのバグを修正 */
	if (cur_store_num == STORE_HOME)
	{
#ifdef JP
		s = "置けるアイテムを持っていません。";
#else
		s = "You don't have any item to drop.";
#endif
	}
	else if (cur_store_num == STORE_MUSEUM)
	{
#ifdef JP
		s = "寄贈できるアイテムを持っていません。";
#else
		s = "You don't have any item to give.";
#endif
	}
	else
	{
#ifdef JP
		s = "欲しい物がないですねえ。";
#else
		s = "You have nothing that I want.";
#endif
	}

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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


	if (dungeon_type == DUNGEON_HEAVEN)
	{
		if (object_is_known(o_ptr) && (o_ptr->name1 == ART_BRUNHILD))
		{
			msg_print("天界から帰還するにはブリュンヒルドが必要です。");
			return;
		}
	}


	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_RARM) && object_is_cursed(o_ptr))
	{
		/* Oops */
#ifdef JP
		msg_print("ふーむ、どうやらそれは呪われているようだね。");
#else
		msg_print("Hmmm, it seems to be cursed.");
#endif


		/* Nope */
		return;
	}


	/* Assume one item */
	amt = 1;

	/* Find out how many the player wants (letter means "all") */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Get local object */
	q_ptr = &forge;

	/* Get a copy of the object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = amt;

	/*
	 * Hack -- If a rod or wand, allocate total maximum
	 * timeouts or charges to those being sold. -LM-
	 */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
	{
		q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
	}

	/* Get a full description */
	object_desc(o_name, q_ptr, 0);

	/* Reset magical weapon effect */
	if (item == INVEN_RARM)
	{
		if (mw_old_weight)
		{
			q_ptr->weight = mw_old_weight;
		}
		if (mw_diff_to_melee)
		{
			q_ptr->to_h -= mw_diff_to_melee;
			q_ptr->to_d -= mw_diff_to_melee;
		}
	}

	/* Remove any inscription, feeling for stores */
	if ((cur_store_num != STORE_HOME) && (cur_store_num != STORE_MUSEUM))
	{
		q_ptr->inscription = 0;
		q_ptr->feeling = FEEL_NONE;
	}

	/* Is there room in the store (or the home?) */
	if (!store_check_num(q_ptr))
	{
		if (cur_store_num == STORE_HOME)
#ifdef JP
			msg_print("我が家にはもう置く場所がない。");
#else
			msg_print("Your home is full.");
#endif

		else if (cur_store_num == STORE_MUSEUM)
#ifdef JP
			msg_print("博物館はもう満杯だ。");
#else
			msg_print("Museum is full.");
#endif

		else
#ifdef JP
			msg_print("すいませんが、店にはもう置く場所がありません。");
#else
			msg_print("I have not the room in my store to keep it.");
#endif

		return;
	}


	/* Real store */
	if ((cur_store_num != STORE_HOME) && (cur_store_num != STORE_MUSEUM))
	{
		/* Describe the transaction */
#ifdef JP
		msg_format("%s(%c)を売却する。", o_name, index_to_label(item));
#else
		msg_format("Selling %s (%c).", o_name, index_to_label(item));
#endif

		msg_print(NULL);

		/* Haggle for it */
		choice = sell_haggle(q_ptr, &price);

		/* Kicked out */
		if (st_ptr->store_open >= turn) return;

		/* Sold... */
		if (choice == 0)
		{
			/* Say "okay" */
			say_comment_1();

			/* Make a sound */
			sound(SOUND_SELL);

			/* Be happy */
			decrease_insults();

			/* Get some money */
			p_ptr->au[SV_GOLD_NOTE] += price;
			p_ptr->update |= (PU_GOLD);
			update_stuff();

			/* Update the display */
			store_prt_gold();

			/* Get the "apparent" value */
			dummy = object_value(q_ptr) * q_ptr->number;

			/* Identify it */
			identify_item(o_ptr);

			/* Get local object */
			q_ptr = &forge;

			/* Get a copy of the object */
			object_copy(q_ptr, o_ptr);

			/* Reset magical weapon effect */
			if (item == INVEN_RARM)
			{
				if (mw_old_weight)
				{
					q_ptr->weight = mw_old_weight;
				}
				if (mw_diff_to_melee)
				{
					q_ptr->to_h -= mw_diff_to_melee;
					q_ptr->to_d -= mw_diff_to_melee;
				}
			}

			/* Modify quantity */
			q_ptr->number = amt;

			/* Make it look like known */
			q_ptr->ident |= IDENT_STORE;

			/*
			 * Hack -- If a rod or wand, let the shopkeeper know just
			 * how many charges he really paid for. -LM-
			 */
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
			{
				q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
			}

			/* Get the "actual" value */
			value = object_value(q_ptr) * q_ptr->number;

			/* Get the description all over again */
			object_desc(o_name, q_ptr, 0);

			/* Describe the result (in message buffer) */
#ifdef JP
			msg_format("%sを $%ldで売却しました。", o_name, (long)price);
#else
			msg_format("You sold %s for %ld gold.", o_name, (long)price);
#endif

			/* The black market is illegal! */
			if ((object_value_real(q_ptr) > 500) && (cur_store_num == STORE_BLACK))
				change_your_alignment(ALI_GNE, -1);

			/* Chaos frame change */
			if (!dun_level && p_ptr->town_num)
			{
				if ((object_value_real(q_ptr) > 500) && one_in_(5)) change_chaos_frame(town[p_ptr->town_num].ethnic, 1);
				if (object_is_cursed(q_ptr)) change_chaos_frame(town[p_ptr->town_num].ethnic, -1);
				if (object_value_real(q_ptr) > max_cost_fix()) change_chaos_frame(town[p_ptr->town_num].ethnic, 1);
			}
			change_chaos_frame(get_object_ethnicity_store(q_ptr), -4);

			/* Show the max price in the store (above prices) */
			if (p_ptr->town_num == NO_TOWN) prt(format("%s (%ld)", f_name + f_info[FEAT_DENEB_SHOP].name, max_cost_fix()), 3, 50);
			else prt(format("%s (%ld)", f_name + f_info[FEAT_SHOP_HEAD + cur_store_num].name, max_cost_fix()), 3, 50);

			if (record_sell) do_cmd_write_nikki(NIKKI_SELL, 0, o_name);

			if (!((o_ptr->tval == TV_FIGURINE) && (value > 0)))
			{
			 /* Analyze the prices (and comment verbally) unless a figurine*/
			purchase_analyze(price, value, dummy);
			}

			/*
			 * Hack -- Allocate charges between those wands or rods sold
			 * and retained, unless all are being sold. -LM-
			 */
			distribute_charges(o_ptr, q_ptr, amt);

			/* Reset timeouts of the sold items */
			q_ptr->timeout = 0;

			/* Take the item from the player, describe the result */
			inven_item_increase(item, -amt);
			inven_item_describe(item);
			inven_item_optimize(item);

			if (item == INVEN_RARM)
			{
				set_magical_weapon(0, 0, item, TRUE);
				set_evil_weapon(0, TRUE, item, FALSE);
			}

			/* Handle stuff */
			handle_stuff();

			/* The store gets that (known) item */
			item_pos = store_carry(q_ptr);

			/* Re-display if item is now in store */
			if (item_pos >= 0)
			{
				store_top = (item_pos / 12) * 12;
				display_inventory();
			}
		}
	}

	/* Player is at museum */
	else if (cur_store_num == STORE_MUSEUM)
	{
		char o2_name[MAX_NLEN];
		object_desc(o2_name, q_ptr, OD_NAME_ONLY);

		if (-1 == store_check_num(q_ptr))
		{
#ifdef JP
			msg_print("それと同じ品物は既に博物館にあるようです。");
#else
			msg_print("The same object as it is already in the Museum.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("博物館に寄贈したものは取り出すことができません！！");
#else
			msg_print("You cannot take items which is given to the Museum back!!");
#endif
		}
#ifdef JP
		if (!get_check(format("本当に%sを寄贈しますか？", o2_name))) return;
#else
		if (!get_check(format("Really give %s to the Museum? ", o2_name))) return;
#endif

		/* Identify it */
		identify_item(q_ptr);
		q_ptr->ident |= IDENT_MENTAL;

		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, q_ptr, amt);

		/* Describe */
#ifdef JP
		msg_format("%sを置いた。(%c)", o_name, index_to_label(item));
#else
		msg_format("You drop %s (%c).", o_name, index_to_label(item));
#endif

		/* Chaos frame change */
		change_chaos_frame(get_object_ethnicity_store(q_ptr), 3);

		choice = 0;

		/* Take it from the players inventory */
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);

		if (item == INVEN_RARM)
		{
			set_magical_weapon(0, 0, item, TRUE);
			set_evil_weapon(0, TRUE, item, FALSE);
		}

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = home_carry(q_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			store_top = (item_pos / 12) * 12;
			display_inventory();
		}
	}
	/* Player is at home */
	else
	{
		/* Distribute charges of wands/rods */
		distribute_charges(o_ptr, q_ptr, amt);

		/* Describe */
#ifdef JP
		msg_format("%sを置いた。(%c)", o_name, index_to_label(item));
#else
		msg_format("You drop %s (%c).", o_name, index_to_label(item));
#endif

		choice = 0;

		/* Take it from the players inventory */
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);

		if (item == INVEN_RARM)
		{
			set_magical_weapon(0, 0, item, TRUE);
			set_evil_weapon(0, TRUE, item, FALSE);
		}

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		item_pos = home_carry(q_ptr);

		/* Update store display */
		if (item_pos >= 0)
		{
			store_top = (item_pos / 12) * 12;
			display_inventory();
		}
	}
	if ((choice == 0) && ((item == INVEN_RARM) || (item == INVEN_LARM))) kamaenaoshi(item);
}


/*
 * Examine an item in a store			   -JDL-
 */
static void store_examine(void)
{
	int         i;
	int         item;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];
	char        out_val[160];


	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
		if (cur_store_num == STORE_HOME)
#ifdef JP
			msg_print("我が家には何も置いてありません。");
#else
			msg_print("Your home is empty.");
#endif

		else if (cur_store_num == STORE_MUSEUM)
#ifdef JP
			msg_print("博物館には何も置いてありません。");
#else
			msg_print("Museum is empty.");
#endif

		else
#ifdef JP
			msg_print("現在商品の在庫を切らしています。");
#else
			msg_print("I am currently out of stock.");
#endif

		return;
	}


	/* Find the number of objects on this and following pages */
	i = (st_ptr->stock_num - store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
#ifdef JP
	sprintf(out_val, "どれを調べますか？");
#else
	sprintf(out_val, "Which item do you want to examine? ");
#endif


	/* Get the item number to be examined */
	if (!get_stock(&item, out_val, 0, i - 1)) return;

	/* Get the actual index */
	item = item + store_top;

	/* Get the actual item */
	o_ptr = &st_ptr->stock[item];

	/* Require full knowledge */
	if (!(o_ptr->ident & IDENT_MENTAL))
	{
		/* This can only happen in the home */
#ifdef JP
		msg_print("このアイテムについて特に知っていることはない。");
#else
		msg_print("You have no special knowledge about that item.");
#endif

		return;
	}

	/* Description */
	object_desc(o_name, o_ptr, 0);

	/* Describe */
#ifdef JP
	msg_format("%sを調べている...", o_name);
#else
	msg_format("Examining %s...", o_name);
#endif


	/* Describe it fully */
	if (!screen_object(o_ptr, NULL, TRUE))
#ifdef JP
		msg_print("特に変わったところはないようだ。");
#else
		msg_print("You see nothing special.");
#endif


	return;
}


/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;


/* Locations of the tables on the screen */
#define HEADER_ROW		1
#define INSTRUCT_ROW	3
#define QUESTION_ROW	7
#define TABLE_ROW		10

#define QUESTION_COL	2
#define CLASS_COL		2
#define CLASS_AUX_COL   22

#define CLASS_WID		20


typedef struct cc_menu cc_menu;

struct cc_menu
{
	int  real;
	bool can_choose;
};

static void choose_realm(void)
{
	if (p_ptr->realm_medium) return;

	p_ptr->realm_medium |= (CH_FIRE | CH_AQUA | CH_EARTH | CH_WIND);

	if ((p_ptr->cexp_info[CLASS_PRIEST].clev > 19) || (p_ptr->cexp_info[CLASS_CLERIC].clev > 34))
		p_ptr->realm_medium |= (CH_HOLY);

	if (p_ptr->cexp_info[CLASS_PRIEST].clev > 29)
		p_ptr->realm_medium |= (CH_CRUSADE);

	return;
}

#define MAX_MENUS			25

/*
 * Change the class of player
 */
static void change_player_class(void)
{
	cc_menu        classes[MAX_MENUS];
	int            i, hgt;
	int            num = 0, top = 0, cur = 0, total_max_clev = 0, experienced_classes = 0;
	char           c;
	char           buf[80];
	bool           done = FALSE;
	byte           attr;
	char           s[80], stat_buf[8];
	player_class   *tmp_cp_ptr;
	byte           old_pclass = p_ptr->pclass;
	cexp_info_type *cexp_ptr;

	/*** Instructions ***/

	/* Calculate character total class level */
	for (i = 0; i < max_c_idx; i++)
	{
		if (p_ptr->cexp_info[i].max_clev > 0)
		{
			total_max_clev += p_ptr->cexp_info[i].max_clev;
			experienced_classes++;
		}
	}

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
#ifdef JP
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "以下のメニューから新しいクラスを選んでください。");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW, -1, TERM_WHITE,
	            "移動キーで項目をスクロールさせ、Enterで決定、ESCで取り消します。");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 0, INSTRUCT_ROW, -1, TERM_L_GREEN, "移動キー");
	Term_putstr(QUESTION_COL + 32, INSTRUCT_ROW, -1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 45, INSTRUCT_ROW, -1, TERM_L_GREEN, "ESC");
#else
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "Please select new class from the menu below:");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW, -1, TERM_WHITE,
	            "Use the movement keys to scroll the menu, Enter to select the current");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 1, -1, TERM_WHITE,
	            "menu item, 'ESC' to cancel.");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 8, INSTRUCT_ROW, -1, TERM_L_GREEN, "movement keys");
	Term_putstr(QUESTION_COL + 42, INSTRUCT_ROW, -1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 1, -1, TERM_L_GREEN, "ESC");
#endif

	if (!astral_mode)
#ifdef JP
		Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
			"性別と能力値とアラインメントによって選べる《クラス》が制限されています。");
#else
		Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
			"'Class' choice is restricted by sex, stats, and alignment.");
#endif
	else
#ifdef JP
		Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
			"性別によって選べる《クラス》が制限されています。");
#else
		Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
			"'Class' choice is restricted by sex.");
#endif

	/* Tabulate classes */
	for (i = 0; i < max_c_idx; i++)
	{
		tmp_cp_ptr = &class_info[i];

		/* Same class cannot be chosen */
		if (i == p_ptr->pclass) continue;

		/* Gender restriction */
		if ((p_ptr->psex == SEX_MALE) && !(tmp_cp_ptr->c_flags & PCF_SEX_MALE)) continue;
		if ((p_ptr->psex == SEX_FEMALE) && !(tmp_cp_ptr->c_flags & PCF_SEX_FEMALE)) continue;

		/* Reincarnation classes are must not be chosen now */
		if (tmp_cp_ptr->c_flags & PCF_REINCARNATE) continue;
		if (tmp_cp_ptr->c_flags & PCF_ALIEN) continue;
		if ((tmp_cp_ptr->c_flags & PCF_SECRET) && !(can_choose_class(i, CLASS_CHOOSE_MODE_NORMAL))) continue;

		classes[num].real = i;
		classes[num++].can_choose = can_choose_class(i, CLASS_CHOOSE_MODE_NORMAL);
	}

	if (!num || (!astral_mode && (cp_ptr->c_flags & PCF_NO_CHANGE)) ||
		(astral_mode && (cp_ptr->c_flags & PCF_REINCARNATE) && (cp_ptr->c_flags & PCF_NO_CHANGE)))
	{
#ifdef JP
		strcpy(buf, "現在のあなたが選べるクラスはありません。");
#else
		strcpy(buf, "You cannot choose any class.");
#endif
		Term_putstr(QUESTION_COL, TABLE_ROW, -1, TERM_L_RED, buf);
		message_add(buf);
#ifdef JP
		Term_putstr(QUESTION_COL, TABLE_ROW + 2, -1, TERM_WHITE,
			"[ 何かキーを押してください ]");
#else
		Term_putstr(QUESTION_COL, TABLE_ROW + 2, -1, TERM_WHITE,
			"[Press any key to continue]");
#endif
		inkey();

		/* Window stuff */
		p_ptr->window |= (PW_MESSAGE);

		return;
	}

	/* Choose */
	while (TRUE)
	{
		hgt = Term->hgt - TABLE_ROW - 1;

		/* Redraw the list */
		for (i = 0; ((i + top < num) && (i <= hgt)); i++)
		{
			tmp_cp_ptr = &class_info[classes[i + top].real];
			if (i + top < 26)
			{
				sprintf(buf, "%c) %s", I2A(i + top), c_name + tmp_cp_ptr->name);
			}
			else
			{
				/* ToDo: Fix the ASCII dependency */
				sprintf(buf, "%c) %s", 'A' + (i + top - 26), c_name + tmp_cp_ptr->name);
			}

			/* Clear */
			Term_erase(CLASS_COL, i + TABLE_ROW, CLASS_WID);

			/* Display */
			/* Highlight the current selection */
			if (i == (cur - top)) attr = classes[i + top].can_choose ? TERM_L_BLUE : TERM_BLUE;
			else attr = classes[i + top].can_choose ? TERM_WHITE : TERM_SLATE;

			Term_putstr(CLASS_COL, i + TABLE_ROW, CLASS_WID, attr, buf);
		}

		tmp_cp_ptr = &class_info[classes[cur].real];

		/* Display relevant details. */
		for (i = 0; i < A_MAX; i++)
		{
			if (tmp_cp_ptr->c_need[i]) cnv_stat(tmp_cp_ptr->c_need[i], stat_buf);
			else strcpy(stat_buf, "      ");
			sprintf(s, "%s%s", stat_names[i], stat_buf);
			Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
		}

#ifdef JP
		prt("", TABLE_ROW + A_MAX + 2, CLASS_AUX_COL);
		strcpy(s, "アラインメント:");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_LAWFUL) strcat(s, " 秩序");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_NEUTRAL) strcat(s, " 中立");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_CHAOTIC) strcat(s, " 混沌");
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
		switch (classes[cur].real)
		{
		case CLASS_HIGHWITCH:
			strcpy(s, "★魔女デネブのとんがり帽子が必要        ");
			break;
		case CLASS_LORD:
		case CLASS_GENERAL:
		case CLASS_NINJAMASTER:
		case CLASS_ARCHMAGE:
		case CLASS_FREYA:
		case CLASS_CRESCENT:
			strcpy(s, "????                                    ");
			break;
		default:
			strcpy(s, "                                        ");
			break;
		}
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);

		if (classes[cur].can_choose)
		{
			strcpy(s, "                              ");
			Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 5, -1, TERM_WHITE, s);
		}
		else
		{
			strcpy(s, "現在はこのクラスを選べません。");
			Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 5, -1, TERM_L_RED, s);
		}
#else
		strcpy(s, "                                 ");
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
		strcpy(s, "Alignment:");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_LAWFUL) strcat(s, " Lawful");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_NEUTRAL) strcat(s, " Neutral");
		if (tmp_cp_ptr->c_flags & PCF_ALIGN_CHAOTIC) strcat(s, " Chaotic");
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
		switch (classes[cur].real)
		{
		case CLASS_LORD:
		case CLASS_GENERAL:
		case CLASS_NINJAMASTER:
		case CLASS_ARCHMAGE:
		case CLASS_FREYA:
		case CLASS_CRESCENT:
			strcpy(s, "????                                              ");
			break;
		default:
			strcpy(s, "                                                  ");
			break;
		}
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);

		if (classes[cur].can_choose)
		{
			strcpy(s, "                                 ");
			Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 5, -1, TERM_WHITE, s);
		}
		else
		{
			strcpy(s, "You cannot choose this class now.");
			Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 5, -1, TERM_L_RED, s);
		}
#endif

		if (done)
		{
			char temp[80 * 9];
			cptr t;

			clear_from(TABLE_ROW);
			Term_putstr(CLASS_COL, TABLE_ROW, -1, TERM_L_BLUE, c_name + tmp_cp_ptr->name);

			roff_to_buf(c_text + class_info[classes[cur].real].text, 74, temp, sizeof temp);
			t = temp;

			for (i = 0; i < 9; i++)
			{
				if (t[0] == 0)
					break; 
				else
				{
					prt(t, TABLE_ROW + 2 + i, CLASS_COL);
					t += strlen(t) + 1;
				}
			}

#ifdef JP
			if (get_check("よろしいですか？"))
#else
			if (get_check("Are you sure? "))
#endif
				break;

			clear_from(TABLE_ROW);
			done = FALSE;
			continue;
		}

		/* Move the cursor */
		put_str("", TABLE_ROW + cur - top, CLASS_COL);

		c = inkey();

		switch (c)
		{
		case ESCAPE:
			return;

		case '\n':
		case '\r':
		case ' ':
			if (classes[cur].can_choose) done = TRUE;
			break;

		/* Going up? */
		case '8':
			if (cur != 0)
			{
				/* Move selection */
				cur--;
			}

			if ((top > 0) && ((cur - top) < 4))
			{
				/* Scroll up */
				top--;
			}
			break;

		/* Going down? */
		case '2':
			if (cur != (num - 1))
			{
				/* Move selection */
				cur++;
			}

			if ((top + hgt < (num - 1)) && ((top + hgt - cur) < 4))
			{
				/* Scroll down */
				top++;
			}
			break;

		default:
			if (isalpha(c))
			{
				int choice;

				if (islower(c))
				{
					choice = A2I(c);
				}
				else
				{
					choice = c - 'A' + 26;
				}

				/* Validate input */
				if ((choice > -1) && (choice < num))
				{
					cur = choice;

					/* Move it onto the screen */
					if ((cur < top) || (cur > top + hgt))
					{
						top = cur;
					}

					/* Done */
					if (classes[cur].can_choose) done = TRUE;
				}
				else
				{
					bell();
				}
			}

			/* Invalid input */
			else bell();
			break;
		}
	}

	/* Set class */
	p_ptr->pclass = classes[cur].real;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &m_info[p_ptr->pclass];
	p_ptr->s_ptr = &s_info[p_ptr->pclass];
	cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
	
	for (i = 0; i < MAX_REALM+1; i++)
		if (p_ptr->magic_exp[i] == 0) p_ptr->magic_exp[i] = p_ptr->s_ptr->s_eff[i];

	/* Clear */
	clear_from(TABLE_ROW);

#ifdef JP
	sprintf(buf, "%sから%sへとクラスチェンジしました。",
#else
	sprintf(buf, "Your class has changed from %s to %s.",
#endif
		c_name + class_info[old_pclass].name, c_name + cp_ptr->name);
	Term_putstr(QUESTION_COL, TABLE_ROW, -1, TERM_WHITE, buf);
	message_add(buf);
#ifdef JP
	Term_putstr(QUESTION_COL, TABLE_ROW + 2, -1, TERM_WHITE,
		"[ 何かキーを押してください ]");
#else
	Term_putstr(QUESTION_COL, TABLE_ROW + 2, -1, TERM_WHITE,
		"[Press any key to continue]");
#endif
	inkey();

#ifdef JP
	sprintf(buf,"%sから%sへとクラスチェンジした。", c_name + class_info[old_pclass].name, c_name + cp_ptr->name);
#else
	sprintf(buf,"changed class from %s to %s.", c_name + class_info[old_pclass].name, c_name + cp_ptr->name);
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);

	switch (p_ptr->pclass)
	{
	case CLASS_GUNNER:
		C_WIPE(p_ptr->race_sp, PY_MAX_LEVEL, s16b);
		for (i = 0; i < max_c_idx; i++) C_WIPE(p_ptr->class_sp[i], PY_MAX_LEVEL, s16b);
		p_ptr->player_gsp = 0;
		break;

	case CLASS_MEDIUM:
		choose_realm();
		break;
	}

	if (cp_ptr->c_flags & PCF_NO_DIGEST) p_ptr->food = PY_FOOD_FULL - 1;

	if (!cexp_ptr->max_clev)
	{
		cexp_ptr->max_clev = cexp_ptr->clev = 1;
		if (!cexp_ptr->max_max_clev) cexp_ptr->max_max_clev = 1;
		p_ptr->cexpfact[p_ptr->pclass] = class_info[p_ptr->pclass].c_exp + 50 * experienced_classes + total_max_clev / 5 * 10;
	}

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
	if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

	dispel_player();
	set_action(ACTION_NONE);
	init_realm_table();

	/* Update stuff */
	p_ptr->update |= (PU_HP | PU_MANA | PU_LITE);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_SPELL | PW_PLAYER);

	/* Reorder the pack (later) */
	p_ptr->notice |= (PN_REORDER);

	/* Load the "pref" files */
	load_all_pref_files();

	return;
}


/*
 * Remove an item from museum
 */
static void museum_remove_object(void)
{
	int         i;
	int         item;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];
	char        out_val[160];

	/* Empty? */
	if (st_ptr->stock_num <= 0)
	{
#ifdef JP
		msg_print("博物館には何も置いてありません。");
#else
		msg_print("Museum is empty.");
#endif

		return;
	}


	/* Find the number of objects on this and following pages */
	i = (st_ptr->stock_num - store_top);

	/* And then restrict it to the current page */
	if (i > 12) i = 12;

	/* Prompt */
#ifdef JP
	sprintf(out_val, "どのアイテムの展示をやめますか？");
#else
	sprintf(out_val, "Which item do you want to remove? ");
#endif


	/* Get the item number to be removed */
	if (!get_stock(&item, out_val, 0, i - 1)) return;

	/* Get the actual index */
	item = item + store_top;

	/* Get the actual item */
	o_ptr = &st_ptr->stock[item];

	/* Description */
	object_desc(o_name, o_ptr, 0);


#ifdef JP
	msg_print("博物館から取り除いたアイテムは二度と見ることはできません！");
	if (!get_check(format("本当に%sの展示をやめますか？", o_name))) return;
#else
	msg_print("You cannot see items which is removed from the Museum!");
	if (!get_check(format("Really remove %s from the Museum? ", o_name))) return;
#endif


	/* Message */
#ifdef JP
	msg_format("%sの展示をやめ、博物館から取り除いた。", o_name);
#else
	msg_format("You removed %s.", o_name);
#endif

	/* Remove the items from the home */
	store_item_increase(item, -o_ptr->number);
	store_item_optimize(item);

	/* The item is gone */

	/* Nothing left */
	if (st_ptr->stock_num == 0) store_top = 0;

	/* Nothing left on that screen */
	else if (store_top >= st_ptr->stock_num) store_top -= 12;

	/* Redraw everything */
	display_inventory();


	return;
}


static int stock_mon_sort_cmp(monster_type *a, monster_type *b)
{
	return (int)r_info[a->r_idx].level - (int)r_info[b->r_idx].level;
}


/*
 * Get the ID of a stocked monster
 */
static int get_stock_mon(int *com_val, int i, int j)
{
	char	command;

	char	out_val[160];

	/* Get the item index */
	if (repeat_pull(com_val))
	{
		/* Verify the item */
		if ((*com_val >= i) && (*com_val <= j))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
#ifdef JP
	(void)sprintf(out_val, "(モンスター:%c-%c, ESCで中断) どのモンスターを連れ出しますか？",
				  I2A(i), I2A(j));
#else
	(void)sprintf(out_val, "(Monsters %c-%c, ESC to exit) Which monster do you want to take? ",
				  I2A(i), I2A(j));
#endif


	/* Ask until done */
	while (TRUE)
	{
		int k;

		/* Escape */
		if (!get_com(out_val, &command, FALSE)) break;

		/* Convert */
		k = (islower(command) ? A2I(command) : -1);

		/* Legal responses */
		if ((k >= i) && (k <= j))
		{
			*com_val = k;
			break;
		}

		/* Oops */
		bell();
	}

	/* Clear the prompt */
	prt("", 0, 0);

	/* Cancel */
	if (command == ESCAPE) return (FALSE);

	repeat_push(*com_val);

	/* Success */
	return (TRUE);
}


static void display_stock_mon_entry(int pos)
{
	int          cur_col;
	monster_type *m_ptr;
	monster_race *r_ptr;

	char m_name[80];
	char out_val[160];


	int maxwid = 75;

	/* Get the monster */
	m_ptr = &stock_mon[pos];
	r_ptr = &r_info[m_ptr->r_idx];

	/* Label it, clear the line --(-- */
	(void)sprintf(out_val, "%c) ", I2A(pos));
	prt(out_val, pos+6, 0);

	cur_col = 3;
	if (show_item_graph)
	{
		byte a = r_ptr->x_attr;
		char c = r_ptr->x_char;

#ifdef AMIGA
		if (a & 0x80)
			a |= 0x40;
#endif

		Term_draw(cur_col, pos + 6, a, c);
		if (use_bigtile)
		{
			cur_col++;
			if (a & 0x80)
				Term_draw(cur_col, pos + 6, 255, -1);
		}
		cur_col += 2;
	}

	/* Describe an monster in the home */
	maxwid = 75;

	/* Describe the object */
	monster_desc(m_name, m_ptr, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
	m_name[maxwid] = '\0';
	c_put_str(TERM_WHITE, m_name, pos+6, cur_col);
}


static void take_pet_home(void)
{
	int stock_mon_num, item, i;
	int cy, cx, d, m_idx;
	monster_type *src_ptr;
	monster_race *r_ptr;
	char m_name[80];

	for (stock_mon_num = 0; stock_mon_num < MAX_STOCK_MON; stock_mon_num++)
	{
		if (!stock_mon[stock_mon_num].r_idx) break;
	}

	if (!stock_mon_num)
	{
#ifdef JP
		msg_print("預けてあるペットはいません。");
#else
		msg_print("No pet in your home.");
#endif
		return;
	}

	prt("", 5, 0);
	for (i = 0; i < 13; i++)
	{
		/* Display that line */
		if (i < stock_mon_num) display_stock_mon_entry(i);
		else prt("", i + 6, 0);
	}

	/* Get the item number to be removed */
	if (!get_stock_mon(&item, 0, stock_mon_num - 1))
	{
		/* Label the item descriptions */
#ifdef JP
		put_str("アイテムの一覧", 5, 4);
#else
		put_str("Item Description", 5, 3);
#endif
		/* If showing weights, show label */
		if (show_weights)
		{
#ifdef JP
			put_str("重さ", 5, 72);
#else
			put_str("Weight", 5, 70);
#endif

		}

		/* Draw in the inventory */
		display_inventory();
		return;
	}

	/* Get the actual monster */
	src_ptr = &stock_mon[item];
	r_ptr = &r_info[src_ptr->r_idx];

	for (d = 1; d < 6; d++)
	{
		for (i = 1000; i > 0; i--)
		{
			scatter(&cy, &cx, py, px, d, 0);
			if ((cave_floor_bold(cy, cx) || (cave[cy][cx].feat == FEAT_TREES)) && !cave[cy][cx].m_idx && !((cy == py) && (cx == px))) break;
		}
		if (i) break;
	}
	m_idx = (d < 6) ? m_pop() : 0;

	if (m_idx)
	{
		monster_type *m_ptr = &m_list[m_idx];

		cave[cy][cx].m_idx = m_idx;

		*m_ptr = *src_ptr;
		m_ptr->fy = cy;
		m_ptr->fx = cx;
		m_ptr->ml = FALSE;
		set_pet(m_ptr);

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags2 & RF2_MULTIPLY) num_repro++;

		/* Hack -- Count the number of "anti-magic monsters" */
		if (r_ptr->flags3 & RF3_ANTI_MAGIC)
			anti_magic_m_idx[num_anti_magic++] = m_idx;

		/* Hack -- Count the number of "fear field monsters" */
		if (r_ptr->flags3 & RF3_FEAR_FIELD)
			fear_field_m_idx[num_fear_field++] = m_idx;

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & RF1_ATTR_MULTI) shimmer_monsters = TRUE;

		if (item < (stock_mon_num - 1))
			stock_mon[item] = stock_mon[stock_mon_num - 1];
		(void)WIPE(&stock_mon[stock_mon_num - 1], monster_type);
		stock_mon_num--;

		if (stock_mon_num > 1)
			qsort(stock_mon, stock_mon_num, sizeof(monster_type), (int(*)(const void *, const void *))stock_mon_sort_cmp);

		monster_desc(m_name, m_ptr, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
#ifdef JP
		msg_format("%sを連れ出した。", m_name);
#else
		msg_format("Take %s.", m_name);
#endif
		if (record_named_pet && m_ptr->nickname)
		{
			do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_TAKE_HOME, m_name);
		}

		p_ptr->update |= (PU_MONSTERS | PU_MON_LITE);
	}
	else
	{
		msg_print("家からうまく連れ出せなかった。");
		real_r_ptr(src_ptr)->cur_num++;
	}

	/* Label the item descriptions */
#ifdef JP
	put_str("アイテムの一覧", 5, 4);
#else
	put_str("Item Description", 5, 3);
#endif

	/* If showing weights, show label */
	if (show_weights)
	{
#ifdef JP
		put_str("重さ", 5, 72);
#else
		put_str("Weight", 5, 70);
#endif

	}

	/* Draw in the inventory */
	display_inventory();
}


static void leave_pet_home(void)
{
	int i, stock_mon_num;
	monster_type *m_ptr, *dest_ptr;
	monster_race *r_ptr;
	char m_name[80];
	char buf[160];
	bool have_pet = FALSE;

	for (stock_mon_num = 0; stock_mon_num < MAX_STOCK_MON; stock_mon_num++)
	{
		if (!stock_mon[stock_mon_num].r_idx) break;
	}

	if (stock_mon_num == MAX_STOCK_MON)
	{
#ifdef JP
		msg_print("これ以上ペットは預けられません。");
#else
		msg_print("Full pet in your home.");
#endif
		return;
	}
	else dest_ptr = &stock_mon[stock_mon_num];

	/* Process the monsters (backwards) */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		if (is_pet(m_ptr))
		{
			r_ptr = &r_info[m_ptr->r_idx];

			if (r_ptr->flags1 & RF1_UNIQUE) continue;

			monster_desc(m_name, m_ptr, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);

			have_pet = TRUE;
#ifdef JP
			sprintf(buf, "%sを預けますか？", m_name);
#else
			sprintf(buf, "Leave %s? ", m_name);
#endif
			if (!get_check(buf)) continue;

			if (i == p_ptr->riding)
			{
#ifdef JP
				msg_format("%sから降りた。", m_name);
#else
				msg_format("You have got off %s. ", m_name);
#endif

				p_ptr->riding = 0;

				/* Update the monsters */
				p_ptr->update |= (PU_BONUS | PU_MONSTERS);
				p_ptr->redraw |= (PR_EXTRA);
			}

#ifdef JP
			msg_format("%sを預けた。", m_name);
#else
			msg_format("Leaved %s.", m_name);
#endif
			if (record_named_pet && m_ptr->nickname)
			{
				do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_PUT_HOME, m_name);
			}

			monster_drop_carried_objects(m_ptr);

			COPY(dest_ptr, m_ptr, monster_type);

			dest_ptr->fy = 0;
			dest_ptr->fx = 0;
			dest_ptr->hp = dest_ptr->maxhp = dest_ptr->max_maxhp;
			(void)set_monster_csleep(i, 0);
			(void)set_monster_fast(i, 0);
			(void)set_monster_slow(i, 0);
			(void)set_monster_stunned(i, 0);
			(void)set_monster_confused(i, 0);
			(void)set_monster_monfear(i, 0);
			(void)set_monster_stoning(i, 0);
			(void)set_monster_melt_weapon(i, 0);
			(void)set_monster_opposite_elem(i, 0);
			(void)set_monster_silent(i, 0);
			(void)set_monster_invulner(i, 0, FALSE);
			dest_ptr->silent_song = FALSE;
			dest_ptr->cdis = 0;
			dest_ptr->ddis = 0;
			dest_ptr->ml = FALSE;
			dest_ptr->target_y = 0;
			dest_ptr->target_x = 0;

			stock_mon_num++;
			qsort(stock_mon, stock_mon_num, sizeof(monster_type), (int(*)(const void *, const void *))stock_mon_sort_cmp);

			delete_monster_idx(i);
			real_r_ptr(dest_ptr)->cur_num++;

			return;
		}
	}

	if (!have_pet)
	{
#ifdef JP
		msg_print("預けられるペットがいない。");
#else
		msg_print("You can leave no pets.");
#endif
	}
}


/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands
 * in the stores which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the stores, to prevent chaos.
 */
static void store_process_command(void)
{

	/* Handle repeating the last command */
	repeat_check();

	if (rogue_like_commands && command_cmd == 'l')
	{
		command_cmd = 'x';	/* hack! */
	}

	/* Parse the command */
	switch (command_cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			leave_store = TRUE;
			break;
		}

		/* 日本語版追加 */
		/* 1 ページ戻るコマンド: 我が家のページ数が多いので重宝するはず By BUG */
		case '-':
		{
			if (st_ptr->stock_num <= 12) {
#ifdef JP
				msg_print("これで全部です。");
#else
				msg_print("Entire inventory is shown.");
#endif
			}
			else{
				store_top -= 12;
				if ( store_top < 0 )
					store_top = ((st_ptr->stock_num - 1 )/12) * 12;
				if ( (cur_store_num == STORE_HOME) && (powerup_home == FALSE) )
					if ( store_top >= 12 ) store_top = 12;
				display_inventory();
			}
			break;
		}

		/* Browse */
		case ' ':
		{
			if (st_ptr->stock_num <= 12)
			{
#ifdef JP
				msg_print("これで全部です。");
#else
				msg_print("Entire inventory is shown.");
#endif

			}
			else
			{
				store_top += 12;
				/*
				 * 隠しオプション(powerup_home)がセットされていないときは
				 * 我が家では 2 ページまでしか表示しない
				 */
				if ((cur_store_num == STORE_HOME) &&
				    (powerup_home == FALSE) &&
				    (st_ptr->stock_num >= STORE_INVEN_MAX))
				{
					if (store_top >= (STORE_INVEN_MAX - 1))
					{
						store_top = 0;
					}
				}
				else
				{
					if (store_top >= st_ptr->stock_num) store_top = 0;
				}

				display_inventory();
			}
			break;
		}

		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_store();
			break;
		}

		/* Get (purchase) */
		case 'g':
		{
			store_purchase();
			break;
		}

		/* Drop (Sell) */
		case 'd':
		{
			store_sell();
			break;
		}

		/* Examine */
		case 'x':
		{
			store_examine();
			break;
		}

		/* Ignore return */
		case '\r':
		{
			break;
		}

		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('I'):
		{
			toggle_inven_equip();
			break;
		}



		/*** Use various objects ***/

		/* Browse a book */
		case 'b':
		{
			if (p_ptr->pclass == CLASS_GUNNER) do_cmd_gunner(TRUE);
			else do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			p_ptr->town_num = old_town_num;
			do_cmd_change_name();
			old_town_num = p_ptr->town_num;
			p_ptr->town_num = cur_town_num;
			display_store();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			do_cmd_redraw();
			display_store();
			break;
		}

		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			p_ptr->town_num = old_town_num;
			do_cmd_feeling();
			old_town_num = p_ptr->town_num;
			p_ptr->town_num = cur_town_num;
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages(0);
			break;
		}

		case '|':
		{
			do_cmd_nikki();
			break;
		}

		/* Check artifacts, uniques etc. */
		case '~':
		{
			p_ptr->town_num = old_town_num;
			do_cmd_knowledge();
			old_town_num = p_ptr->town_num;
			p_ptr->town_num = cur_town_num;
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			/* Giga-Hack: Class change */
			if ((cur_store_num == STORE_HOME) && (command_cmd == 'c'))
			{
				change_player_class();
				display_store();
			}
			else if ((cur_store_num == STORE_HOME) && (command_cmd == 'G'))
			{
				take_pet_home();
			}
			else if ((cur_store_num == STORE_HOME) && (command_cmd == 'D'))
			{
				leave_pet_home();
			}
			else if ((cur_store_num == STORE_MUSEUM) && (command_cmd == 'r'))
			{
				museum_remove_object();
			}
			else
			{
#ifdef JP
				msg_print("そのコマンドは現在は使えません。");
#else
				msg_print("That command does not work now.");
#endif
			}

			break;
		}
	}
}


/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "command_arg" and all
 * command macros and other nifty stuff, but we use the special
 * "shopping" argument, to force certain commands to be converted
 * into other commands, normally, we convert "p" (pray) and "m"
 * (cast magic) into "g" (get), and "s" (search) into "d" (drop).
 */
void do_cmd_store(void)
{
	int         which;
	int         maintain_num;
	int         tmp_chr;
	int         i;
	cave_type   *c_ptr;


	/* Access the player grid */
	c_ptr = &cave[py][px];

	/* Verify a store */
	if (!((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		  (c_ptr->feat <= FEAT_SHOP_TAIL)) &&
	    (c_ptr->feat != FEAT_MUSEUM) &&
		(c_ptr->feat != FEAT_DENEB_SHOP))
	{
#ifdef JP
		msg_print("ここには店がありません。");
#else
		msg_print("You see no store here.");
#endif

		return;
	}

	/* Extract the store code */
	if (c_ptr->feat == FEAT_MUSEUM) which = STORE_MUSEUM;
	else if (c_ptr->feat == FEAT_DENEB_SHOP) which = STORE_BLACK;
	else which = (c_ptr->feat - FEAT_SHOP_HEAD);

	/* Restriction by chaos frame */
	if (!dun_level && p_ptr->town_num && (which != STORE_BLACK))
	{
		int ethnic = town[p_ptr->town_num].ethnic;
		if (ethnic != NO_ETHNICITY)
		{
			/* Special Restriction for Barmamutha Genocider! */
			if (p_ptr->town_num == TOWN_BARMAMUTHA)
			{
				if (misc_event_flags & EVENT_CLOSE_BARMAMUTHA)
				{
					if (which != STORE_HOME)
					{
#ifdef JP
						msg_print("ドアに鍵がかかっている。");
#else
						msg_print("The doors are locked.");
#endif
						return;
					}
					else
					{
#ifdef JP
						msg_print("ドアはがっちりと閉じられているようだ。");
#else
						msg_print("The door appears to be stuck.");
#endif
						return;
					}
				}
			}

			if ((p_ptr->town_num == TOWN_ARMORICA) && !astral_mode)
			{
				if (!(misc_event_flags & EVENT_LIBERATION_OF_ARMORICA))
				{
					if (which != STORE_HOME)
					{
#ifdef JP
						msg_print("ドアに鍵がかかっている。");
#else
						msg_print("The doors are locked.");
#endif
						return;
					}
					else
					{
#ifdef JP
						msg_print("ドアはがっちりと閉じられているようだ。");
#else
						msg_print("The door appears to be stuck.");
#endif
						return;
					}
				}
			}
			if ((which != STORE_HOME) && (chaos_frame[ethnic] <= CFRAME_CLOSE_BLDG))
			{
#ifdef JP
				msg_print("ドアに鍵がかかっている。");
#else
				msg_print("The doors are locked.");
#endif
				return;
			}
			else if (chaos_frame[ethnic] <= CFRAME_LOWER_LIMIT)
			{
#ifdef JP
				msg_print("ドアはがっちりと閉じられているようだ。");
#else
				msg_print("The door appears to be stuck.");
#endif
				return;
			}
		}
	}

	old_town_num = p_ptr->town_num;
	if ((which == STORE_HOME) || (which == STORE_MUSEUM)) p_ptr->town_num = 1;
	if (dun_level) p_ptr->town_num = NO_TOWN;
	cur_town_num = p_ptr->town_num;

	/* Hack -- Check the "locked doors" */
	if (town[p_ptr->town_num].store[which].store_open >= turn)
	{
#ifdef JP
		msg_print("ドアに鍵がかかっている。");
#else
		msg_print("The doors are locked.");
#endif

		p_ptr->town_num = old_town_num;
		return;
	}

	/* Calculate the number of store maintainances since the last visit */
	maintain_num = (turn - town[p_ptr->town_num].store[which].last_visit) / (TURNS_PER_TICK * STORE_TURNS);

	/* Maintain the store max. 10 times */
	if (maintain_num > 10) maintain_num = 10;

	if (maintain_num)
	{
		/* Maintain the store */
		for (i = 0; i < maintain_num; i++)
			store_maint(p_ptr->town_num, which);

		/* Save the visit */
		town[p_ptr->town_num].store[which].last_visit = turn;
	}

	/* Forget the lite */
	forget_lite();

	/* Forget the view */
	forget_view();


	/* Hack -- Character is in "icky" mode */
	character_icky = TRUE;


	/* No command argument */
	command_arg = 0;

	/* No repeated command */
	command_rep = 0;

	/* No automatic command */
	command_new = 0;


	/* Save the store number */
	cur_store_num = which;

	/* Save the store and owner pointers */
	st_ptr = &town[p_ptr->town_num].store[cur_store_num];
	ot_ptr = &owners[cur_store_num][st_ptr->owner];


	/* Start at the beginning */
	store_top = 0;

	/* Display the store */
	display_store();

	/* Do not leave */
	leave_store = FALSE;

	/* Interact with player */
	while (!leave_store)
	{
		/* Hack -- Clear line 1 */
		prt("", 1, 0);

		/* Hack -- Check the charisma */
		tmp_chr = p_ptr->stat_use[A_CHR];

		/* Clear */
		clear_from(20);


		/* Basic commands */
#ifdef JP
		prt(" ESC) 建物から出る", 21, 0);
#else
		prt(" ESC) Exit from Building.", 21, 0);
#endif


		/* Browse if necessary */
		if (st_ptr->stock_num > 12)
		{
#ifdef JP
			prt(" -)前ページ", 22, 0);
			prt(" スペース) 次ページ", 23, 0);
#else
			prt(" -) Previous page", 22, 0);
			prt(" SPACE) Next page", 23, 0);
#endif

		}

		/* Home commands */
		if (cur_store_num == STORE_HOME)
		{
#ifdef JP
			prt("D) ペットを預ける", 20, 1);
			prt("G) ペットを連れ出す", 20, 27);
			prt("g) アイテムを取る", 21, 27);
			prt("d) アイテムを置く", 22, 27);
			prt("x) 家のアイテムを調べる", 23,27);
			prt(" c) クラスチェンジ", 23, 57);
#else
			prt("D) Leave a pet.", 20, 0);
			prt("G) Take a pet.", 20, 27);
			prt("g) Get an item.", 21, 27);
			prt("d) Drop an item.", 22, 27);
			prt("x) eXamine an item in the home.", 23,27);
			prt(" c) Change your class.", 23, 57);
#endif

		}

		/* Museum commands */
		else if (cur_store_num == STORE_MUSEUM)
		{
#ifdef JP
			prt("d) アイテムを置く", 21, 27);
			prt("r) アイテムの展示をやめる", 22, 27);
			prt("x) 博物館のアイテムを調べる", 23,27);
#else
			prt("d) Drop an item.", 21, 27);
			prt("r) Remove an item from the museum.", 22,27);
			prt("x) eXamine an item in the museum.", 23,27);
#endif

		}

		/* Shop commands XXX XXX XXX */
		else
		{
#ifdef JP
			prt("p) 商品を買う", 21, 30);
			prt("s) アイテムを売る", 22, 30);
			prt("x) 商品を調べる", 23,30);
#else
			prt("p) Purchase an item.", 21, 30);
			prt("s) Sell an item.", 22, 30);
			prt("x) eXamine an item in the shop", 23,30);
#endif

		}

#ifdef JP
		/* 基本的なコマンドの追加表示 */

		prt("i/e) 持ち物/装備の一覧", 21, 56);

		if (rogue_like_commands == TRUE)
		{
			prt("w/T) 装備する/はずす", 22, 56);
		}
		else
		{
			prt("w/t) 装備する/はずす", 22, 56);
		}
#else
		prt("i/e) Inventry/Equipment list", 21, 56);

		if (rogue_like_commands == TRUE)
		{
			prt("w/T) Wear/Take off equipment", 22, 56);
		}
		else
		{
			prt("w/t) Wear/Take off equipment", 22, 56);
		}
#endif
		/* Prompt */
#ifdef JP
		prt("コマンド:", 20, 53);
#else
		prt("You may: ", 20, 53);
#endif


		/* Get a command */
		request_command(TRUE);

		/* Process the command */
		store_process_command();

		/* Hack -- Character is still in "icky" mode */
		character_icky = TRUE;

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* XXX XXX XXX Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			object_type *o_ptr = &inventory[item];

			/* Hack -- Flee from the store */
			if (cur_store_num != STORE_HOME)
			{
				/* Message */
#ifdef JP
				if (cur_store_num == STORE_MUSEUM)
					msg_print("ザックからアイテムがあふれそうなので、あわてて博物館から出た...");
				else
					msg_print("ザックからアイテムがあふれそうなので、あわてて店から出た...");
#else
				if (cur_store_num == STORE_MUSEUM)
					msg_print("Your pack is so full that you flee the Museum...");
				else
					msg_print("Your pack is so full that you flee the store...");
#endif


				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Flee from the home */
			else if (!store_check_num(o_ptr))
			{
				/* Message */
#ifdef JP
				msg_print("ザックからアイテムがあふれそうなので、あわてて家から出た...");
#else
				msg_print("Your pack is so full that you flee your home...");
#endif


				/* Leave */
				leave_store = TRUE;
			}

			/* Hack -- Drop items into the home */
			else
			{
				int item_pos;

				object_type forge;
				object_type *q_ptr;

				char o_name[MAX_NLEN];


				/* Give a message */
#ifdef JP
				msg_print("ザックからアイテムがあふれてしまった！");
#else
				msg_print("Your pack overflows!");
#endif


				/* Get local object */
				q_ptr = &forge;

				/* Grab a copy of the item */
				object_copy(q_ptr, o_ptr);

				/* Describe it */
				object_desc(o_name, q_ptr, 0);

				/* Message */
#ifdef JP
				msg_format("%sが落ちた。(%c)", o_name, index_to_label(item));
#else
				msg_format("You drop %s (%c).", o_name, index_to_label(item));
#endif


				/* Remove it from the players inventory */
				inven_item_increase(item, -255);
				inven_item_describe(item);
				inven_item_optimize(item);

				/* Handle stuff */
				handle_stuff();

				/* Let the home carry it */
				item_pos = home_carry(q_ptr);

				/* Redraw the home */
				if (item_pos >= 0)
				{
					store_top = (item_pos / 12) * 12;
					display_inventory();
				}
			}
		}

		/* Hack -- Redisplay store prices if charisma changes */
		if (tmp_chr != p_ptr->stat_use[A_CHR]) display_inventory();

		/* Hack -- get kicked out of the store */
		if (st_ptr->store_open >= turn) leave_store = TRUE;
	}

	p_ptr->town_num = old_town_num;

	/* Free turn XXX XXX XXX */
	energy_use = 100;


	/* Hack -- Character is no longer in "icky" mode */
	character_icky = FALSE;


	/* Hack -- Cancel automatic command */
	command_new = 0;

	/* Hack -- Cancel "see" mode */
	command_see = FALSE;


	/* Flush messages XXX XXX XXX */
	msg_print(NULL);


	/* Clear the screen */
	Term_clear();


	/* Update everything */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}



/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int i, j;


	/* Ignore home */
	if (which == STORE_HOME) return;
	if (which == STORE_MUSEUM) return;


	/* Save the store index */
	cur_store_num = which;

	/* Activate that store */
	st_ptr = &town[p_ptr->town_num].store[cur_store_num];

	j = st_ptr->owner;
	/* Pick a new owner */
	while(1)
	{
		st_ptr->owner = (byte)randint0(MAX_OWNERS);
		if (j == st_ptr->owner) continue;
		for (i = 1;i < max_towns; i++)
		{
			if (i == p_ptr->town_num) continue;
			if (st_ptr->owner == town[i].store[cur_store_num].owner) break;
		}
		if (i == max_towns) break;
	}

	/* Activate the new owner */
	ot_ptr = &owners[cur_store_num][st_ptr->owner];


	/* Reset the owner data */
	st_ptr->insult_cur = 0;
	st_ptr->store_open = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;


	/* Hack -- discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the item */
		o_ptr = &st_ptr->stock[i];

		/* Hack -- Sell all old items for "half price" */
		if (!(o_ptr->art_name))
			o_ptr->discount = 50;

		/* Hack -- Items are no longer "fixed price" */
		o_ptr->ident &= ~(IDENT_FIXED);

		/* Mega-Hack -- Note that the item is "on sale" */
#ifdef JP
		o_ptr->inscription = quark_add("売出中");
#else
		o_ptr->inscription = quark_add("on sale");
#endif

	}
}


/*
 * Maintain the inventory at the stores.
 */
void store_maint(int town_num, int store_num)
{
	int 		j;

	cur_store_num = store_num;

	/* Ignore home */
	if (store_num == STORE_HOME) return;
	if (store_num == STORE_MUSEUM) return;

	/* Activate that store */
	st_ptr = &town[town_num].store[store_num];

	/* Activate the owner */
	ot_ptr = &owners[store_num][st_ptr->owner];

	/* Store keeper forgives the player */
	st_ptr->insult_cur = 0;

	/* Mega-Hack -- prune the black market */
	if (store_num == STORE_BLACK)
	{
		/* Destroy crappy black market items */
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type *o_ptr = &st_ptr->stock[j];

			/* Destroy crappy items */
			if (black_market_crap(o_ptr))
			{
				/* Destroy the item */
				store_item_increase(j, 0 - o_ptr->number);
				store_item_optimize(j);
			}
		}
	}


	/* Choose the number of slots to keep */
	j = st_ptr->stock_num;

	/* Sell a few items */
	j = j - randint1(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "underflow" */
	if (j < 0) j = 0;

	/* Destroy objects until only "j" slots are left */
	while (st_ptr->stock_num > j) store_delete();


	/* Choose the number of slots to fill */
	j = st_ptr->stock_num;

	/* Buy some more items */
	j = j + randint1(STORE_TURNOVER);

	/* Never keep more than "STORE_MAX_KEEP" slots */
	if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

	/* Always "keep" at least "STORE_MIN_KEEP" items */
	if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

	/* Hack -- prevent "overflow" */
	if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

	/* Acquire some new items */
	while (st_ptr->stock_num < j) store_create();
}


/*
 * Initialize the stores
 */
void store_init(int town_num, int store_num)
{
	int 		k;

	cur_store_num = store_num;

	/* Activate that store */
	st_ptr = &town[town_num].store[store_num];


	/* Pick an owner */
	while(1)
	{
		int i;

		st_ptr->owner = (byte)randint0(MAX_OWNERS);
		for (i = 1;i < max_towns; i++)
		{
			if (i == town_num) continue;
			if (st_ptr->owner == town[i].store[store_num].owner) break;
		}
		if (i == max_towns) break;
	}

	/* Activate the new owner */
	ot_ptr = &owners[store_num][st_ptr->owner];


	/* Initialize the store */
	st_ptr->store_open = 0;
	st_ptr->insult_cur = 0;
	st_ptr->good_buy = 0;
	st_ptr->bad_buy = 0;

	/* Nothing in stock */
	st_ptr->stock_num = 0;

	/*
	 * MEGA-HACK - Last visit to store is
	 * BEFORE player birth to enable store restocking
	 */
	st_ptr->last_visit = -200L * STORE_TURNS;

	/* Clear any old items */
	for (k = 0; k < st_ptr->stock_size; k++)
	{
		object_wipe(&st_ptr->stock[k]);
	}
}


void move_to_black_market(object_type *o_ptr)
{
	/* Not in town */
	if (!p_ptr->town_num) return;

	st_ptr = &town[p_ptr->town_num].store[STORE_BLACK];

	o_ptr->ident |= IDENT_STORE;

	(void)store_carry(o_ptr);

	object_wipe(o_ptr); /* Don't leave a bogus object behind... */
}


void move_inventory_to_home(void)
{
	int i;
	object_type *o_ptr;
	bool old_powerup_home = powerup_home;

	powerup_home = TRUE;

	/* Save the store number */
	cur_store_num = STORE_HOME;

	/* Save the store and owner pointers */
	st_ptr = &town[1].store[cur_store_num];

	for (i = st_ptr->stock_size - 1; i >= 0; i--)
	{
		/* Get the item */
		o_ptr = &st_ptr->stock[i];

		if (!o_ptr->k_idx) continue;

		if (object_is_runeweapon(o_ptr) && o_ptr->art_name && (o_ptr->xtra3 > 0))
		{
			/* Remove the items from the home */
			store_item_increase(i, -255);
			store_item_optimize(i);
		}
	}

	powerup_home = old_powerup_home;

	if (!ancestor_inven_cnt || !ancestor_inventory) return;

	for (i = 0; i < ancestor_inven_cnt; i++)
	{
		o_ptr = &ancestor_inventory[i];

		if (store_check_num(o_ptr))
		{
			/* Let the home carry it */
			home_carry(o_ptr);
			object_wipe(o_ptr);
		}
	}
}
